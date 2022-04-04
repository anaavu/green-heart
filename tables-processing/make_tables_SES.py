# ---------------------------------------------------------------------
# Author: Anagha
# Date: July 14, 2021
# Purpose: To make condensed data tables from many R model outputs
# Project: SES-Greenness (Green Heart)
# Inputs: Folder containing model outputs, and file output directory
# Outputs: CSV file with rows of greenness metric and 1 column of effect size
# Notes: Coefficient included if p < 0.05, otherwise blank
# ---------------------------------------------------------------------
import os
import pandas

# User inputs
models_directory = r"I:\Backups\GIS\Local_Computer_Stuff\GIS\models\no_interactions_ses_adj_full_all"
output_location = r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - General\GIS\GreenHeart\DataOutputsandDeliverables\SES Paper\July2021"

# This is what the output file will be called (folder name of models)
resultfilename = models_directory.rsplit("\\", 2)[2]
print(resultfilename)

# Grabs list of files in models directory
files = [f for f in os.listdir(models_directory) if not f.endswith("_selected.csv") and
         not f.endswith("confint.csv") and not f.endswith("standardz.csv")]

# Create pandas df
modelsdf = pandas.DataFrame(columns=["greenness", "term", "coefficient"])

for each in files:
    print(each)
    read = pandas.read_csv(os.path.join(models_directory, each)) #read in file
    print(read)
    for i in range(len(read)):
        if i <= 1:
            pass
        term = read['term'][i] # pull metric name from unadj model
        coefficient = read['estimate'][i] # pull coeff of greenness metric from model
        pval = read['p.value'][i] # pull p value of greenness metric from model
        if pval < 0.05: # if True, add coefficient
            modelsdf = modelsdf.append(pandas.DataFrame([[each, term, coefficient]], columns=modelsdf.columns))
        else: # If false, add "-" in place
            modelsdf = modelsdf.append(pandas.DataFrame([[each, term, "-"]], columns=modelsdf.columns))

#modelsdf = modelsdf.set_index('term')
#modelsdf = modelsdf.groupby('greenness').coefficient.agg(list).apply(pandas.Series).T
#modelsdf.columns.name=None
# # Order of the rows of greenness
# modelsdf = modelsdf.loc[['Participant_20m_Mean_ndvi', 'Participant_50m_Mean_ndvi', 'Participant_100m_Mean_ndvi',
#                          'Participant_150m_Mean_ndvi', 'Participant_200m_Mean_ndvi',
#                          'Participant_300m_Mean_ndvi', 'Participant_500m_Mean_ndvi',
#                          "Parcel_ndvi_Mean"], :]
# Output
modelsdf.to_csv("{}\{}0.csv".format(output_location, resultfilename))