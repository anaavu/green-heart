# ---------------------------------------------------------------------
# Author: Anagha
# Date: July 14, 2021
# Purpose: To make condensed data tables from many R model outputs
# Project: Hypertension-Greenness (Green Heart)
# Inputs: Folder containing model outputs, and file output directory
# Outputs: CSV file with rows of greenness metric and 1 column of effect size
# Notes: Coefficient included if p < 0.05, otherwise blank
# ---------------------------------------------------------------------
import os
import pandas

# User inputs
models_directory = r"I:\Backups\GIS\Local_Computer_Stuff\GIS\models\mh_ViewsonLife_model6"
output_location = r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - General\GIS\GreenHeart\DataOutputsandDeliverables\Mental Health paper\tables_Nov2021"


# This is what the output file will be called (folder name of models)
resultfilename = models_directory.rsplit("\\", 2)[2]
print(resultfilename)

# Grabs list of files in models directory
files = [f for f in os.listdir(models_directory) if not f.endswith("_selected.csv") and not f.endswith("confint.csv")]
#files = [f for f in os.listdir(models_directory) if f.endswith("_standardz.csv")]

# Create pandas df
modelsdf = pandas.DataFrame(columns=["greenness", "coefficient"])

for each in files:
    print(each)
    read = pandas.read_csv(os.path.join(models_directory, each)) #read in file
    #print(read)
    greencol = read['term'][1] # pull greenness metric name from model
    coefficient = read['estimate'][1] # pull coeff of greenness metric from model
    pval = read['p.value'][1] # pull p value of greenness metric from model
    if pval < 0.05: # if True, add coefficient
        modelsdf = modelsdf.append(pandas.DataFrame([[greencol, coefficient]], columns=modelsdf.columns))
    else: # If false, add "-" in place
        modelsdf = modelsdf.append(pandas.DataFrame([[greencol, "-"]], columns=modelsdf.columns))
print(modelsdf['greenness'])
modelsdf_T = modelsdf.T
modelsdf_T = modelsdf_T
modelsdf_T.columns = modelsdf_T.iloc[0]
modelsdf_T = modelsdf_T.drop(["greenness"])

# modelsdf_T = modelsdf_T.reset_index(drop=True)
print(modelsdf_T)
#modelsdf_T = modelsdf_T.set_index('greenness')
# Order of the rows of greenness
# modelsdf = modelsdf[['greenness', "morevisible_ndvi_MEAN", "lessvisible_ndvi_MEAN", "Nobuilding_ndvi_Mean", "Parcel_ndvi_Mean",
#                      'Participant_20m_Mean_ndvi', 'Participant_50m_Mean_ndvi', 'Participant_100m_Mean_ndvi',
#                          'Participant_150m_Mean_ndvi', 'Participant_200m_Mean_ndvi',
#                          'Participant_300m_Mean_ndvi', 'Participant_500m_Mean_ndvi', "Walktime5_ndvi_Mean",
#                         "morevisible_NDVI_L19_MEAN", "lessvisible_NDVI_L19_MEAN", "Nobuilding_NDVI_L19_Mean",
#                      "Parcel_NDVI_L19_Mean", "Participant_20m_Mean_NDVI_L19",
#                      "Participant_50m_Mean_NDVI_L19", "Participant_100m_Mean_NDVI_L19",
#                      "Participant_150m_Mean_NDVI_L19", "Participant_200m_Mean_NDVI_L19",
#                      "Participant_300m_Mean_NDVI_L19", "Participant_500m_Mean_NDVI_L19",
#                     "morevisible_canopy_MEAN", "lessvisible_canopy_MEAN", "Nobuilding_canopy_Mean",
#                     "Parcel_canopy_Mean",
#                          'Participant_20m_Mean_canopy', 'Participant_50m_Mean_canopy', 'Participant_100m_Mean_canopy',
#                          'Participant_150m_Mean_canopy', 'Participant_200m_Mean_canopy',
#                          'Participant_300m_Mean_canopy', 'Participant_500m_Mean_canopy', "Walktime5_canopy_Mean",
#                      "morevisible_canopy2_MEAN", "lessvisible_canopy2_MEAN", "Nobuilding_canopy2_Mean",
#                      "Parcel_canopy2_Mean",
#                      'Participant_20m_Mean_canopy2', 'Participant_50m_Mean_canopy2', 'Participant_100m_Mean_canopy2',
#                      'Participant_150m_Mean_canopy2', 'Participant_200m_Mean_canopy2',
#                      'Participant_300m_Mean_canopy2', 'Participant_500m_Mean_canopy2', "Walktime5_canopy2_Mean"
#                      ]]

# modelsdf_T = modelsdf_T[["morevisible_ndvi_MEAN", "lessvisible_ndvi_MEAN", "Nobuilding_ndvi_Mean", "Parcel_ndvi_Mean",
#                      'Participant_20m_Mean_ndvi', 'Participant_50m_Mean_ndvi', 'Participant_100m_Mean_ndvi',
#                          'Participant_150m_Mean_ndvi', 'Participant_200m_Mean_ndvi',
#                          'Participant_300m_Mean_ndvi', 'Participant_500m_Mean_ndvi', "Walktime5_ndvi_Mean",
#                         "morevisible_NDVI_L19_MEAN", "lessvisible_NDVI_L19_MEAN", "Nobuilding_NDVI_L19_Mean",
#                      "Parcel_NDVI_L19_Mean", "Participant_20m_Mean_NDVI_L19",
#                      "Participant_50m_Mean_NDVI_L19", "Participant_100m_Mean_NDVI_L19",
#                      "Participant_150m_Mean_NDVI_L19", "Participant_200m_Mean_NDVI_L19",
#                      "Participant_300m_Mean_NDVI_L19", "Participant_500m_Mean_NDVI_L19",
#                     "morevisible_canopy_MEAN", "lessvisible_canopy_MEAN", "Nobuilding_canopy_Mean",
#                     "Parcel_canopy_Mean",
#                          'Participant_20m_Mean_canopy', 'Participant_50m_Mean_canopy', 'Participant_100m_Mean_canopy',
#                          'Participant_150m_Mean_canopy', 'Participant_200m_Mean_canopy',
#                          'Participant_300m_Mean_canopy', 'Participant_500m_Mean_canopy', "Walktime5_canopy_Mean",
#                      "morevisible_canopy2_MEAN", "lessvisible_canopy2_MEAN", "Nobuilding_canopy2_Mean",
#                      "Parcel_canopy2_Mean",
#                      'Participant_20m_Mean_canopy2', 'Participant_50m_Mean_canopy2', 'Participant_100m_Mean_canopy2',
#                      'Participant_150m_Mean_canopy2', 'Participant_200m_Mean_canopy2',
#                      'Participant_300m_Mean_canopy2', 'Participant_500m_Mean_canopy2', "Walktime5_canopy2_Mean"
#                      ]]

modelsdf_T = modelsdf_T[["morevisible_ndvi_MEAN", "lessvisible_ndvi_MEAN", "Nobuilding_ndvi_Mean", "Parcel_ndvi_Mean",
                     'Participant_20m_Mean_ndvi', 'Participant_50m_Mean_ndvi', 'Participant_100m_Mean_ndvi',
                         'Participant_150m_Mean_ndvi', 'Participant_200m_Mean_ndvi',
                         'Participant_300m_Mean_ndvi', 'Participant_500m_Mean_ndvi',
                         "morevisible_canopy_MEAN", "lessvisible_canopy_MEAN", "Nobuilding_canopy_Mean",
                    "Parcel_canopy_Mean",
                         'Participant_20m_Mean_canopy', 'Participant_50m_Mean_canopy', 'Participant_100m_Mean_canopy',
                         'Participant_150m_Mean_canopy', 'Participant_200m_Mean_canopy',
                         'Participant_300m_Mean_canopy', 'Participant_500m_Mean_canopy',
                        "morevisible_NDVI_L19_MEAN", "lessvisible_NDVI_L19_MEAN", "Nobuilding_NDVI_L19_Mean",
                     "Parcel_NDVI_L19_Mean", "Participant_20m_Mean_NDVI_L19",
                     "Participant_50m_Mean_NDVI_L19", "Participant_100m_Mean_NDVI_L19",
                     "Participant_150m_Mean_NDVI_L19", "Participant_200m_Mean_NDVI_L19",
                     "Participant_300m_Mean_NDVI_L19", "Participant_500m_Mean_NDVI_L19",
                     ]]

# Output
modelsdf_T.to_csv("{}\{}.csv".format(output_location, resultfilename))