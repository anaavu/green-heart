import os
import pandas as pd

# User inputs
models_directory = r"I:\Backups\GIS\Local_Computer_Stuff\GIS\models\no_interactions_ses_unadj_inc_homerent"
output_location = r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - General\GIS\GreenHeart\DataOutputsandDeliverables\SES Paper\July2021"

# This is what the output file will be called (folder name of models)
resultfilename = models_directory.rsplit("\\", 2)[2]
print(resultfilename)

# Grabs list of files in models directory
files = [f for f in os.listdir(models_directory) if not f.endswith("_selected.csv") and
         not f.endswith("confint.csv") and not f.endswith("standardz.csv")]
#files = [f for f in os.listdir(models_directory) if f.endswith("_standardz.csv")]

# Create greenness column
modelsdf = pd.DataFrame(columns=["term", "greenness"])

# Grab all of the terms from the first csv file
terms = pd.read_csv(os.path.join(models_directory, files[0]))
print(terms)

# Create column in output dataframe with all of the terms
modelsdf['term'] = terms['Unnamed: 0']


# Create columns with filenames
for file in files:
    read = pd.read_csv(os.path.join(models_directory, file)) # Read each file
    filetitle = file.split("_full")[0]
    filedf = pd.DataFrame(columns=[filetitle]) # Create dataframe with the name of the file as the title
    for row in range(len(read)):
        coefficient = read['estimate'][row] # Pull coeff of greenness metric from model
        pval = read['p.value'][row] # Pull p value of greenness metric from model
        if pval < 0.05: # If True, add coefficient to output dataframe
            filedf.loc[row] = coefficient
        else: # If false, add "-" in place to csv to output dataframe
            filedf.loc[row] = "-"
    modelsdf = modelsdf.join(pd.DataFrame(filedf)) # Join each file dataframe to the output dataframe
print(modelsdf.columns)
modelsdf = modelsdf[['term', "morevisible_ndvi_MEAN", "lessvisible_ndvi_MEAN", "Nobuilding_ndvi_Mean", "Parcel_ndvi_Mean",
                     'Participant_20m_Mean_ndvi', 'Participant_50m_Mean_ndvi', 'Participant_100m_Mean_ndvi',
                         'Participant_150m_Mean_ndvi', 'Participant_200m_Mean_ndvi',
                         'Participant_300m_Mean_ndvi', 'Participant_500m_Mean_ndvi', "Walktime5_ndvi_Mean",
                        "morevisible_NDVI_L19_MEAN", "lessvisible_NDVI_L19_MEAN", "Nobuilding_NDVI_L19_Mean",
                     "Parcel_NDVI_L19_Mean", "Participant_20m_Mean_NDVI_L19",
                     "Participant_50m_Mean_NDVI_L19", "Participant_100m_Mean_NDVI_L19",
                     "Participant_150m_Mean_NDVI_L19", "Participant_200m_Mean_NDVI_L19",
                     "Participant_300m_Mean_NDVI_L19", "Participant_500m_Mean_NDVI_L19",
                    "morevisible_canopy_MEAN", "lessvisible_canopy_MEAN", "Nobuilding_canopy_Mean",
                    "Parcel_canopy_Mean",
                         'Participant_20m_Mean_canopy', 'Participant_50m_Mean_canopy', 'Participant_100m_Mean_canopy',
                         'Participant_150m_Mean_canopy', 'Participant_200m_Mean_canopy',
                         'Participant_300m_Mean_canopy', 'Participant_500m_Mean_canopy', "Walktime5_canopy_Mean",
                     "morevisible_canopy2_MEAN", "lessvisible_canopy2_MEAN", "Nobuilding_canopy2_Mean",
                     "Parcel_canopy2_Mean",
                     'Participant_20m_Mean_canopy2', 'Participant_50m_Mean_canopy2', 'Participant_100m_Mean_canopy2',
                     'Participant_150m_Mean_canopy2', 'Participant_200m_Mean_canopy2',
                     'Participant_300m_Mean_canopy2', 'Participant_500m_Mean_canopy2', "Walktime5_canopy2_Mean"
                     ]]
# Output
modelsdf.to_csv("{}\{}.csv".format(output_location, resultfilename))
