import os
import pandas as pd

# User inputs
models_directory = r"E:\GIS\Local_Computer_Stuff\GIS\models\no_interactions_ses_unadj_propval_svihigh"
output_location = r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - General\GIS\GreenHeart\DataOutputsandDeliverables\SES Paper\July2021"

# This is what the output file will be called (folder name of models)
resultfilename = models_directory.rsplit("\\", 2)[2]
print(resultfilename)

# Grabs list of files in models directory
files = [f for f in os.listdir(models_directory) if not f.endswith("_selected.csv") and not f.endswith("confint.csv") and not f.endswith("_standardz.csv")]

# Create greenness column
modelsdf = pd.DataFrame(columns=["term", "greenness"])

# Grab all of the terms from the first csv file
terms = pd.read_csv(os.path.join(models_directory, files[0]))
# Create column in output dataframe with all of the terms
modelsdf['term'] = terms['term']


# Create columns with filenames
for file in files:
    read = pd.read_csv(os.path.join(models_directory, file)) # Read each file
    filetitle = file.split("_propval")[0]
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
# modelsdf = modelsdf[['term', "Parcel_ndvi_Mean", "Nobuilding_ndvi_Mean", "morevisible_ndvi_MEAN", "lessvisible_ndvi_MEAN",
#                      'Participant_20m_Mean_ndvi', 'Participant_50m_Mean_ndvi', 'Participant_100m_Mean_ndvi',
#                          'Participant_150m_Mean_ndvi', 'Participant_200m_Mean_ndvi',
#                          'Participant_300m_Mean_ndvi', 'Participant_500m_Mean_ndvi',
#                      "Parcel_ndvi_Sum", "Nobuilding_ndvi_Sum",
#                          'Participant_20m_Sum_ndvi', 'Participant_50m_Sum_ndvi', 'Participant_100m_Sum_ndvi',
#                          'Participant_150m_Sum_ndvi', 'Participant_200m_Sum_ndvi',
#                          'Participant_300m_Sum_ndvi', 'Participant_500m_Sum_ndvi',
#                         "Parcel_NDVI_L19_Mean",
#                      "Nobuilding_NDVI_L19_Mean", "lessvisible_NDVI_L19_MEAN", "morevisible_NDVI_L19_MEAN",
#                          "Participant_20m_Mean_NDVI_L19",
#                      "Participant_50m_Mean_NDVI_L19", "Participant_100m_Mean_NDVI_L19",
#                      "Participant_150m_Mean_NDVI_L19", "Participant_200m_Mean_NDVI_L19",
#                      "Participant_300m_Mean_NDVI_L19", "Participant_500m_Mean_NDVI_L19",
#                         "Parcel_NDVI_L19_Sum", "Nobuilding_NDVI_L19_Sum",
#                      "Participant_20m_Sum_NDVI_L19", "Participant_50m_Sum_NDVI_L19", "Participant_100m_Sum_NDVI_L19",
#                      "Participant_150m_Sum_NDVI_L19", "Participant_200m_Sum_NDVI_L19",
#                      "Participant_300m_Sum_NDVI_L19", "Participant_500m_Sum_NDVI_L19",
#                     "Parcel_canopy_Mean", "Nobuilding_canopy_Mean", "morevisible_canopy_MEAN", "lessvisible_canopy_MEAN",
#                          'Participant_20m_Mean_canopy', 'Participant_50m_Mean_canopy', 'Participant_100m_Mean_canopy',
#                          'Participant_150m_Mean_canopy', 'Participant_200m_Mean_canopy',
#                          'Participant_300m_Mean_canopy', 'Participant_500m_Mean_canopy',
#                         "Parcel_canopy_Sum", "Nobuilding_canopy_Sum",
#                          'Participant_20m_Sum_canopy', 'Participant_50m_Sum_canopy', 'Participant_100m_Sum_canopy',
#                          'Participant_150m_Sum_canopy', 'Participant_200m_Sum_canopy',
#                          'Participant_300m_Sum_canopy', 'Participant_500m_Sum_canopy',
#                          'Participant_50m_Mean_biomass', 'Participant_100m_Mean_biomass',
#                          'Participant_150m_Mean_biomass', 'Participant_200m_Mean_biomass',
#                          'Participant_300m_Mean_biomass', 'Participant_500m_Mean_biomass',
#                          "Parcel_biomass_Mean", "Nobuilding_biomass_Mean", "morevisible_biomass_MEAN", "lessvisible_biomass_MEAN",
#                          'Participant_20m_Sum_biomass', 'Participant_50m_Sum_biomass', 'Participant_100m_Sum_biomass',
#                          'Participant_150m_Sum_biomass', 'Participant_200m_Sum_biomass',
#                          'Participant_300m_Sum_biomass', 'Participant_500m_Sum_biomass',
#                          "Parcel_biomass_Sum", "Nobuilding_biomass_Sum",
#                          'Participant_20m_Mean_leafarea', 'Participant_50m_Mean_leafarea', 'Participant_100m_Mean_leafarea',
#                          'Participant_150m_Mean_leafarea', 'Participant_200m_Mean_leafarea',
#                          'Participant_300m_Mean_leafarea', 'Participant_500m_Mean_leafarea',
#                          "Parcel_leafarea_Mean", "Nobuilding_leafarea_Mean", "morevisible_leafarea_MEAN", "lessvisible_leafarea_MEAN",
#                          'Participant_20m_Sum_leafarea', 'Participant_50m_Sum_leafarea', 'Participant_100m_Sum_leafarea',
#                          'Participant_150m_Sum_leafarea', 'Participant_200m_Sum_leafarea',
#                          'Participant_300m_Sum_leafarea', 'Participant_500m_Sum_leafarea',
#                          "Parcel_leafarea_Sum", "Nobuilding_leafarea_Sum",
#                          'Participant_20m_Mean_LAI', 'Participant_50m_Mean_LAI', 'Participant_100m_Mean_LAI',
#                          'Participant_150m_Mean_LAI', 'Participant_200m_Mean_LAI',
#                          'Participant_300m_Mean_LAI', 'Participant_500m_Mean_LAI',
#                          "Parcel_LAI_Mean", "Nobuilding_LAI_Mean", "morevisible_LAI_MEAN", "lessvisible_LAI_MEAN",
#                          'Participant_20m_Sum_LAI', 'Participant_50m_Sum_LAI', 'Participant_100m_Sum_LAI',
#                          'Participant_150m_Sum_LAI', 'Participant_200m_Sum_LAI',
#                          'Participant_300m_Sum_LAI', 'Participant_500m_Sum_LAI',
#                          "Parcel_LAI_Sum", "Nobuilding_LAI_Sum"]]

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
# modelsdf = modelsdf.rename(columns={
#                                     "morevisible_ndvi_MEAN": "FYard", "lessvisible_ndvi_MEAN": "BYard",
#                                     "Nobuilding_ndvi_Mean": "AllYard", "Parcel_ndvi_Mean": "Parcel",
#                            'Participant_20m_Mean_ndvi': "20m", 'Participant_50m_Mean_ndvi' : "50m",
#                             'Participant_100m_Mean_ndvi': "100m", 'Participant_150m_Mean_ndvi': "150m",
#                             'Participant_200m_Mean_ndvi': "200m",'Participant_300m_Mean_ndvi': "300m",
#                             'Participant_500m_Mean_ndvi' : "500m", "Walktime5_ndvi_Mean" : "Walktime 5min",
#                            "morevisible_NDVI_L19_MEAN" : "FYard", "lessvisible_NDVI_L19_MEAN" : "BYard",
#                             "Nobuilding_NDVI_L19_Mean" : "AllYard", "Parcel_NDVI_L19_Mean" : "Parcel",
#                             "Participant_20m_Mean_NDVI_L19" : "20m", "Participant_50m_Mean_NDVI_L19": "50m",
#                             "Participant_100m_Mean_NDVI_L19": "100m", "Participant_150m_Mean_NDVI_L19": "150m",
#                             "Participant_200m_Mean_NDVI_L19": "200m", "Participant_300m_Mean_NDVI_L19": "300m",
#                             "Participant_500m_Mean_NDVI_L19" : "500m",
#                            "morevisible_canopy_MEAN" : "FYard", "lessvisible_canopy_MEAN" : "BYard",
#                             "Nobuilding_canopy_Mean" : "AllYard", "Parcel_canopy_Mean" : "Parcel",
#                            'Participant_20m_Mean_canopy' : "20m", 'Participant_50m_Mean_canopy' : "50m",
#                             'Participant_100m_Mean_canopy' : "100m", 'Participant_150m_Mean_canopy' : "150m",
#                             'Participant_200m_Mean_canopy' : "200m", 'Participant_300m_Mean_canopy' : "300m",
#                             'Participant_500m_Mean_canopy' : "500m", "Walktime5_canopy_Mean" : "Walktime 5min",
#                            "morevisible_canopy2_MEAN" : "FYard", "lessvisible_canopy2_MEAN" : "BYard",
#                             "Nobuilding_canopy2_Mean" : "AllYard", "Parcel_canopy2_Mean" : "Parcel",
#                            'Participant_20m_Mean_canopy2' : "20m", 'Participant_50m_Mean_canopy2' : "50m",
#                            'Participant_100m_Mean_canopy2' : "100m", 'Participant_150m_Mean_canopy2' : "150m",
#                             'Participant_200m_Mean_canopy2' : "200m", 'Participant_300m_Mean_canopy2' : "300m",
#                             'Participant_500m_Mean_canopy2' : "500m", "Walktime5_canopy2_Mean" : "Walktime 5min",
#                             })
# Output
modelsdf.to_csv("{}\{}.csv".format(output_location, resultfilename))
