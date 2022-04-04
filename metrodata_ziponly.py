# libraries
import os
import pandas as pd
import arcpy
from arcpy.sa import *
arcpy.env.overwriteOutput = True

os.chdir("C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS")
arcpy.env.workspace = workspace = "COVID/GeocodingItemizedSteps/CSV/Oct11_data/ziponly_output_coimmunity"
# data input
dates = ["20200525", "20200601", "20200706", "20200720", "20200727", "20200803", "20200810", "20200817", "20200824",
         "20200831", "20200907", "20200914", "20200921", "20200928", "20201005", "20201012", "20201019", "20201026",
         "20201102", "20201109", "20201116", "20201123", "20201130", "20201207", "20201214", "20201221", "20201228",
         "20210104", "20210111", "20210118", "20210125", "20210201", "20210208", "20210215", "20210222", "20210301",
         "20210308", "20210315", "20210322", "20210329", "20210405", "20210412", "20210419", "20210426", "20210503",
         "20210510", "20210517", "20210531", "20210705", "20210712", "20210719", "20210726", "20210802", "20210809",
         "20210816", "20210823", "20210830", "20210906", "20210913", "20210920", "20210927", "20211004"]
for date in dates:
    ziponly_input = pd.read_csv("COVID/GeocodingItemizedSteps/CSV/Oct11_data/newdataset_ziponly_JZ/newdataset_ziponly_{}.csv".format(date), low_memory=False)
    usable_records = pd.read_csv("COVID/GeocodingItemizedSteps/CSV/Oct11_data/All_geocoded_noziponly.csv", encoding= 'ISO-8859-1', low_memory=False)
    # ziponly_input = pd.read_csv(r"C:\Users\a0uppa01\University of Louisville\Envirome Institutemembers_group - General\GIS\COVID\GeocodingItemizedSteps\CSV\ziponly_input.csv", dtype='str')
    usable_records['USER_lhd_z'] = pd.to_numeric(usable_records['USER_lhd_z'], errors='coerce')
    blocks = "COVID/ProcessedData/more_sheds/Sewersheds/blocks_zip_sewersheds_coimm.shp"
    zipcodes = "CentralResources/zcta_spatial_population/zcta_spatial_population_Kentucky.shp"


    # frequency of Ids per county
    freq_ziponly = pd.crosstab(index=ziponly_input['USER_lhd_z'], columns='count_zip')
    # freq_main = pd.crosstab(index=usable_records['Zip.Code'], columns='count_patient')
    # freq_all = freq_ziponly.merge(freq_main, how='left', on='Zip.Code')
    # freq_all['bias'] = freq_all['count_zip']/freq_all['count_patient']
    # freq_all.to_csv("COVID/GeocodingItemizedSteps/CSV/Sept21_data/freq_all.csv")

    # join zip code data to block data and come out with a usable table
    sjres = arcpy.SpatialJoin_analysis(target_features=blocks, join_features=zipcodes,
                               out_feature_class="intermediate/block_zip.shp", match_option="HAVE_THEIR_CENTER_IN")
    blocks_zip_df = arcpy.TableToTable_conversion(sjres, "{}/intermediate".format(workspace), "blocks_zip_df_{}.csv".format(date))
    blocks_zip_df = pd.read_csv("COVID/GeocodingItemizedSteps/CSV/Oct11_data/ziponly_output_coimmunity/intermediate/blocks_zip_df_{}.csv".format(date))

    # zip by zip population summary. note that zip file already had a population
    # count but we may not trust that one.
    # zip pop sum seems to consistently overcount population total
    # unless total diff is diff of 2010/2019
    # total population acc to blocks 741K. total acc to zips ~760K
    # est population is 766K
    # so actually, blocks 2010 is undercounting pop
    zipsummary = blocks_zip_df[["ZCTA", "Total"]].groupby("ZCTA").sum()

    blocks_zip_df = blocks_zip_df.merge(zipsummary, how='left', on='ZCTA')

    # Distribution ratio (block pop/zip pop)
    blocks_zip_df['Distribution_ratio'] = blocks_zip_df['Total_x'] / blocks_zip_df['Total_y']

    # Merge freq (ziponly_input) to blocks_zip_df
    # Multiply blocks_df_zip dist ratio * cases per county
    blocks_zip_df = blocks_zip_df.merge(freq_ziponly, how='left', left_on='ZCTA', right_on='USER_lhd_z')
    blocks_zip_df['Cases_per_block'] = blocks_zip_df['Distribution_ratio'] * blocks_zip_df['count_zip']

    # Math.floor half and math.ceiling half
    blocks_zip_df.to_csv("COVID/GeocodingItemizedSteps/CSV/Oct11_data/ziponly_output_coimmunity/ziponly_inblocks_{}.csv".format(date))


    # 1:n of value (value as dict) gets assigned
