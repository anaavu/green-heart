# libraries
import pandas as pd
import arcpy
from arcpy.sa import *
arcpy.env.overwriteOutput = True


arcpy.env.workspace = workspace = r"C:\Users\a0uppa01\University of Louisville\Envirome " \
                                  r"Institute members_group - General\GIS\COVID\Geocoding" \
                                  r"ItemizedSteps\CSV"
# data input
ziponly_input = pd.read_csv(r"C:\Users\a0uppa01\University of Louisville\Envirome Institute "
                            r"members_group - General\GIS\COVID\GeocodingItemized"
                            r"Steps\CSV\ziponly_input.csv", dtype='str')
print(list(ziponly_input.columns))
usable_records = pd.read_csv(r"C:\Users\a0uppa01\University of Louisville\Envirome Institute "
                            r"members_group - General\GIS\COVID\GeocodingItemized"
                            r"Steps\CSV\usable_records.csv", encoding= 'ISO-8859-1', low_memory=False)
blocks = r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - " \
         r"General\GIS\CentralResources\blocks_spatial_population\blocks_spatial_" \
         r"population_Jefferson.shp"
zipcodes = r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group " \
           r"- General\GIS\CentralResources\zcta_spatial_population\zcta_spatial_" \
           r"population_Kentucky.shp"

# frequency of Ids per county
freq = pd.crosstab(index=ziponly_input['Zip.Code'], columns='count')

# join zip code data to block data and come out with a usable table
sjres = arcpy.SpatialJoin_analysis(target_features=blocks, join_features=zipcodes,
                           out_feature_class="block_zip.shp", match_option="HAVE_THEIR_CENTER_IN")
blocks_zip_df = arcpy.TableToTable_conversion(sjres, workspace, "blocks_zip_df.csv")
blocks_zip_df = pd.read_csv(r"C:\Users\a0uppa01\University of Louisville\Envirome " \
                                  r"Institute members_group - General\GIS\COVID\Geocoding" \
                                  r"ItemizedSteps\CSV\blocks_zip_df.csv")

# zip by zip population summary. note that zip file already had a population
# count but we may not trust that one.
# zip pop sum seems to consistently overcount population total
# unless total diff is diff of 2010/2019
# total population acc to blocks 741K. total acc to zips ~760K
# est population is 766K
# so actually, blocks 2010 is undercounting pop
zipsummary = blocks_zip_df[["ZCTA", "Total"]].groupby("ZCTA").sum()
zipsummary.to_csv(r"C:\Users\a0uppa01\University of Louisville\Envirome " \
                                  r"Institute members_group - General\GIS\COVID\Geocoding" \
                                  r"ItemizedSteps\CSV\zipsummary.csv")

blocks_zip_df = blocks_zip_df.merge(zipsummary, how='left', on='ZCTA')

# Distribution ratio (block pop/zip pop)
blocks_zip_df['Distribution_ratio'] = blocks_zip_df['Total_x'] / blocks_zip_df['Total_y']

# Merge freq (ziponly_input) to blocks_zip_df
# Multiply blocks_df_zip dist ratio * cases per county
blocks_zip_df = blocks_zip_df.merge(freq, how='left', left_on='ZCTA', right_on='Zip.Code')
blocks_zip_df['Cases_per_block'] = blocks_zip_df['Distribution_ratio'] * blocks_zip_df['count']
blocks_zip_df.to_csv(r"C:\Users\a0uppa01\University of Louisville\Envirome " \
                                  r"Institute members_group - General\GIS\COVID\Geocoding" \
                                  r"ItemizedSteps\CSV\ziponly_inblocks.csv")

# Math.floor half and math.ceiling half

# 1:n of value (value as dict) gets assigned

