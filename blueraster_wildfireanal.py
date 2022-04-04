import pandas as pd
import arcpy
import arcpy.management
import os
arcpy.env.overwriteOutput = True


os.chdir("E:/bluerastertest")
arcpy.env.workspace = workspace = "E:/bluerastertest"
if not arcpy.Exists("wildfires.gdb"):
    arcpy.CreateFileGDB_management(workspace, "wildfires.gdb")
wfdatapath = "MODIS_C6_1_South_America_7d.csv"
wfdata = pd.read_csv(wfdatapath, low_memory=False)
countrybounds = "ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"
countrylines = "ne_10m_admin_0_boundary_lines_land/ne_10m_admin_0_boundary_lines_land.shp"
outgdb = "wildfires.gdb"



# Create a gdb as new workspace

# Requirements:
# Count of fires by country
# Highest confidence fire in each country with a fire

# Lat long to points
wfdata_pts = arcpy.management.XYTableToPoint(in_table=workspace+"/"+wfdatapath,
                                             out_feature_class=outgdb+"/"+"wfdata_pts",
                                             x_field="longitude", y_field="latitude",
                                             coordinate_system=arcpy.SpatialReference(4326))


## Make sure reference is in WGD84 to match country boundaries
# Create the spatial reference object
spatial_ref = arcpy.Describe(countrybounds).spatialReference

# If the spatial reference is unknown
if spatial_ref.name == "Unknown":
    print("{} has an unknown spatial reference".format(countrybounds))

# Otherwise, print out the feature class name and spatial reference
else:
    print("{} : {}".format(countrybounds, spatial_ref.name))

# Create the required FieldMap and FieldMappings objects
countryabbr = arcpy.FieldMap()
countryname = arcpy.FieldMap()
confidence = arcpy.FieldMap()
fms = arcpy.FieldMappings()

# Get the field names of vegetation type and diameter for both original files
# Add fields to their corresponding FieldMap objects
countryabbr.addInputField(countrybounds, "SOV_A3")
countryname.addInputField(countrybounds, "ADMIN")
confidence.addInputField(wfdata_pts, "confidence")

# Set the output field properties for both FieldMap objects
confidence.mergeRule = "Maximum"

# Add the FieldMap objects to the FieldMappings object
fms.addFieldMap(countryabbr)
fms.addFieldMap(countryname)
fms.addFieldMap(confidence)


print(fms)
# spatial join points to countries
countries_wildfires = arcpy.SpatialJoin_analysis(target_features=countrybounds,join_features=wfdata_pts,
                                                 out_feature_class=outgdb+"/"+"countries_wildfires",
                                                 field_mapping=fms)
arcpy.TableToTable_conversion(countries_wildfires, workspace, "countries_wildfires.csv")


# Optional
# Fires within 5 km of a border
fieldmappings = arcpy.FieldMappings()
fieldmappings.addTable(wfdata_pts)
wf_nearborders = arcpy.SpatialJoin_analysis(target_features=wfdata_pts,join_features=countrylines,
                                                 out_feature_class=outgdb+"/"+"wf_nearborders",
                                            join_type="KEEP_COMMON", search_radius="5 Kilometers",
                                            field_mapping=fieldmappings)
arcpy.TableToTable_conversion(wf_nearborders, workspace, "wf_nearborders.csv")

arcpy.management.AddField(wfdata_pts, "Avg Fire Dist", "DOUBLE")
fields = ['OID_','Avg Fire Dist']
# Average distance to all other fires
# not to use updatecursor/searchcursor -- very very slow
# Uncomment the below lines
# wfdist = arcpy.GenerateNearTable_analysis(in_features=wfdata_pts,near_features=wfdata_pts,out_table="wfdist",
#                                          closest='ALL', closest_count=6000, method='GEODESIC')
# arcpy.TableToTable_conversion(wfdist, workspace, "wfdist.csv")

# pandas read csv and group by mean
wfdist1 = pd.read_csv('wfdist.csv')
wfdist_avgbywf = wfdist1.groupby(['IN_FID'])['NEAR_DIST'].mean().reset_index().to_frame()
wfdist_avgbywf.to_csv('wfdist_avgbywf.csv')


# Put these things in a gdb
# List which wildfires were not captured: number in original file, number converted to pts layer, number joined to country
# Future ideas: Jupyter notebook
# Geoserver or Postgis server
# Make it object oriented
# add time recording
# Multivariate clustering and choose max?