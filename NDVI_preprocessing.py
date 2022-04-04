
import arcpy
from arcpy.sa import *
arcpy.env.overwriteOutput = True
arcpy.env.parallelProcessingFactor = "72%"

# Check out the ArcGIS Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")
# Set workspace
arcpy.env.workspace = r"I:\Backups\GIS\Local_Computer_Stuff\GIS\GH"
# Inputs
ghgeocodes = r"E:\GIS\Local_Computer_Stuff\GIS\GH\GHdata_2022.gdb\ghgeocodes_2022"
geocodes_idfield = "ProcessingID"
walktime_idfield = "Processing"
radii = [20, 50, 100, 150, 200, 250, 300, 500]  # if raster value wanted within a radius (or multiple radius) of participants/points
# radii = [50, 300] ## if raster value wanted within a radius (or multiple radius) of participants/points
parcels = r"I:\Backups\GIS\LOJIC\pva.gdb\LAND\Parcel"
buildings = r"I:\Backups\GIS\LOJIC\ptd.gdb\bg"
rasterfile = "ndvi_2016"
rasterfilepath = r"E:\GIS\Local_Computer_Stuff\GIS\GH\GHflyoverData2016\Hyperspectral\TNC_Davey_Louisville_AOIbuff100m_HSI_NDVI_1m_Project.tif"
# rasterfilepath = r"I:\Backups\GIS\Local_Computer_Stuff\GIS\GH\GHflyoverData2016\Hyperspectral\TNC_Davey_Louisville_AOIbuff100m_HSI_NDVI_1m_Project.tif"
lessvisible = r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - " \
              r"General\GIS\GreenHeart\ProcessedData\VisibleGreenHeart\LessVisible.shp"
morevisible = r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - " \
              r"General\GIS\GreenHeart\ProcessedData\VisibleGreenHeart\MoreVisible.shp"
blockgroups = r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - " \
              r"General\GIS\CentralResources\block_group_jefferson.shp"
walktime5 = r"I:\Backups\GIS\Local_Computer_Stuff\GIS\GH\AllParticipants_FiveMin_WalkTime.shp"
outputfolder = r"E:\GIS\Local_Computer_Stuff\GIS\GH\GHdata_2022.gdb"

# Construct parcels without building file
if not arcpy.Exists(outputfolder + "\\parcel_minus_building"):
    parcel_minus_building = arcpy.Erase_analysis(parcels, buildings,
                                                 outputfolder + "\\parcel_minus_building")
else:
    parcel_minus_building = outputfolder + "\\parcel_minus_building"
print("Set up files for analysis")
print(arcpy.GetMessages())

# Buffer to produce polygons for point radius based zonal stats
for each in radii:
    arcpy.Buffer_analysis(ghgeocodes,
                          "in_memory\\Part_{}m".format(each), "{} Meters".format(each))
# Add rasterfile information to all files using Zonal Statistics as Table
for each in radii:
    ZonalStatisticsAsTable("in_memory\\Part_{}m".format(each), geocodes_idfield,
                           rasterfilepath,
                           outputfolder + "\\Part_{}m_".format(each)+rasterfile)
parcel_raster = ZonalStatisticsAsTable(parcels, "PARCELID", rasterfilepath,
                                       outputfolder + "\\parcels_"+rasterfile, statistics_type="ALL")
nobuilding_raster = ZonalStatisticsAsTable(parcel_minus_building, "PARCELID", rasterfilepath,
                                           outputfolder + "\\nobuild_"+rasterfile, statistics_type="ALL")
print(arcpy.GetMessages())
lessvisible_raster = ZonalStatisticsAsTable(lessvisible, "PARCEL_ID", rasterfilepath,
                                            outputfolder + "\\lessvis_"+rasterfile, statistics_type="ALL")
morevisible_raster = ZonalStatisticsAsTable(morevisible, "PARCEL_ID", rasterfilepath,
                                            outputfolder + "\\morevis_"+rasterfile, statistics_type="ALL")
bg_raster = ZonalStatisticsAsTable(blockgroups, "FIPS", rasterfilepath,
                                   outputfolder + "\\bg_"+rasterfile, statistics_type="ALL")
walk_raster = ZonalStatisticsAsTable(walktime5, walktime_idfield, rasterfilepath,
                                     outputfolder + "\\wlktm5_"+rasterfile, statistics_type="ALL")

print("Completed zonal statistics")

# Joining yard to parcel
if not arcpy.Exists(outputfolder + "\\Parcels_LessVisible"):
    parcel_lessvisible = arcpy.SpatialJoin_analysis(parcels, lessvisible,
                                                    outputfolder + "\\Parcels_LessVisible",
                                                    match_option="CONTAINS")
if not arcpy.Exists(outputfolder + "\\Parcels_LessMoreVisible"):
    parcel_lessmorevisible = arcpy.SpatialJoin_analysis(parcel_lessvisible, morevisible,
                                                        outputfolder + "\\Parcels_LessMoreVisible",
                                                        match_option="CONTAINS")
print(arcpy.GetMessages())
