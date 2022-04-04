import arcpy
arcpy.env.overwriteOutput = True

# Check out the ArcGIS Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")
# Set workspace
arcpy.env.workspace = workspace = r"E:\GIS\Local_Computer_Stuff\GIS\GH"
# Inputs
ghgeocodes = r"E:\GIS\Local_Computer_Stuff\GIS\GH\GHdata_2022.gdb\ghgeocodes_2022"
geocodes_idfield = "ProcessingID"
walktime_idfield = "Processing"
radii = [20, 50, 100, 150, 200, 250, 300, 500]  # if raster value wanted within a radius (or multiple radius) of participants/points
# radii = [50, 300]  # if raster value wanted within a radius (or multiple radius) of participants/points
# ghdata = "Documents\\GIS\\GHData_RY_041720.csv"
parcels = r"I:\Backups\GIS\LOJIC\pva.gdb\LAND\Parcel"
buildings = r"I:\Backups\GIS\LOJIC\ptd.gdb\bg"
rasterfile = "ndvi_2016"
# rasterfilepath = r"I:\Backups\GIS\greenness_rasters\biomass_density_raster\leafarea_dens.tif"
rasterfilepath = r"E:\GIS\Local_Computer_Stuff\GIS\GH\GHflyoverData2016\Hyperspectral\TNC_Davey_Louisville_AOIbuff100m_HSI_NDVI_1m_Project.tif"
lessvisible = r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - " \
              r"General\GIS\GreenHeart\ProcessedData\VisibleGreenHeart\LessVisible.shp"
morevisible = r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - " \
              r"General\GIS\GreenHeart\ProcessedData\VisibleGreenHeart\MoreVisible.shp"
blockgroups = r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - " \
              r"General\GIS\CentralResources\block_group_jefferson.shp"
walktime5 = r"I:\Backups\GIS\Local_Computer_Stuff\GIS\AllParticipants_FiveMin_WalkTime.shp"
outputfolder = r"E:\GIS\Local_Computer_Stuff\GIS\GH\GHdata_2022.gdb"
outputExcel = "AllGHParticipants_{}data_20220203.csv".format(rasterfile)

# Making layers for those features which will join together
ghgeocodes_layer = arcpy.MakeFeatureLayer_management(ghgeocodes, "ghgeocodes_lyr")
arcpy.MakeFeatureLayer_management(outputfolder + "\\Parcels_LessMoreVisible", "parcel_yard_lyr")
print("Converted fcs to layers")

# Join raster data to parcel-yards
arcpy.AddJoin_management("parcel_yard_lyr", "PARCEL_ID",
                         outputfolder + "\\lessvis_"+rasterfile, "PARCEL_ID")
arcpy.AddJoin_management("parcel_yard_lyr", "PARCEL_ID",  # check AU
                         outputfolder + "\\morevis_"+rasterfile, "PARCEL_ID")
print("Joined yards, raster vals and corresponding parcels")

# Join GH participants with parcels
participants_parcels = arcpy.SpatialJoin_analysis(ghgeocodes_layer, parcels,
                                                  outputfolder + "\\participants_parcels")
participants_bg = arcpy.SpatialJoin_analysis(ghgeocodes_layer, blockgroups,
                                             outputfolder + "\\participants_bg")

if arcpy.Exists(ghgeocodes_layer):
    desc = arcpy.Describe(ghgeocodes_layer)
    field_names = [f.name for f in arcpy.ListFields(ghgeocodes_layer)]
    print(field_names)
    arcpy.DeleteField_management(ghgeocodes_layer,
                                 ["Field86"])
    if "SUM" in field_names:
        arcpy.DeleteField_management(ghgeocodes_layer, ["SUM"])
    if "MEAN" in field_names:
        arcpy.DeleteField_management(ghgeocodes_layer, ["MEAN"])
# Join all data together
# Each participant radius
for each in radii:
    ghgeocodes_layer = arcpy.JoinField_management(ghgeocodes_layer, geocodes_idfield,
                                                  outputfolder + "\\Part_{}m_".format(each)+rasterfile,
                                                  geocodes_idfield, ["MEAN", "SUM"])
    arcpy.AlterField_management(ghgeocodes_layer, "SUM",
                                "Part_{}m_Sum_{}".format(each, rasterfile),
                                "Part {}m Sum {}".format(each, rasterfile))
    arcpy.AlterField_management(ghgeocodes_layer, "MEAN",
                                "Part_{}m_Mean_{}".format(each, rasterfile),
                                "Part {}m Mean {}".format(each, rasterfile))
# Parcel ID
ghgeocodes_layer = arcpy.JoinField_management(ghgeocodes_layer, geocodes_idfield, participants_parcels,
                                              geocodes_idfield, ["PARCELID"])
# Parcel raster using Parcel ID
ghgeocodes_layer = arcpy.JoinField_management(ghgeocodes_layer, "PARCELID",
                                              outputfolder + "\\parcels_"+rasterfile, "PARCELID",
                                              ["SUM", "MEAN"])
arcpy.AlterField_management(ghgeocodes_layer, "SUM", 'Parcel_{}_Sum'.format(rasterfile),
                            'Parcel {} Sum'.format(rasterfile))
arcpy.AlterField_management(ghgeocodes_layer, "MEAN", 'Parcel_{}_Mean'.format(rasterfile),
                            'Parcel {} Mean'.format(rasterfile))
# Nobuilding raster using Parcel ID
ghgeocodes_layer = arcpy.JoinField_management(ghgeocodes_layer, "PARCELID",
                                              outputfolder + "\\nobuild_"+rasterfile, "PARCELID",
                                              ["SUM", "MEAN"])
arcpy.AlterField_management(ghgeocodes_layer, "SUM", 'Nobuild_{}_Sum'.format(rasterfile),
                            'Nobuild {} Sum'.format(rasterfile))
arcpy.AlterField_management(ghgeocodes_layer, "MEAN", 'Nobuild_{}_Mean'.format(rasterfile),
                            'Nobuild {} Mean'.format(rasterfile))
# Less and more visible raster using Parcel ID
ghgeocodes_layer = arcpy.JoinField_management(ghgeocodes_layer, "PARCELID", "parcel_yard_lyr",
                                              "PARCELID", ["lessvis_{}.Mean".format(rasterfile),
                                                           "morevis_{}.Mean".format(rasterfile)])

# Block group
ghgeocodes_layer = arcpy.JoinField_management(ghgeocodes_layer, geocodes_idfield, participants_bg,
                                              geocodes_idfield, ["FIPS"])
# Block group raster using Block group ID
ghgeocodes_layer = arcpy.JoinField_management(ghgeocodes_layer, "FIPS",
                                              outputfolder + "\\bg_"+rasterfile, "FIPS",
                                              ["SUM", "MEAN"])
arcpy.AlterField_management(ghgeocodes_layer, "SUM", 'BG_{}_Sum'.format(rasterfile),
                            'BG {} Sum'.format(rasterfile))
arcpy.AlterField_management(ghgeocodes_layer, "MEAN", 'BG_{}_Mean'.format(rasterfile),
                            'BG {} Mean'.format(rasterfile))
arcpy.AlterField_management(ghgeocodes_layer, "PARCELID", 'ParcelID_{}'.format(rasterfile),
                            'ParcelID {}'.format(rasterfile))
arcpy.AlterField_management(ghgeocodes_layer, "FIPS", 'FIPS_{}'.format(rasterfile),
                            'FIPS {}'.format(rasterfile))

# Walktime raster
ghgeocodes_layer = arcpy.JoinField_management(ghgeocodes_layer, geocodes_idfield,
                                              outputfolder + "\\wlktm5_"+rasterfile,
                                              walktime_idfield, ["SUM", "MEAN"])
arcpy.AlterField_management(ghgeocodes_layer, "SUM",
                            "Wlktm5_{}_Sum".format(rasterfile),
                            "Wlktm5 {} Sum".format(rasterfile))
arcpy.AlterField_management(ghgeocodes_layer, "MEAN",
                            "Wlktm5_{}_Mean".format(rasterfile),
                            "Wlktm5 {} Mean".format(rasterfile))

print("Aggregated all data in one")
# sj - participants_parcels, Parcel_LessMoreVisible, participants_parcels_yards
# Export final dataset
# arcpy.TableToExcel_conversion(ghgeocodes_layer, "Documents\\GIS\\{}".format(outputExcel))
arcpy.TableToTable_conversion(ghgeocodes_layer, workspace, outputExcel)
arcpy.FeatureClassToGeodatabase_conversion("parcel_yard_lyr", outputfolder)
