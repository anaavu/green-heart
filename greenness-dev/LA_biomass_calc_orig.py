
import arcpy
import os.path
arcpy.env.overwriteOutput = True

# Check out the ArcGIS Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")

# Set workspace
arcpy.env.workspace = workspace = r"E:\GIS\Local_Computer_Stuff\GIS\GH\GHdata_2022.gdb"
# Inputs
ghtrees = r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - " \
          r"General\GIS\GreenHeart\RawData\20210930_Data_Redelivery\20210930_Data_Redelivery\DRG_Greenheart_20210930.gdb\Greenheart_Tree_Segments"
greenmetric = "LA"
ghgeocodes = r"E:\GIS\Local_Computer_Stuff\GIS\GH\GHdata_2022.gdb\ghgeocodes_2022"
geocodes_idfield = "ProcessingID_Long"
walktime_idfield = "Processing"
radii = [20, 50, 100, 150, 200, 250, 300, 500]  # if raster value wanted within a radius (or multiple radius) of participants/points
# radii = [50, 300]  # if raster value wanted within a radius (or multiple radius) of participants/points
# AU: Replace parcels with GH Parcels (make sure GH Parcels are ALL GH)
#polygons as one list
polygonslist = {"parcels": r"I:\Backups\GIS\LOJIC\pva.gdb\LAND\Parcel",
                    "lessvisible" : r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - " \
              r"General\GIS\GreenHeart\ProcessedData\VisibleGreenHeart\LessVisible.shp",
                "morevisible" : r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - " \
              r"General\GIS\GreenHeart\ProcessedData\VisibleGreenHeart\MoreVisible.shp",
                "blockgroups" : r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - " \
              r"General\GIS\CentralResources\block_group_jefferson.shp",
                "walktime5" : r"E:\GIS\Local_Computer_Stuff\GIS\GH\AllParticipants_FiveMin_WalkTime.shp"}
polygonslist = {"walktime5" : r"E:\GIS\Local_Computer_Stuff\GIS\GH\AllParticipants_FiveMin_WalkTime.shp"}

buildings = r"I:\Backups\GIS\LOJIC\ptd.gdb\bg"
outputfolder = r"E:\GIS\Local_Computer_Stuff\GIS\GH\GHdata_2022.gdb"
outputExcel = "AllGHParticipants_{}data_20220203.csv".format(greenmetric)


# Construct parcels without building file
if not arcpy.Exists(outputfolder + "\\parcel_minus_building"):
    arcpy.Erase_analysis(polygonslist["parcels"], buildings,
                                                 outputfolder + "\\parcel_minus_building")
polygonslist["parcel_minus_building"] = outputfolder + "\\parcel_minus_building"


print(arcpy.GetMessages())
print("Set up files for analysis")

ghgeocodes_layer = arcpy.MakeFeatureLayer_management(ghgeocodes, "ghgeocodes_lyr")
ghtrees_lyr = arcpy.MakeFeatureLayer_management(ghtrees, "ghtrees_lyr")
arcpy.management.AddField(ghtrees_lyr, "Area_m2", "DOUBLE")
print(arcpy.GetMessages())
print("0")
# Generate the area (original) in m^2 using CalculateGeometry
# arcpy.CalculateGeometryAttributes_management(ghtrees_lyr, [["Area_m2", "AREA"]],
#                                              area_unit="SQUARE_METERS")
# Uncomment this later!!!
print("1")
# Buffer to produce polygons for point radius based calculations
for each in radii:
    arcpy.Buffer_analysis(ghgeocodes,
                          "GHPart_{}m".format(each), "{} Meters".format(each))
    # Pairwise intersect ghtrees layer and buffer to get smaller polys
    if not arcpy.Exists("trees_partbuffers_{}m".format(each)):
        arcpy.Intersect_analysis([ghtrees, "GHPart_{}m".format(each)],
                                 "trees_partbuffers_{}m".format(each))
    # Delete identical - field Shape_Area
    print("2")
    arcpy.management.DeleteIdentical("trees_partbuffers_{}m".format(each), ["Shape_Area"])
    # Calculate geometry New_Area = m^2
    arcpy.management.AddField("trees_partbuffers_{}m".format(each), "New_Area", "DOUBLE")
    arcpy.CalculateGeometryAttributes_management("trees_partbuffers_{}m".format(each),
                                                 [["New_Area", "AREA"]],
                                                 area_unit="SQUARE_METERS")
    # arcpy.CalculateField_management("trees_partbuffers_{}m".format(each),
    #                                 "New_Area", '!Shape_Area! / 10.764')
    print("3")
    # Calculate field in cut up polygons: (NA/OA)*LA = NLA
    arcpy.management.AddField("trees_partbuffers_{}m".format(each), "New_{}_{}m".format(greenmetric, each), "DOUBLE")
    arcpy.CalculateField_management("trees_partbuffers_{}m".format(each),
                                    "New_{}_{}m".format(greenmetric, each),
                                    '(!New_Area! / !Area_m2!) * !LEAF_AREA!')
    print("4")
    # feature to point on cut up tree polys
    arcpy.FeatureToPoint_management("trees_partbuffers_{}m".format(each),
                                    "trees_partbuffers_{}m_topt".format(each), "INSIDE")
    ## Field map stuff
    # Create a new fieldmappings and add the two input feature classes.
    fieldmappings = arcpy.FieldMappings()
    fieldmappings.addTable("GHPart_{}m".format(each))
    fieldmappings.addTable("trees_partbuffers_{}m_topt".format(each))
    print(fieldmappings)
    print("5")
    # First get the POP1990 fieldmap. POP1990 is a field in the cities feature class.
    # The output will have the states with the attributes of the cities. Setting the
    # field's merge rule to mean will aggregate the values for all of the cities for
    # each state into an average value.
    newgreenfield = fieldmappings.findFieldMapIndex("New_{}_{}m".format(greenmetric, each))
    print(newgreenfield)
    fieldmap = fieldmappings.getFieldMap(newgreenfield)


    # Set the merge rule to mean and then replace the old fieldmap in the mappings object
    # with the updated one
    fieldmap.mergeRule = "Sum"
    fieldmappings.replaceFieldMap(newgreenfield, fieldmap)
    print(fieldmappings)
    print("5")
    # spatial join points to original buffers
    arcpy.analysis.SpatialJoin("GHPart_{}m".format(each), "trees_partbuffers_{}m_topt".format(each),
                               "GHPart_{}m_{}".format(each, greenmetric),
                               field_mapping=fieldmappings)
    # Bring it back to the original points? using join by attributes I guess
    print("6")
    fields = arcpy.ListFields(workspace + "\\" + "GHPart_{}m_{}".format(each, greenmetric))
    # arcpy.management.JoinField(ghgeocodes,
    #                            "ProcessingID_Long",
    #                            "GHPart_20m_LA",
    #                            "ProcessingID_Long",
    #                            fields=["New_LA_20m", "New_Area"])
    field_names = [f.name.upper() for f in fields]
    print(field_names)
    for f in field_names:
        if f.startswith("PROCESS"):
            print(f)
    if "PROCESSINGID_LONG" in field_names:
        print("Exists1")
    if "PROCESSINGID" in field_names:
        print("Exists2")
#     arcpy.ValidateJoin_management(ghgeocodes_layer,
#                                "ProcessingID_Long",
#                                workspace + "\\" + "GHPart_{}m_{}".format(each, greenmetric),
#                                "PROCESSINGID")
#     print(arcpy.GetMessages())
#     arcpy.ValidateJoin_management(ghgeocodes, geocodes_idfield,
#                                "GHPart_{}m_{}".format(each, greenmetric),
#                                geocodes_idfield)
#     print(arcpy.GetMessages())
#     break
#     arcpy.JoinField_management(ghgeocodes, geocodes_idfield,
#                                "GHPart_{}m_{}".format(each, greenmetric),
#                                geocodes_idfield,
#                                fields=["New_{}_{}m".format(greenmetric, each)])


##### PARCEL LEVEL
def processpolygons(polygonfilename, polygonfilepath):
    arcpy.Intersect_analysis([ghtrees, polygonfilepath],
                             "trees_{}".format(polygonfilename))
    # Delete identical - field Shape_Area
    print("2")
    arcpy.management.DeleteIdentical("trees_{}".format(polygonfilename), ["Shape_Area"])
    # Calculate geometry New_Area = m^2
    arcpy.management.AddField("trees_{}".format(polygonfilename), "New_Area", "DOUBLE")
    arcpy.CalculateGeometryAttributes_management("trees_{}".format(polygonfilename),
                                                 [["New_Area", "AREA"]],
                                                 area_unit="SQUARE_METERS")
    print("3")
    # Calculate field in cut up polygons: (NA/OA)*LA = NLA
    arcpy.management.AddField("trees_{}".format(polygonfilename), "New_{}_{}".format(greenmetric, polygonfilename), "DOUBLE")
    arcpy.CalculateField_management("trees_{}".format(polygonfilename),
                                    "New_{}_{}".format(greenmetric, polygonfilename),
                                    '(!New_Area! / !Area_m2!) * !LEAF_AREA!')
    print("4")
    # feature to point on cut up tree polys
    arcpy.FeatureToPoint_management("trees_{}".format(polygonfilename),
                                    "trees_{}_topt".format(polygonfilename), "INSIDE")
    ## Field map stuff
    # Create a new fieldmappings and add the two input feature classes.
    fieldmappings = arcpy.FieldMappings()
    fieldmappings.addTable(polygonfilepath)
    fieldmappings.addTable("trees_{}_topt".format(polygonfilename))
    print(fieldmappings)
    print("5")
    # First get the POP1990 fieldmap. POP1990 is a field in the cities feature class.
    # The output will have the states with the attributes of the cities. Setting the
    # field's merge rule to mean will aggregate the values for all of the cities for
    # each state into an average value.
    newgreenfield = fieldmappings.findFieldMapIndex("New_{}_{}".format(greenmetric, polygonfilename))
    print(newgreenfield)
    fieldmap = fieldmappings.getFieldMap(newgreenfield)

    # Set the merge rule to mean and then replace the old fieldmap in the mappings object
    # with the updated one
    fieldmap.mergeRule = "Sum"
    fieldmappings.replaceFieldMap(newgreenfield, fieldmap)
    print(fieldmappings)
    print("5")
    # spatial join points to original buffers
    arcpy.analysis.SpatialJoin(polygonfilepath, "trees_{}_topt".format(polygonfilename),
                               "{}_{}".format(polygonfilename, greenmetric),
                               field_mapping=fieldmappings)
    # Bring it back to the original points? using join by attributes I guess
    print("6")
    fields = arcpy.ListFields(workspace + "\\" + "{}_{}".format(polygonfilename, greenmetric))
    # arcpy.management.JoinField(ghgeocodes,
    #                            "ProcessingID_Long",
    #                            "GHPart_20m_LA",
    #                            "ProcessingID_Long",
    #                            fields=["New_LA_20m", "New_Area"])
    field_names = [f.name.upper() for f in fields]
    print(field_names)
    for f in field_names:
        if f.startswith("PROCESS"):
            print(f)
    if "PROCESSINGID_LONG" in field_names:
        print("Exists1")
    if "PROCESSINGID" in field_names:
        print("Exists2")
    participants_polygons = arcpy.SpatialJoin_analysis(ghgeocodes_layer, polygonfilepath,
                                                      outputfolder + "\\participants_{}_{}".format(polygonfilename, greenmetric))
    # arcpy.ValidateJoin_management(ghgeocodes_layer,
    #                               "ProcessingID_Long",
    #                               workspace + "\\" + "{}_{}".format(polygonfilename, greenmetric),
    #                               "PROCESSINGID")
    # print(arcpy.GetMessages())
    # arcpy.ValidateJoin_management(ghgeocodes, geocodes_idfield,
    #                               "{}_{}".format(polygonfilename, greenmetric),
    #                               geocodes_idfield)
    # print(arcpy.GetMessages())
    # arcpy.JoinField_management(ghgeocodes, geocodes_idfield,
    #                            "{}_{}".format(polygonfilename, greenmetric),
    #                            geocodes_idfield,
    #                            fields=["New_{}_{}".format(greenmetric, polygonfilename)])
    return None

for key in polygonslist:
    processpolygons(key, polygonslist[key])

# Joining yard to parcel
if not arcpy.Exists(outputfolder + "\\Parcels_LessVisible"):
    parcel_lessvisible = arcpy.SpatialJoin_analysis(polygonslist["parcels"], polygonslist["lessvisible"],
                                                    outputfolder + "\\Parcels_LessVisible",
                                                    match_option="CONTAINS")
if not arcpy.Exists(outputfolder + "\\Parcels_LessMoreVisible"):
    parcel_lessmorevisible = arcpy.SpatialJoin_analysis(parcel_lessvisible, polygonslist["morevisible"],
                                                        outputfolder + "\\Parcels_LessMoreVisible",
                                                        match_option="CONTAINS")

# Join GH participants with parcels
participants_parcels = arcpy.SpatialJoin_analysis(ghgeocodes_layer, polygonslist["parcels"],
                                                  outputfolder + "\\participants_parcels")
participants_bg = arcpy.SpatialJoin_analysis(ghgeocodes_layer, polygonslist["blockgroups"],
                                             outputfolder + "\\participants_bg")
