
import arcpy
arcpy.env.overwriteOutput = True

# Check out the ArcGIS Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")
arcpy.CheckOutExtension("BusinessPrem")

# Set workspace
arcpy.env.workspace = workspace = r"E:\GIS\Local_Computer_Stuff\GIS\GH\GHdata_2022.gdb"
# Inputs
ghtrees_sample = "ghtrees_sample"
ghtrees = r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - " \
          r"General\GIS\GreenHeart\RawData\20210930_Data_Redelivery\20210930_Data_Redelivery\DRG_Greenheart_20210930.gdb\Greenheart_Tree_Segments"
greenmetric = "LA"
ghgeocodes = r"E:\GIS\Local_Computer_Stuff\GIS\GH\GHdata_2022.gdb\ghgeocodes_2022"
geocodes_idfield = "ProcessingID_Long"
walktime_idfield = "Processing"
radii = [20, 50, 100, 150, 200, 250, 300, 500]  # if raster value wanted within a radius (or multiple radius) of participants/points
# radii = [50, 300]  # if raster value wanted within a radius (or multiple radius) of participants/points
# ghdata = "Documents\\GIS\\GHData_RY_041720.csv"
parcels = r"I:\Backups\GIS\LOJIC\pva.gdb\LAND\Parcel"
buildings = r"I:\Backups\GIS\LOJIC\ptd.gdb\bg"
lessvisible = r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - " \
              r"General\GIS\GreenHeart\ProcessedData\VisibleGreenHeart\LessVisible.shp"
morevisible = r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - " \
              r"General\GIS\GreenHeart\ProcessedData\VisibleGreenHeart\MoreVisible.shp"
blockgroups = r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - " \
              r"General\GIS\CentralResources\block_group_jefferson.shp"
walktime5 = r"I:\Backups\GIS\Local_Computer_Stuff\GIS\AllParticipants_FiveMin_WalkTime.shp"
outputfolder = r"E:\GIS\Local_Computer_Stuff\GIS\GH\GHdata_2022.gdb"
outputExcel = "AllGHParticipants_{}data_20220203.csv".format(greenmetric)

# Construct parcels without building file
if not arcpy.Exists(outputfolder + "\\parcel_minus_building"):
    parcel_minus_building = arcpy.Erase_analysis(parcels, buildings,
                                                 outputfolder + "\\parcel_minus_building")
else:
    parcel_minus_building = outputfolder + "\\parcel_minus_building"
print("Set up files for analysis")
print(arcpy.GetMessages())

ghtrees_lyr = arcpy.MakeFeatureLayer_management(ghtrees, "ghtrees_lyr")
lstFields = arcpy.ListFields(ghtrees_lyr)

if not "Area_m2" in lstFields:
    arcpy.management.AddField(ghtrees_lyr, "Area_m2", "DOUBLE")
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
    arcpy.analysis.PairwiseIntersect([ghtrees, "GHPart_{}m".format(each)],
                                     "trees_partbuffers_{}m".format(each))
    # Delete identical - field Shape_Area
    print("2")
    #arcpy.management.DeleteIdentical("trees_partbuffers_{}m".format(each), ["Shape_Area"])
    trees_partbuffers_lyr = arcpy.MakeFeatureLayer_management("trees_partbuffers_{}m".format(each), "trees_partbuffers_lyr")
    ghtrees_sample_lyr = arcpy.MakeFeatureLayer_management(ghtrees_sample, "ghtrees_sample_lyr")
    #arcpy.ba.RemoveOverlap("trees_partbuffers_20m", "trees_overlaprm")
    print(ghtrees)
    arcpy.analysis.RemoveOverlapMultiple(trees_partbuffers_lyr, "trees_overlaprm11")
    print(arcpy.GetMessages())
    arcpy.analysis.RemoveOverlapMultiple("trees_partbuffers_20m", "trees_overlaprm")
    #                                     "trees_partbuffers_{}m_overlaprm".format(each))
    # Calculate geometry New_Area = m^2
    arcpy.management.AddField("trees_partbuffers_{}m_overlaprm".format(each), "New_Area", "DOUBLE")
    # arcpy.CalculateGeometryAttributes_management("trees_partbuffers_{}m".format(each),
    #                                              [["New_Area", "AREA"]],
    #                                              area_unit="SQUARE_METERS")
    arcpy.CalculateField_management("trees_partbuffers_{}m_overlaprm".format(each),
                                    "New_Area", '!Shape_Area! / 10.764')
    print("3")
    # Calculate field in cut up polygons: (NA/OA)*LA = NLA
    arcpy.management.AddField("trees_partbuffers_{}m_overlaprm".format(each), "New_{}_{}m".format(greenmetric, each), "DOUBLE")
    arcpy.CalculateField_management("trees_partbuffers_{}m_overlaprm".format(each),
                                    "New_{}_{}m".format(greenmetric, each),
                                    '(!New_Area! / !Area_m2!) * !LEAF_AREA!')
    print("4")
    # feature to point on cut up tree polys
    arcpy.FeatureToPoint_management("trees_partbuffers_{}m_overlaprm".format(each),
                                    "trees_partbuffers_{}m_overlaprm_topt".format(each), "INSIDE")
    ## Field map stuff
    # Create a new fieldmappings and add the two input feature classes.
    fieldmappings = arcpy.FieldMappings()
    fieldmappings.addTable("GHPart_{}m".format(each))
    fieldmappings.addTable("trees_partbuffers_{}m_overlaprm_topt".format(each))
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
    arcpy.analysis.SpatialJoin("GHPart_{}m".format(each), "trees_partbuffers_{}m_overlaprm_topt".format(each),
                               "GHPart_{}m_{}".format(each, greenmetric),
                               field_mapping=fieldmappings)
    # Bring it back to the original points? using join by attributes I guess
    print("6")
    fields = arcpy.ListFields("GHPart_{}m_{}".format(each, greenmetric))
    for field in fields:
        print("{0} is a type of {1} with a length of {2}"
              .format(field.name, field.type, field.length))
    print("New_{}_{}m".format(greenmetric, each))
    # arcpy.management.JoinField(ghgeocodes,
    #                            "ProcessingID_Long",
    #                            "GHPart_20m_LA",
    #                            "ProcessingID_Long",
    #                            fields=["New_LA_20m", "New_Area"])
    arcpy.JoinField_management(ghgeocodes, geocodes_idfield,
                               "GHPart_{}m_{}".format(each, greenmetric),
                               geocodes_idfield,
                               fields=["New_{}_{}m".format(greenmetric, each)])
