import arcpy
from arcpy.sa import *


file_path = r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - General\GIS\GreenHeart\ProcessedData\NDVI_7in_Mosaic.tif"
radii_in_m = [20, 50, 100, 150, 200, 300, 500, 1000]
radii_in_m = [20, 50, 100, 150, 300, 500, 1000]

arcpy.env.workspace = r"C:\Users\a0uppa01\Documents\GIS"
print(file_path)

for each in radii_in_m:
    print("Running focal stats of radius: ", each)
    radius_in_ft = each*3.28
    outFocalStatistics = FocalStatistics(in_raster=file_path,
                                         neighborhood=NbrCircle(radius_in_ft, "MAP"))
    outFocalStatistics.save("ndvi_focstat_"+str(each)+"m.tif"
                                                          "")
    print(arcpy.GetMessages())

