library("rgdal")
library("gdalUtils")
library("raster")
library(sf)
sf::sf_extSoftVersion()


setwd("C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/GreenHeart/RawData/DEM_7in_NDVI/NDVI_DEM")
a <- list.files(path = ".", pattern = ".tif$")
e <- extent(1188289.858711, 1215289.858711, 240663.793757, 271263.793757)
template <- raster(e)
proj4string(template) <- CRS('+proj=lcc +lat_1=37.96666666666667 +lat_2=38.96666666666667 +lat_0=37.5 +lon_0=-84.25 +x_0=500000.0000000002 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs')

filename <- "C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/GreenHeart/ProcessedData/NDVI_7in_Mosaic_TakeTwo.tif"
writeRaster(template, file="NDVI_7in_Mosaic.tif", format="GTiff")
gdalUtils::mosaic_rasters(gdalfile=a,dst_dataset="NDVI_7in_Mosaic.tif",of="GTiff")
gdalinfo("NDVI_7in_Mosaic.tif")
