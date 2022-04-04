#######################################################################################
## Author: Anagha Uppal
## Date: June 6, 2020
## Purpose: Assign area-weighted (largest=assignment) hazard values to each zip code    
## in US using spatial join for vector and extract attributes for rasters
## Inputs: vector (.shp or .json) or raster (.tif)
## Outputs: shapefile (.shp) or CSV (.csv)
#######################################################################################
## use sjoin.R or listed bg metadata directions to add clusters->bg values for bg_dems_filtered
library(sf)
library(raster)
library(rgdal)
library(lwgeom)
library(stringr)

## function definition
process_hazard <- function(name, filepath) {
  check <- str_sub(filepath,-3,-1)
  print(check)
  if (check == "shp") {
    hazardPoly <- st_read(filepath)
    print("Read in file")
    # before adding a file, make sure the projection is NAD83; if not, reproject
    hazardPoly <- st_transform(hazardPoly, crs = crs("+init=epsg:4269"))
    print("Projected sf")
    # a polygon file may have topological errors; cannot join without valid topology
    hazardPoly <- st_make_valid(hazardPoly)
    hazardJoined <- NA
    hazardJoined <- st_join(
      zipcodePoly,
      hazardPoly,
      join = st_intersects,
      suffix = c(".x", ".y"),
      left = TRUE,
      largest = TRUE
    )
    print("Extracted")
    # outputs - first is for a shapefile output and second for a csv; comment out whichever is not needed
    st_write(hazardJoined, paste0("Outputs", "/", name, "_sjoin.shp"), driver = "ESRI Shapefile")  # create to a shapefile 
    st_write(hazardJoined, paste0("Outputs", "/", name, "_sjoin.csv"))  # create to a CSV 
    
  }
  
  else if (check == "tif") {
    beginCluster(n=2)
    hazardRaster <- raster(filepath)
    print("Read in file")
    # before adding a file, make sure the projection is NAD83; if not, reproject
    hazardRaster <- projectRaster(hazardRaster, crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
    print("Projected raster")
    hazardJoined <- NA
    hazardJoined <- extract(hazardRaster, zipcodePolySP, fun=mean, na.rm=FALSE, weights=TRUE, 
                            normalizeWeights=TRUE, cellnumbers=FALSE, small=TRUE, df=TRUE, 
                            factors=FALSE, sp=FALSE)
    print("Extracted")
    # adding zcta IDs for merging
    hazardJoined$ZCTA5CE10 <- zipcodePolySP$ZCTA5CE10
    hazardJoined <- merge(zipcodePolySP,hazardJoined,by = "ZCTA5CE10")
    # outputs - first is for a shapefile output and second for a csv; comment out whichever is not needed
    writeOGR(obj = hazardJoined, dsn = "Outputs", layer = paste0(name, "_sjoin"), driver = "ESRI Shapefile") # this is in geographical projection
    write.csv(hazardJoined@data, file = paste0("Outputs", "/", name, "_sjoin.csv"))
    endCluster()
  }
  return(0)
}

## inputs
setwd("/Users/Anaavu/Desktop/Anagha/Extra-Curriculars/ClimateMind")
zipcodePoly <- st_read("zcta_2019/tl_2019_us_zcta510.shp")
zipcodePolySP <- as(zipcodePoly, "Spatial") # for extracting rasters
hazardFiles <- data.frame("Heat" = c("Extreme_Heat", "extreme_heat/ghcn_extreme_heat.shp"), 
                          "Fire" = c("Wildfire", "WHP/WHP.tif"))

## process each file
for (item in hazardFiles) {
  name = as.character(item[1])
  filepath = as.character(item[2])
  process_hazard(name, filepath)
}
#######################################################################################
## Author: Anagha Uppal
## Date: June 6, 2020
## Purpose: Assign area-weighted (largest=assignment) hazard values to each zip code    
## in US using spatial join for vector and extract attributes for rasters
## Inputs: vector (.shp or .json) or raster (.tif)
## Outputs: shapefile (.shp) or CSV (.csv)
#######################################################################################
## use sjoin.R or listed bg metadata directions to add clusters->bg values for bg_dems_filtered
library(sf)
library(raster)
library(rgdal)
library(lwgeom)
library(stringr)

## function definition
process_hazard <- function(name, filepath) {
  check <- str_sub(filepath,-3,-1)
  print(check)
  if (check == "shp") {
    hazardPoly <- st_read(filepath)
    print("Read in file")
    # before adding a file, make sure the projection is NAD83; if not, reproject
    hazardPoly <- st_transform(hazardPoly, crs = crs("+init=epsg:4269"))
    print("Projected sf")
    # a polygon file may have topological errors; cannot join without valid topology
    hazardPoly <- st_make_valid(hazardPoly)
    hazardJoined <- NA
    hazardJoined <- st_join(
      zipcodePoly,
      hazardPoly,
      join = st_intersects,
      suffix = c(".x", ".y"),
      left = TRUE,
      largest = TRUE
    )
    print("Extracted")
    # outputs - first is for a shapefile output and second for a csv; comment out whichever is not needed
    st_write(hazardJoined, paste0("Outputs", "/", name, "_sjoin.shp"), driver = "ESRI Shapefile")  # create to a shapefile 
    st_write(hazardJoined, paste0("Outputs", "/", name, "_sjoin.csv"))  # create to a CSV 
    
  }
  
  else if (check == "tif") {
    beginCluster(n=2)
    hazardRaster <- raster(filepath)
    print("Read in file")
    # before adding a file, make sure the projection is NAD83; if not, reproject
    hazardRaster <- projectRaster(hazardRaster, crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
    print("Projected raster")
    hazardJoined <- NA
    hazardJoined <- extract(hazardRaster, zipcodePolySP, fun=mean, na.rm=FALSE, weights=TRUE, 
                            normalizeWeights=TRUE, cellnumbers=FALSE, small=TRUE, df=TRUE, 
                            factors=FALSE, sp=FALSE)
    print("Extracted")
    # adding zcta IDs for merging
    hazardJoined$ZCTA5CE10 <- zipcodePolySP$ZCTA5CE10
    hazardJoined <- merge(zipcodePolySP,hazardJoined,by = "ZCTA5CE10")
    # outputs - first is for a shapefile output and second for a csv; comment out whichever is not needed
    writeOGR(obj = hazardJoined, dsn = "Outputs", layer = paste0(name, "_sjoin"), driver = "ESRI Shapefile") # this is in geographical projection
    write.csv(hazardJoined@data, file = paste0("Outputs", "/", name, "_sjoin.csv"))
    endCluster()
  }
  return(0)
}

## inputs
setwd("/Users/Anaavu/Desktop/Anagha/Extra-Curriculars/ClimateMind")
zipcodePoly <- st_read("zcta_2019/tl_2019_us_zcta510.shp")
zipcodePolySP <- as(zipcodePoly, "Spatial") # for extracting rasters
hazardFiles <- data.frame("Heat" = c("Extreme_Heat", "extreme_heat/ghcn_extreme_heat.shp"), 
                          "Fire" = c("Wildfire", "WHP/WHP.tif"))

## process each file
for (item in hazardFiles) {
  name = as.character(item[1])
  filepath = as.character(item[2])
  process_hazard(name, filepath)
}
