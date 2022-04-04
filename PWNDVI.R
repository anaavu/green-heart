library(dplyr)
# PWNDVI
setwd("I:/Backups/GIS/PWNDVI")
tractndvi <- read.csv("Monthly_Tract_2016_PWNDVI.csv", header=TRUE)[-1,]
countyndvi <- read.csv("Monthly_County_2016_PWNDVI.csv")
tractndvi$July_2016 <- as.numeric(tractndvi$July_2016)
tractndvi$countyfips <- substr(tractndvi$FIPS,1,5)
county_julyndvimean <- tractndvi %>%
  group_by(countyfips) %>%
  summarise(mean_july_ndvi = mean(July_2016,na.rm = TRUE))
tractndvi <- left_join(tractndvi, county_julyndvimean, c("countyfips" = "countyfips"))
tractndvi$julydev <- tractndvi$July_2016 - tractndvi$mean_july_ndvi
write.csv(tractndvi, "tract_july_deviation.csv")

library(raster)
library(rgdal)

fgdb <- "PWNDVI_2016_Monthly_Layer_Pkg/p20/census_tract_ndvi_stats.gdb"
ogrListLayers(fgdb)
fc <- readOGR(dsn=fgdb,layer="Census_Tract_PWNDVI_2016_Monthly")
summary(fc)
#plot(fc)

fc <- merge(fc, tractndvi[ , c("FIPS", "July_2016", "mean_july_ndvi", "julydev")], by="FIPS")
shapefile(fc, "tracts_shp.shp", overwrite=TRUE)


# Traditional NDVI
setwd("I:/Backups/GIS/PWNDVI")
tractndvi <- read.csv("Traditional_Mean_NDVI_Tracts_2016.csv", header=TRUE, 
                      colClasses = c("numeric",rep("character",5),rep("numeric",14)))
county_julyndvimean <- tractndvi %>%
  group_by(STCOFIPS) %>%
  summarise(mean_july_ndvi = mean(July_2016,na.rm = TRUE))
tractndvi <- left_join(tractndvi, county_julyndvimean, c("STCOFIPS" = "STCOFIPS"))
tractndvi$julydev <- tractndvi$July_2016 - tractndvi$mean_july_ndvi
write.csv(tractndvi, "tract_july_deviation_traditional.csv")

library(raster)
library(rgdal)

fgdb <- "Tract_Level_Mean_Traditional_NDVI_2016/p20/census_tract_ndvi_stats.gdb"
ogrListLayers(fgdb)
fc <- readOGR(dsn=fgdb,layer="Traditional_Mean_NDVI_Tracts_2016")
summary(fc)
#plot(fc)

fc <- merge(fc, tractndvi[ , c("FIPS", "July_2016", "mean_july_ndvi", "julydev")], by="FIPS")
shapefile(fc, "tracts_shp_traditional.shp", overwrite=TRUE)

