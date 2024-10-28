#####
#Plots Planes Pixels Project
#Stack annual change and cause of change rasters and clip to AOIs
#Annual change and annual cause of change rasters from Landscape Change Monitoring System (LCMS)
#LCMS v2023.9
#downloaded 15 October 2025 from https://data.fs.usda.gov/geodata/rastergateway/LCMS/

#####
#---SETUP
#set basepath
basepath<-"E:/Projects/PlotsPlanesPixels/Data/Spatial/LCMS"
setwd(basepath)

list.of.packages <- c("tidyverse","sf","terra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(sf)
library(terra)

#####
#---DATA INPUTS

#AOIs as shapefiles
#two AOIs, Malhuer and Klamath
malh.aoi <-  st_read("E:/Projects/PlotsPlanesPixels/Data/Spatial/MPB2_malh_fp.shp")
klam.aoi <-  st_read("E:/Projects/PlotsPlanesPixels/Data/Spatial/mpb_allyears_fp.shp")

#1985-2023 LCMS 
#annual change and annual cause of change
#individual year rasters so load as lists
change.directory <- "E:/Projects/PlotsPlanesPixels/Data/Spatial/LCMS/Change_Annual"
cause.directory <- "E:/Projects/PlotsPlanesPixels/Data/Spatial/LCMS/Cause_of_Change_Annual"
change.list <- list.files(path = change.directory,
                              pattern = "*.tif$", recursive = TRUE, full.names = TRUE)
cause.list <- list.files(path = cause.directory,
                              pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

change.rasters <- rast(change.list)
cause.rasters <- rast(cause.list)

#####
#---PROCESSING

#transform shapfile crs as needed
malh.aoi <- st_transform(malh.aoi, crs = st_crs(change.rasters))
klam.aoi <- st_transform(klam.aoi, crs = st_crs(change.rasters))

#crop and mask rasters to each aoi
malh.change <- crop(change.rasters, malh.aoi, snap="near", mask=TRUE, touches=TRUE, extend=FALSE)
malh.cause <- crop(cause.rasters, malh.aoi, snap="near", mask=TRUE, touches=TRUE, extend=FALSE)

klam.change <- crop(change.rasters, klam.aoi, snap="near", mask=TRUE, touches=TRUE, extend=FALSE)
klam.cause <- crop(cause.rasters, klam.aoi, snap="near", mask=TRUE, touches=TRUE, extend=FALSE)

#change names of each layer to year
change.names <- paste("change",seq(from=1985,to=2023),sep="_")
cause.names <- paste("cause",seq(from=1985,to=2023),sep="_")

names(malh.change) <- change.names
names(malh.cause) <- cause.names

names(klam.change) <- change.names
names(klam.cause) <- cause.names


activeCat(malh.change) <- 1

malh.change@ptr

#get raster attribute tables
change.rat <- levels(malh.change)
cause.rat <- levels(malh.cause)

#and export
writeRaster(malh.change,
            "E:/Projects/PlotsPlanesPixels/Data/Spatial/LCMS/malh_change_annual.tif",
            datatype = "INT1U",
            overwrite=TRUE)

writeRaster(malh.cause,
            "E:/Projects/PlotsPlanesPixels/Data/Spatial/LCMS/malh_cause_of_change_annual.tif",
            datatype = "INT1U",
            overwrite=TRUE)

writeRaster(klam.change,
            "E:/Projects/PlotsPlanesPixels/Data/Spatial/LCMS/klam_change_annual.tif",
            datatype = "INT1U",
            overwrite=TRUE)

writeRaster(klam.cause,
            "E:/Projects/PlotsPlanesPixels/Data/Spatial/LCMS/klam_cause_of_change_annual.tif",
            datatype = "INT1U",
            overwrite=TRUE)

write.csv(change.rat, "E:/Projects/PlotsPlanesPixels/Data/Spatial/LCMS/change_rat.csv")
write.csv(cause.rat, "E:/Projects/PlotsPlanesPixels/Data/Spatial/LCMS/cause_rat.csv")
