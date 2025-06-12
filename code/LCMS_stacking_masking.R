#####
#Plots Planes Pixels Project
#Stack annual change and cause of change rasters and clip to AOIs
#Annual change and annual cause of change rasters from Landscape Change Monitoring System (LCMS)
#LCMS v2023.9
#downloaded 15 October 2025 from https://data.fs.usda.gov/geodata/rastergateway/LCMS/

#####
#---SETUP
#set basepath
# basepath<-"E:/Projects/PlotsPlanesPixels/Data/Spatial/LCMS"
# setwd(basepath)

# list.of.packages <- c("tidyverse","sf","terra")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(sf)
library(terra)

#####
#---DATA INPUTS

#AOIs as shapefiles
#two AOIs, Malhuer and Klamath
aoi <-  sf::st_read("data/ADS_data/shapefiles/psme_fp.shp")

#1985-2023 LCMS 
#annual change and annual cause of change
#individual year rasters so load as lists
#change.directory <- "E:/Projects/PlotsPlanesPixels/Data/Spatial/LCMS/Change_Annual"
cause.directory <- "C:/Users/DanielPerret/Box/PlotsPlanesPixels/data/LCMS/CONUS_annual"
cause.list <- list.files(path = cause.directory,
                              pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

#change.rasters <- rast(change.list)
cause.rasters <- rast(cause.list)

#####
#---PROCESSING

#transform shapfile crs as needed
aoi <- st_transform(aoi, crs = st_crs(cause.rasters))


#crop and mask rasters to each aoi
crop.rast <- crop(cause.rasters, aoi, snap="near", mask=TRUE, touches=TRUE, extend=FALSE)

#change names of each layer to year
cause.names <- paste("cause",seq(from=2010,to=2020),sep="_")

names(crop.rast) <- cause.names

#get raster attribute tables
cause.rat <- levels(crop.rast)[[1]]

#and export
writeRaster(crop.rast,
            "C:/Users/DanielPerret/Box/PlotsPlanesPixels/data/LCMS/psme_causeofchange_annual.tif",
            datatype = "INT1U",
            overwrite=TRUE)
