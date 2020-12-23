

library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(foreign)

# subset(ogrDrivers(), grepl("GDB", name)) # print which drivers are available?

# Flowlines Filename
fc_filename <- "NHDPLUS_H_1807_HU4_GDB/NHDPLUS_H_1807_HU4_GDB.gdb"  # Name of NHD+ Vector Database
fc_list <- ogrListLayers(fc_filename) # Get names of all features within database
# 'Value-Added Data' Filename (includes stream orders)

# Load Vector Data
fc <- readOGR(dsn=fc_filename, layer="NHDFlowline")
# For some reason, we can't directly load the VAA vector file from the database with readOGR, because it doesn't contain geometry - just an attribute table
# Need to convert to a .dbf file and read that instead, which loads as a normal data frame
vaa <- read.dbf("flowline_cropbox/test.dbf")

# Load Raster Data
sentinel <- stack("04_19_2016_sub.tif")

# Subset Area
extent_ll <- extent(projectExtent(sentinel, crs(fc)))
fc_sub <- crop(fc, extent_ll)

# Combine VAA (including stream order) with geometry
fc_sub_vaa <- merge(fc_sub, vaa)
writeOGR("flowline_cropbox", layer="flowlines_vaa", driver="ESRI Shapefile")

# Extract Distances
#raster_points <- as(sentinel[[1]], "SpatialPoints")
#raster_points_ll <- spTransform(raster_points, crs(fc_sub))
#stream_dists <- gDistance(raster_points_ll, fc_sub, byid=TRUE)