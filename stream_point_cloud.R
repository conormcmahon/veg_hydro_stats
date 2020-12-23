
library(raster)
library(rgdal)
library(sp)
library(spatstat)

# Load Raster
flow_raster <- raster("flowline_cropbox/flowlines_raster.tif")

# Convert to Points
flow_points <- as(flow_raster, "SpatialPointsDataFrame")

# Load DEM
dem <- raster("D:/SERDP/Pendleton/LiDAR/DEM/dem_mosaic.tif")
# for some reason, this always loads with the wrong CRS - correcting it:
crs(dem) <- crs(flow_points)
# Get Height at Flowlines
flow_points_dem <- raster::extract(dem, flow_points_data, sp=TRUE)

# Save Points to CSV
flow_df <- data.frame(x=coordinates(flow_points_dem)[,1], y=coordinates(flow_points_dem)[,2], flow_points_dem@data)
names(flow_df) <- c("easting","northing","stream_order","elevation")
write.csv(flow_df, "flowlines.csv")