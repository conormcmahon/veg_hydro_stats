


library(tidyverse)
library(raster)
library(here)
library(caret)
library(snow)
library(randomForest)
library(rlist)


feb <- raster("D:/SERDP/Vandenberg/S2/2018_Pheno/Stacked/20180216_output.tif", band=10)
apr <- raster("D:/SERDP/Vandenberg/S2/2018_Pheno/Stacked/20180422_output.tif", band=10)
jul <- raster("D:/SERDP/Vandenberg/S2/2018_Pheno/Stacked/20180706_output.tif", band=10)
sep <- raster("D:/SERDP/Vandenberg/S2/2018_Pheno/Stacked/20180914_output.tif", band=10)
nov <- raster("D:/SERDP/Vandenberg/S2/2018_Pheno/Stacked/20181103_output.tif", band=10)
dec <- raster("D:/SERDP/Vandenberg/S2/2018_Pheno/Stacked/20181228_output.tif", band=10)

# Load and Resample DEM
source("D:/SERDP/Pendleton/R/fusion_analysis/aggregate_data_to_new_plot.R")
dem <- raster("D:/SERDP/Vandenberg/hydrology/NHDPLUS_H_1806_HU4_RASTER/HRNHDPlusRasters1806/elev_cm.tif")
dem_resamp <- aggregate_custom(input_raster=dem, target_raster=feb, filename="D:/SERDP/Vandenberg/hydrology/dem_resamp.tif")
# Generate Slope Raster
slope <- terrain(dem, opt="slope", neighbors=8, filename="D:/SERDP/Vandenberg/hydrology/slope.tif")

# Stack data, prepare for RF tests
alldata_vand <- stack(feb,
                 apr,
                 jul,
                 sep,
                 nov,
                 dec,
                 slope)
# Subset because full scene is too big to process
alldata_vand <- crop(alldata_vand, extent(c(714400, 750900, 3811200, 3860500)))
alldata_vand_df <- as.data.frame(alldata_vand, xy=TRUE)
# Because the old RF model was built with different dates, temporary hacky approach to fixing name misalignment:
oldnames <- c("x", "y", "X02_19_2016", "X04_19_2016", "X07_28_2016", "X09_06_2016", "X11_05_2016", "X12_25_2016", "slope")
names(alldata_vand_df) <- oldnames

# Run Predictions
predictions_vand <- raster::predict(modFit_rf, newdata=alldata_vand_df)
predictions_vand_num <- 1:length(predictions_vand)
predictions_vand_num[predictions_vand=="Riparian"] <- 1 
predictions_vand_num[predictions_vand=="Chaparral"] <- 2
predictions_vand_num[predictions_vand=="Grassland"] <- 3
predictions_vand_num[predictions_vand=="Turf"] <- 4
predictions_vand_num[predictions_vand=="Suburb"] <- 5
predictions_vand_num[predictions_vand=="Pavement"] <- 6
predictions_vand_num[predictions_vand=="Water"] <- 7
predictions_vand_df <- alldata_vand_df %>% drop_na() %>% dplyr::select(1,2)
predictions_vand_df$class <- predictions_vand_num
predictions_vand_raster <- rasterFromXYZ(predictions_vand_df)
crs(predictions_vand_raster) <- crs(alldata)
# Output Final Raster
writeRaster(predictions_vand_raster, "classes_vand.tif")