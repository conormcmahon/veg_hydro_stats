

library(tidyverse)
library(raster)
library(sp)
library(rgdal)
library(here)
library(caret)
library(snow)
library(randomForest)
library(rlist)
library(tidyverse)

pheno <- stack(raster("D:/SERDP/Pendleton/Sentinel2/214b/stacked_10m/02_09_2016.tif", band=10),
               raster("D:/SERDP/Pendleton/Sentinel2/214b/stacked_10m/02_19_2016.tif", band=10),
               raster("D:/SERDP/Pendleton/Sentinel2/214b/stacked_10m/04_19_2016.tif", band=10),
               raster("D:/SERDP/Pendleton/Sentinel2/214b/stacked_10m/07_28_2016.tif", band=10),
               raster("D:/SERDP/Pendleton/Sentinel2/214b/stacked_10m/09_06_2016.tif", band=10),
               raster("D:/SERDP/Pendleton/Sentinel2/214b/stacked_10m/10_06_2016.tif", band=10),
               raster("D:/SERDP/Pendleton/Sentinel2/214b/stacked_10m/11_05_2016.tif", band=10),
               raster("D:/SERDP/Pendleton/Sentinel2/214b/stacked_10m/11_25_2016.tif", band=10),
               raster("D:/SERDP/Pendleton/Sentinel2/214b/stacked_10m/12_25_2016.tif", band=10))
               
lidar <- stack(raster("D:/SERDP/Pendleton/LiDAR/CHM/chm_10m_utm.tif", band=1),
               raster("D:/SERDP/Pendleton/LiDAR/DEM/dem_mosaic_10m.tif", band=1),
               raster("D:/SERDP/Pendleton/LiDAR/DEM/slope_10m.tif", band=1),
               raster("D:/SERDP/Pendleton/R/veg_hydro_stats/streamdist_data_coarse.tif", band=1),
               raster("D:/SERDP/Pendleton/R/veg_hydro_stats/streamdist_data_coarse.tif", band=2),
               raster("D:/SERDP/Pendleton/R/veg_hydro_stats/streamdist_data_coarse.tif", band=3))
# Subset LiDAR input data 
lidar <- lidar[[c(1,3,4)]]
lidar[[3]][lidar[[3]] > 100] <- 100
lidar[[3]][lidar[[3]] < 0] <- 0


pheno <- crop(pheno, extent(lidar))

alldata <- stack(pheno, lidar)
names(alldata) <- c("X02_09_2016", 
                    "X02_19_2016", 
                    "X04_19_2016", 
                    "X07_28_2016", 
                    "X09_06_2016", 
                    "X10_06_2016", 
                    "X11_05_2016", 
                    "X11_25_2016", 
                    "X12_25_2016", 
                    "chm", 
                    "slope", 
                    "stream_height")

segments <- readOGR("D:/SERDP/Pendleton/R/veg_hydro_stats/rf_training_polygons/rf_training.shp")


# Generate Ground Truth Segment Groups
# --- Riparian ---
riparian_segments <- segments[segments$level_2=="riparian",]
riparian_df <- raster::extract(alldata, riparian_segments, cellnumbers=TRUE)
riparian_df <- as.data.frame(list.rbind(riparian_df))
riparian_df <- cbind(riparian_df, coordinates(alldata)[riparian_df[,1],])
riparian_df$class <- "Riparian"
# --- Water ---
water_segments <- segments[segments$level_1=="water",]
water_df <- raster::extract(alldata, water_segments, cellnumbers=TRUE)
water_df <- as.data.frame(list.rbind(water_df))
water_df <- cbind(water_df, coordinates(alldata)[water_df[,1],])
water_df$class <- "Water"
# --- Chaparral ---
chaparral_segments <- segments[segments$level_3=="chaparral" | segments$level_3=="north_slope",]
chaparral_df <- raster::extract(alldata, chaparral_segments, cellnumbers=TRUE)
chaparral_df <- as.data.frame(list.rbind(chaparral_df))
chaparral_df <- cbind(chaparral_df, coordinates(alldata)[chaparral_df[,1],])
chaparral_df$class <- "Chaparral"
# --- Grassland ---
grassland_segments <- segments[segments$level_3=="grassland",]
grassland_df <- raster::extract(alldata, grassland_segments, cellnumbers=TRUE)
grassland_df <- as.data.frame(list.rbind(grassland_df))
grassland_df <- cbind(grassland_df, coordinates(alldata)[grassland_df[,1],])
grassland_df$class <- "Grassland"
# --- Turf ---
turf_segments <- segments[segments$level_3=="turf",]
turf_df <- raster::extract(alldata, turf_segments, cellnumbers=TRUE)
turf_df <- as.data.frame(list.rbind(turf_df))
turf_df <- cbind(turf_df, coordinates(alldata)[turf_df[,1],])
turf_df$class <- "Turf"
# --- Suburb ---
suburb_segments <- segments[segments$level_3=="suburb",]
suburb_df <- raster::extract(alldata, suburb_segments, cellnumbers=TRUE)
suburb_df <- as.data.frame(list.rbind(suburb_df))
suburb_df <- cbind(suburb_df, coordinates(alldata)[suburb_df[,1],])
suburb_df$class <- "Suburb"
# --- Pavement ---
pavement_segments <- segments[segments$level_2=="paving",]
pavement_df <- raster::extract(alldata, pavement_segments, cellnumbers=TRUE)
pavement_df <- as.data.frame(list.rbind(pavement_df))
pavement_df <- cbind(pavement_df, coordinates(alldata)[pavement_df[,1],])
pavement_df$class <- "Pavement"
# --- All Label Data ---
label_df <- rbind(riparian_df, water_df, chaparral_df, grassland_df, turf_df, suburb_df, pavement_df)
label_df <- label_df %>% 
  dplyr::select(2:ncol(label_df)) %>%
  drop_na()
label_df$class <- factor(label_df$class, 
                            levels = c("Riparian", "Chaparral", "Grassland", "Turf", "Suburb", "Pavement", "Water"))

# --- Visualize Variation by Class ----
# Slope Plot
slope_plot <- ggplot() + 
  geom_boxplot(data = label_df, aes(y=slope, x=class),
               fill = c("darkorange3", "chartreuse4", "darkseagreen", "chartreuse", "darkorchid1", "darkgrey", "blue"), 
               notch=TRUE) + 
  theme_bw() + 
  labs(x = "Class", y = "Ground Slope (radians)",
       title = "Ground Slope by Land Cover Type")
ggsave(plot=slope_plot, width=8, height = 4, filename=here::here("output_imagery","slope.png"))
# CHM Plot
chm_plot <- ggplot() + 
  geom_boxplot(data = label_df, aes(y=chm/3.28, x=class),
               fill = c("darkorange3", "chartreuse4", "darkseagreen", "chartreuse", "darkorchid1", "darkgrey", "blue"), 
               notch=TRUE) + 
  theme_bw() + 
  labs(x = "Class", y = "Canopy Height (m)",
       title = "Canopy Height by Land Cover Type")
ggsave(plot=chm_plot, width=8, height = 4, filename=here::here("output_imagery","chm.png"))
# Stream Height Plot
stream_height_plot <- ggplot() + 
  geom_boxplot(data = label_df, aes(y=stream_height/3.28, x=class),
               fill = c("darkorange3", "chartreuse4", "darkseagreen", "chartreuse", "darkorchid1", "darkgrey", "blue"), 
               notch=TRUE) + 
  theme_bw() + 
  labs(x = "Class", y = "Height Over Nearest Stream (m)",
       title = "Local Height Over Stream by Land Cover Type")
ggsave(plot=stream_height_plot, width=8, height = 4, filename=here::here("output_imagery","stream_height.png"))

# Phenology Plots
pheno_df_long <- label_df %>% pivot_longer((1:9), names_to="date", values_to="ndvi")
pheno_df_long$day_of_year <- 0
pheno_df_long[pheno_df_long$date=="X02_09_2016",]$day_of_year <- 40
pheno_df_long[pheno_df_long$date=="X02_19_2016",]$day_of_year <- 50
pheno_df_long[pheno_df_long$date=="X04_19_2016",]$day_of_year <- 110
pheno_df_long[pheno_df_long$date=="X07_29_2016",]$day_of_year <- 221
pheno_df_long[pheno_df_long$date=="X09_06_2016",]$day_of_year <- 250
pheno_df_long[pheno_df_long$date=="X10_06_2016",]$day_of_year <- 280
pheno_df_long[pheno_df_long$date=="X11_05_2016",]$day_of_year <- 310
pheno_df_long[pheno_df_long$date=="X11_25_2016",]$day_of_year <- 330
pheno_df_long[pheno_df_long$date=="X12_25_2016",]$day_of_year <- 360
pheno_stats <- pheno_df_long %>% 
  group_by(class, day_of_year) %>%
  summarise(p_05=quantile(ndvi, probs=c(0.05)), 
            p_25=quantile(ndvi, probs=c(0.25)),
            p_50=quantile(ndvi, probs=c(0.50)),
            p_75=quantile(ndvi, probs=c(0.75)),
            p_95=quantile(ndvi, probs=c(0.95)))
# Natural Vegetation Phenology
veg_pheno_plot <- ggplot(data=pheno_stats[pheno_stats$class %in% factor(c("Riparian","Chaparral","Grassland")),], aes(x=day_of_year)) + 
  geom_line(aes(y=p_05), linetype="dashed", color="red") + 
  geom_line(aes(y=p_25), color = "cyan3") + 
  geom_line(aes(y=p_50)) + 
  geom_line(aes(y=p_75), color = "cyan3") + 
  geom_line(aes(y=p_95), linetype="dashed", color="red") + 
  geom_hline(yintercept=0, color="gray") +
  facet_wrap(~class, ncol=1) + 
  labs(x = "Day of Year",
       y = "NDVI") + 
  ggtitle("Greenness Phenology by Natural Vegetation Type")
ggsave(plot=veg_pheno_plot, filename=here::here("output_imagery","veg_phenology.png"), 
       width=8, height=7)
# Natural Vegetation Phenology
nonveg_pheno_plot <- ggplot(data=pheno_stats[!(pheno_stats$class %in% factor(c("Riparian","Chaparral","Grassland"))),], aes(x=day_of_year)) + 
  geom_line(aes(y=p_05), linetype="dashed", color="red") + 
  geom_line(aes(y=p_25), color = "cyan3") + 
  geom_line(aes(y=p_50)) + 
  geom_line(aes(y=p_75), color = "cyan3") + 
  geom_line(aes(y=p_95), linetype="dashed", color="red") + 
  geom_hline(yintercept=0, color="gray") +
  facet_wrap(~class, ncol=1) + 
  labs(x = "Day of Year",
       y = "NDVI") + 
  ggtitle("Greenness Phenology by Unnatural Land Cover Type")
ggsave(plot=nonveg_pheno_plot, filename=here::here("output_imagery","nonveg_phenology.png"), 
       width=8, height=7)



# ------ Generate Training Data ------
# --- Riparian ---
riparian_df <- label_df[label_df$class=="Riparian",]
riparian_training_indices <- sample(1:nrow(riparian_df),500)
train <- riparian_df[riparian_training_indices,]
leftover <- riparian_df[-riparian_training_indices,]
# --- Chaparral ---
chaparral_df <- label_df[label_df$class=="Chaparral",]
chaparral_training_indices <- sample(1:nrow(chaparral_df),500)
train <- rbind(train, chaparral_df[chaparral_training_indices,])
leftover <- rbind(leftover, chaparral_df[-chaparral_training_indices,])
# --- Grassland ---
grassland_df <- label_df[label_df$class=="Grassland",]
grassland_training_indices <- sample(1:nrow(grassland_df),500)
train <- rbind(train, grassland_df[grassland_training_indices,])
leftover <- rbind(leftover, grassland_df[-grassland_training_indices,])
# --- Suburb ---
suburb_df <- label_df[label_df$class=="Suburb",]
suburb_training_indices <- sample(1:nrow(suburb_df),500)
train <- rbind(train, suburb_df[suburb_training_indices,])
leftover <- rbind(leftover, suburb_df[-suburb_training_indices,])
# --- Pavement ---
pavement_df <- label_df[label_df$class=="Pavement",]
pavement_training_indices <- sample(1:nrow(pavement_df),500)
train <- rbind(train, pavement_df[pavement_training_indices,])
leftover <- rbind(leftover, pavement_df[-pavement_training_indices,])
# --- Turf ---
turf_df <- label_df[label_df$class=="Turf",]
turf_training_indices <- sample(1:nrow(turf_df),500)
train <- rbind(train, turf_df[turf_training_indices,])
leftover <- rbind(leftover, turf_df[-turf_training_indices,])
# --- Water ---
water_df <- label_df[label_df$class=="Water",]
water_training_indices <- sample(1:nrow(water_df),500)
train <- rbind(train, water_df[water_training_indices,])
leftover <- rbind(leftover, water_df[-water_training_indices,])

# Generate some validation data
#   to keep it balanced, we'll figure out the minimum number of remaining samples across all classes:
remaining_per_class <- leftover %>% group_by(class) %>% summarise(num=n())
min_remaining <- min(remaining_per_class$num)
# --- Riparian ---
riparian_df <- leftover[leftover$class=="Riparian",]
riparian_training_indices <- sample(1:nrow(riparian_df),min_remaining)
valid <- riparian_df[riparian_training_indices,]
leftover_final <- riparian_df[-riparian_training_indices,]
# --- Chaparral ---
chaparral_df <- leftover[leftover$class=="Chaparral",]
chaparral_training_indices <- sample(1:nrow(chaparral_df),min_remaining)
valid <- rbind(valid, chaparral_df[chaparral_training_indices,])
leftover_final <- rbind(leftover_final, chaparral_df[-chaparral_training_indices,])
# --- Grassland ---
grassland_df <- leftover[leftover$class=="Grassland",]
grassland_training_indices <- sample(1:nrow(grassland_df),min_remaining)
valid <- rbind(valid, grassland_df[grassland_training_indices,])
leftover_final <- rbind(leftover_final, grassland_df[-grassland_training_indices,])
# --- Suburb ---
suburb_df <- leftover[leftover$class=="Suburb",]
suburb_training_indices <- sample(1:nrow(suburb_df),min_remaining)
valid <- rbind(valid, suburb_df[suburb_training_indices,])
leftover_final <- rbind(leftover_final, suburb_df[-suburb_training_indices,])
# --- Pavement ---
pavement_df <- leftover[leftover$class=="Pavement",]
pavement_training_indices <- sample(1:nrow(pavement_df),min_remaining)
valid <- rbind(valid, pavement_df[pavement_training_indices,])
leftover_final <- rbind(leftover_final, pavement_df[-pavement_training_indices,])
# --- Turf ---
turf_df <- leftover[leftover$class=="Turf",]
turf_training_indices <- sample(1:nrow(turf_df),min_remaining)
valid <- rbind(valid, turf_df[turf_training_indices,])
leftover_final <- rbind(leftover_final, turf_df[-turf_training_indices,])
# --- Water ---
water_df <- leftover[leftover$class=="Water",]
water_training_indices <- sample(1:nrow(water_df),min_remaining)
valid <- rbind(valid, water_df[water_training_indices,])
leftover_final <- rbind(leftover_final, water_df[-water_training_indices,])


# --- Train RF Model ---
modFit_rf <- train(as.factor(class) ~ X02_09_2016+X02_19_2016+X04_19_2016+X07_28_2016+X09_06_2016+X10_06_2016+X11_05_2016+X11_25_2016+X12_25_2016+chm+slope+stream_height, 
                   method="rf", data = train)
# --- Validation  Model (Balanced) ---
validation_results <- raster::predict(modFit_rf, newdata=valid)
# Confusion Matrix with Validation Data
confusionMatrix(validation_results, as.factor(valid$class))
# --- Validation Model (All) ---
validation_results_all <- raster::predict(modFit_rf, newdata=leftover)
confusionMatrix(validation_results_all, as.factor(leftover$class))
# --- Variable Importance ---
varImp(modFit_rf)

# --- Generate Output Raster ---
alldata_df <- as.data.frame(alldata, xy=TRUE)
predictions_all <- raster::predict(modFit_rf, newdata=alldata_df)
predictions_all_num <- 1:length(predictions_all)
predictions_all_num[predictions_all=="Riparian"] <- 1 
predictions_all_num[predictions_all=="Chaparral"] <- 2
predictions_all_num[predictions_all=="Grassland"] <- 3
predictions_all_num[predictions_all=="Turf"] <- 4
predictions_all_num[predictions_all=="Suburb"] <- 5
predictions_all_num[predictions_all=="Pavement"] <- 6
predictions_all_num[predictions_all=="Water"] <- 7
predictions_df <- alldata_df %>% drop_na() %>% dplyr::select(1,2)
predictions_df$class <- predictions_all_num
predictions_raster <- rasterFromXYZ(predictions_df)
crs(predictions_raster) <- crs(alldata)
# Mask output to prevent classification outside bounds of data availability
zero_height_mask <- crop(lidar[[1]] > 0, extent(predictions_raster))
zero_slope_mask <- crop(lidar[[3]] > 0, extent(predictions_raster))
water_mask <- predictions_raster == 7
total_mask <- (zero_height_mask + zero_slope_mask + water_mask) > 0
predictions_raster_masked <- predictions_raster*total_mask
# Output Final Raster
writeRaster(predictions_raster_masked, "classes.tif")

# Generate 30m Raster at Landsat Resolution

source(here::here("..","fusion_analysis","aggregate_data_to_new_plot.R"))
mesma <- raster(here::here("..","..","MESMA","LC08_L1TP_040037_20140509_20170307_01_T1_sr__envi_crop.envi_SMA_20190319T14H24M36S"))
predictions_raster_resamp <- aggregate_custom(predictions_raster_masked, mesma, "classes_resamp.tif")