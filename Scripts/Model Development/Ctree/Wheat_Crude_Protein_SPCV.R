#Name: Tasin Siraj
#Date: 18/01/2024
#RStudio/2023.03.1+446

library(dplyr)
library(ggplot2)
library(sf)
library(caret)
library(randomForest)
library(spatialsample)
library(vip)
library(gridExtra)

rm(list = ls())
set.seed(3944410)

dataset <- read.csv("Analysis_21-22_Spatial.csv") %>% 
  filter(N_Fertilization_kg_ha != 0 & Pgl_Bez != 'ohne N')
wheat <- dataset %>%
  filter(CropType_Long == "Winter Soft Wheat")

wheat <- wheat %>%
  rename( Latitude =GPS_noerdlB,
          Longitude =GPS_oestlL) 

wheat_all <-  wheat %>% select(7:132) %>%
  select(-N_Efficiency, -CropType_Long, -Variety, -Year, -Real_sample,
         -Soiltype, -Soiltype_Short, -Soil.Group, -Na_eff, -NH4_N, -NO3_N )

class_cols <- sapply(wheat_all, is.character)
wheat_all <- wheat_all %>%
  mutate_at(vars(which(class_cols)), as.factor)

Strategy_var <- c("urea", "NI", "UI", "N_amount_rel", "No_of_applications",
                  "T1", "T2", "T3", "T4", "T5", "UI_T1", "UI_T2", "UI_T3",
                  "UI_T4", "UI_T5", "NI_T1", "NI_T2", "NI_T3", "NI_T4", "NI_T5")
wheat_all <- wheat_all %>%
  mutate_at(vars(Strategy_var), as.factor)

wheat_all <- wheat_all %>% select(-N_amount_rel, -NI_T5)

rows_with_na_wheat <- wheat_all[!complete.cases(wheat_all), ]
wheat_all <- na.omit(wheat_all)

spatial_df <- st_as_sf(wheat_all, coords = c("Longitude", "Latitude"), crs = 4326)

length(unique(spatial_df$geometry))
set.seed(1)
cluster_folds <- spatial_clustering_cv(spatial_df, v = 5, cluster_function = c("kmeans"))
cluster_folds

geojson_path <- "D:/Zalf/Dataset/Analysis/Update 2/Data/germany.json"
germany <- st_read(geojson_path, quiet = TRUE)

fold_1 <- assessment(cluster_folds$splits[[1]])
fold_2 <- assessment(cluster_folds$splits[[2]])
fold_3 <- assessment(cluster_folds$splits[[3]])
fold_4 <- assessment(cluster_folds$splits[[4]])
fold_5 <- assessment(cluster_folds$splits[[5]])

spatial_folds <- ggplot() +
  geom_sf(data = germany, fill = "white", color = "gray50") +
  geom_sf(data = fold_1, color = "firebrick2", size = 2, alpha =0.6) +
  geom_sf(data = fold_2, color = "blue", size = 2, alpha =0.6) +
  geom_sf(data = fold_3, color = "green", size = 2, alpha =0.6) +
  geom_sf(data = fold_4, color = "magenta", size = 2, alpha =0.6) +
  geom_sf(data = fold_5, color = "purple", size = 2, alpha =0.6) +
  labs(title = "Sample Locations", x = "Longitude", y = "Latitude")

spatial_folds <- grid.arrange(autoplot(cluster_folds), spatial_folds, ncol = 2)

predictions <- list()
actual_values <- list()
fold_r <- numeric(length(cluster_folds$splits))

set.seed(42)
for (fold in 1:length(cluster_folds$splits)) {
  train_data <- analysis(cluster_folds$splits[[fold]])
  test_data <- assessment(cluster_folds$splits[[fold]])
  
  train_data$geometry <- NULL
  test_data$geometry <- NULL
  
  ctree_control <- ctree_control(testtype = "Univariate", minsplit = 10, maxdepth = 3)
  model <- ctree(RP_Go ~ ., data = train_data, controls = ctree_control)
  
  pred <- predict(model, newdata = test_data)
  predictions[[fold]] <- pred
  actual_values[[fold]] <- test_data$RP_Go
  
  fold_r[fold] <- cor(pred, test_data$RP_Go)
}

all_predictions <- unlist(predictions)
all_actual_values <- unlist(actual_values)
plot(all_predictions, all_actual_values)

combined_r <- cor(all_predictions, all_actual_values) %>% round(., 4)
combined_r

fold_r

mean(fold_r)
