#Name: Tasin Siraj
#Date: 18/01/2024
#RStudio/2023.03.1+446

library(dplyr)
library(ggplot2)
library(lattice)
library(caret)
library(sf)
library(randomForest)


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
  select( -N_Efficiency, -CropType_Long, -Variety, -Year, -Real_sample,
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

geojson_path <- "D:/Zalf/Dataset/Analysis/Update 2/Data/germany.json"
germany <- st_read(geojson_path, quiet = TRUE)
df_sf <- st_as_sf(wheat_all, coords = c("Longitude", "Latitude"),
                  crs = st_crs(germany))

ggplot() +
  geom_sf(data = germany, fill = "white", color = "gray50") +
  geom_sf(data = df_sf, color = "firebrick2", size = 2, alpha =0.6) +
  labs(title = "Sample Locations", x = "Longitude", y = "Latitude")

set.seed(42)
unique_locations <- unique(wheat_all[, c("Longitude", "Latitude")])

k=5

folds <- lapply(1:k, function(i) {
  test_indices <- sample(1:nrow(unique_locations), size = floor(nrow(unique_locations) / k))
  train_indices <- setdiff(1:nrow(unique_locations), test_indices)
  
  
  train_indices <- which(wheat_all$Longitude %in% unique_locations$Longitude[train_indices] &
                           wheat_all$Latitude %in% unique_locations$Latitude[train_indices])
  
  test_indices <- which(wheat_all$Longitude %in% unique_locations$Longitude[test_indices] &
                          wheat_all$Latitude %in% unique_locations$Latitude[test_indices])
  
  return(list(train = train_indices, test = test_indices))
})

plots <- list()

for (i in 1:k) {
  test_indices <- unlist(folds[[i]]$test)
  train_indices <- unlist(folds[[i]]$train)
  
  p <- ggplot() +
    geom_sf(data = germany, fill = "white", color = "gray50") +
    labs(title = paste("Fold", i, "Sample Locations"), x = "Longitude", y = "Latitude")
  
  p <- p +
    geom_point(data = wheat_all[test_indices, ], aes(x = Longitude, y = Latitude), color = "red", size = 2) +
    geom_point(data = wheat_all[train_indices, ], aes(x = Longitude, y = Latitude), color = "blue", size = 2)
  
  
  plots[[i]] <- p
  print(p)
}

cv_results <- list()
predictions <- list()
actual_values <- list()
set.seed(42)
for (fold in seq_along(folds)) {
  train_indices <- unlist(folds[[fold]]$train)
  test_indices <- unlist(folds[[fold]]$test)
  
  train_data <- wheat_all[train_indices, ]
  test_data <- wheat_all[test_indices, ]
  train_data$Latitude <- NULL
  train_data$Longitude <- NULL
  
  ctree_control <- ctree_control(testtype = "Univariate", minsplit = 10, maxdepth = 3)
  model <- ctree(RP_Go ~ ., data = train_data, controls = ctree_control)
  pred <- predict(model, newdata = test_data)
  predictions[[fold]] <- pred
  actual_values[[fold]] <- test_data$RP_Go
  
  cv_results[[fold]] <- cor(pred, test_data$RP_Go)
}

all_predictions <- unlist(predictions)
all_actual_values <- unlist(actual_values)

combined_r <- cor(all_predictions, all_actual_values) %>% round(., 4)
combined_r

plot(all_predictions, all_actual_values, main = "Predictions vs Actual Values", xlab = "Predictions", ylab = "Actual Values")
cat("Fold results:\n")
print(cv_results)
cat("Mean r:", mean(unlist(cv_results)), "\n")
