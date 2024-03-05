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

rm(list = ls())
set.seed(3944410)

dataset <- read.csv("Analysis_21-22_Spatial.csv") %>% 
  filter(N_Fertilization_kg_ha != 0 & Pgl_Bez != 'ohne N')

barley <- dataset %>%
  filter(CropType_Long == "Winter Barley")
barley <- barley %>%
  rename( Latitude =GPS_noerdlB,
          Longitude =GPS_oestlL) 

columns_with_all_NAs_barley <- names(barley)[apply(is.na(barley), 2, all)]
barley <- barley[, !colnames(barley) %in% columns_with_all_NAs_barley]
rows_with_na_barley <- barley[!complete.cases(barley), ]

barley_all <-  barley %>% select(6:106) %>%
  select(-RP_Go, -N_Efficiency, -CropType_Long, -Variety, -Year, -Real_sample,
         -Soiltype, -Soiltype_Short, -Soil.Group, -Na_eff, -NH4_N, -NO3_N)

class_cols <- sapply(barley_all, is.character)
barley_all <- barley_all %>%
  mutate_at(vars(which(class_cols)), as.factor)

Strategy_var <- c("urea", "NI", "UI", "N_amount_rel", "No_of_applications",
                  "T1", "T2", "T3", "T4", "T5", "UI_T1", "UI_T2", "UI_T3",
                  "UI_T4", "UI_T5", "NI_T1", "NI_T2", "NI_T3", "NI_T4", "NI_T5")
barley_all <- barley_all %>%
  mutate_at(vars(Strategy_var), as.factor)

barley_all <- barley_all %>% select(-N_amount_rel, -NI_T5, -T4, -T5, -UI_T4, -UI_T5, 
                                    -NI_T4, -NI_T3)

spatial_df <- st_as_sf(barley_all, coords = c("Longitude", "Latitude"), crs = 4326)

geojson_path <- "D:/Zalf/Thesis/Data/Thesis/germany.json"
germany <- st_read(geojson_path, quiet = TRUE)
df_sf <- st_as_sf(barley_all, coords = c("Longitude", "Latitude"),
                  crs = st_crs(germany))

ggplot() +
  geom_sf(data = germany, fill = "white", color = "gray50") +
  geom_sf(data = df_sf, color = "firebrick2", size = 2, alpha =0.6) +
  labs(title = "Sample Locations", x = "Longitude", y = "Latitude")

set.seed(42)
unique_locations <- unique(barley_all[, c("Longitude", "Latitude")])

k=5

folds <- lapply(1:k, function(i) {
  test_indices <- sample(1:nrow(unique_locations), size = floor(nrow(unique_locations) / k))
  train_indices <- setdiff(1:nrow(unique_locations), test_indices)
  
  
  train_indices <- which(barley_all$Longitude %in% unique_locations$Longitude[train_indices] &
                           barley_all$Latitude %in% unique_locations$Latitude[train_indices])
  
  test_indices <- which(barley_all$Longitude %in% unique_locations$Longitude[test_indices] &
                          barley_all$Latitude %in% unique_locations$Latitude[test_indices])
  
  return(list(train = train_indices, test = test_indices))
})

plots <- list()

for (i in 1:k) {
  test_indices <- unlist(folds[[i]]$test)
  train_indices <- unlist(folds[[i]]$train)
  
  p <- ggplot() +
    geom_sf(data = germany, fill = "white", color = "gray50") +
    labs(title = paste("Fold", i), x = "Longitude", y = "Latitude")
  
  p <- p +
    geom_point(data = barley_all[test_indices, ], aes(x = Longitude, y = Latitude), color = "red", size = 2) +
    geom_point(data = barley_all[train_indices, ], aes(x = Longitude, y = Latitude), color = "blue", size = 2)
  
  
  plots[[i]] <- p
  print(p)
}

combined_plot <- plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] +
  plot_layout(ncol = 3)
combined_plot <- combined_plot + 
  plot_annotation(title = "Random Spatial Locations Across Folds for Winter Barley")
combined_plot

ggsave("Barley_Random_Spatial.png", combined_plot, width = 16, height = 9, units = "in", dpi = 300)


cv_results <- list()
predictions <- list()
actual_values <- list()
set.seed(42)
for (fold in seq_along(folds)) {
  train_indices <- unlist(folds[[fold]]$train)
  test_indices <- unlist(folds[[fold]]$test)
  
  train_data <- barley_all[train_indices, ]
  test_data <- barley_all[test_indices, ]
  train_data$Latitude <- NULL
  train_data$Longitude <- NULL
  
  ctree_control <- ctree_control(testtype = "Univariate", minsplit = 10, maxdepth = 3)
  model <- ctree(Ert_dt_ha ~ ., data = train_data, controls = ctree_control)
  
  pred <- predict(model, newdata = test_data)
  predictions[[fold]] <- pred
  actual_values[[fold]] <- test_data$Ert_dt_ha
  
  cv_results[[fold]] <- cor(pred, test_data$Ert_dt_ha)
}

all_predictions <- unlist(predictions)
all_actual_values <- unlist(actual_values)

combined_r <- cor(all_predictions, all_actual_values) %>% round(., 4)
combined_r

plot(all_predictions, all_actual_values, main = "Predictions vs Actual Values", xlab = "Predictions", ylab = "Actual Values")
cat("Fold results:\n")
print(cv_results)
cat("Mean r:", mean(unlist(cv_results)), "\n")
