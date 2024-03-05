#Name: Tasin Siraj
#Date: 18/01/2024
#RStudio/2023.03.1+446

library(dplyr)
library(ggplot2)
library(sf)
library(caret)
library(party)
library(randomForest)
library(spatialsample)
library(vip)

rm(list = ls())
set.seed(3944410)

dataset <- read.csv("Analysis_21-22.csv") %>% 
  filter(N_Fertilization_kg_ha != 0 & Pgl_Bez != 'ohne N')

wheat <- dataset %>%
  filter(CropType_Long == "Winter Soft Wheat")

wheat_all <-  wheat %>% select(6:129) %>%
  select( -RP_Go, -N_Efficiency, -CropType_Long, -Variety, -Year,
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

set.seed(123)
wheat_all <- wheat_all[sample(nrow(wheat_all)), ]
folds <- createFolds(wheat_all$Ert_dt_ha, k = 5, list = TRUE, returnTrain = TRUE)

predictions <- list()
actual_values <- list()
fold_r <- numeric(length(folds))

for (fold_index in 1:length(folds)) {
  fold_train_indices <- folds[[fold_index]]
  fold_train_data <- wheat_all[fold_train_indices, ]
  fold_test_indices <- setdiff(1:nrow(wheat_all), fold_train_indices)
  fold_test_data <- wheat_all[fold_test_indices, ]
  
  ctree_control <- ctree_control(testtype = "Univariate", minsplit = 10, maxdepth = 3)
  model <- ctree(Ert_dt_ha ~ ., data = fold_train_data, controls = ctree_control)
  
  pred <- predict(model, newdata = fold_test_data)
  
  predictions[[fold_index]] <- pred
  actual_values[[fold_index]] <- fold_test_data$Ert_dt_ha
  
  fold_r[fold_index] <- cor(pred, fold_test_data$Ert_dt_ha)
}

all_predictions <- unlist(predictions)
all_actual_values <- unlist(actual_values)

combined_r <- cor(all_predictions, all_actual_values)
combined_r <- round(combined_r, 4)

print(combined_r)
fold_r
mean(fold_r)
