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
train_test_split <- sample(c(1:nrow(wheat_all)), 0.8 * nrow(wheat_all), replace = FALSE)
data_train <- wheat_all[train_test_split, ]
data_test <- setdiff(wheat_all, data_train)

cross_val <- trainControl(method = "cv", number = 5)

set.seed(42)
model <- caret::train(Ert_dt_ha ~ ., data = data_train, method = "rf", trControl = cross_val)
pred <- predict(model, data_test)
r <- cor(pred, data_test$Ert_dt_ha) %>% round(., 4)
r

set.seed(42)
var_imp <- vip(model, method = "permute", train = data_train, target = "Ert_dt_ha", metric = "rsq",
               pred_wrapper = predict, nsim = 30, geom = "boxplot",
               aesthetics = list(fill = "darkgreen",color = "black")) +
  labs(title = "Variable Importance") + theme_bw()

var_imp