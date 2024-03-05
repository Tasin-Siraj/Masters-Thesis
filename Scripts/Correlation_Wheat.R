#Name: Tasin Siraj
#Date: 05/11/2023
#R version 4.3.0 (2023-04-21 ucrt)

rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(gridExtra)
library(corrplot)

data <- read.csv("Analysis_21-22.csv") %>% 
  filter(N_Fertilization_kg_ha != 0 & Pgl_Bez != 'ohne N')

wheat <- data %>%
  filter(CropType_Long == "Winter Soft Wheat")

high_cor_pairs <- function(data, threshold) {
  correlation_matrix <- cor(data, use = "pairwise.complete.obs", method = "pearson")
  high_cor <- which(abs(correlation_matrix) > threshold, arr.ind = TRUE)
  high_cor_pairs <- data.frame(
    Var1 = rownames(correlation_matrix)[high_cor[, 1]],
    Var2 = colnames(correlation_matrix)[high_cor[, 2]],
    Correlation = correlation_matrix[high_cor]
  )
  high_cor_pairs <- high_cor_pairs[high_cor_pairs$Var1 != high_cor_pairs$Var2,]
  high_cor_pairs <- high_cor_pairs[!duplicated(t(apply(high_cor_pairs, 1, sort))),]
  
  high_cor_pairs <- high_cor_pairs[order(-high_cor_pairs$Correlation),]
  
  return(high_cor_pairs)
}


#Soil_Variables
soil_data <- wheat[, 12:43]

soil_data <- soil_data %>% select(-Soiltype, -Soiltype_Short, 
                                  -Soil.Group, -Na_eff, -NH4_N, -NO3_N)
correlation_soil <- soil_data %>%
  select_if(is.numeric) %>%
  select_if(function(x) sd(x, na.rm = TRUE) != 0)

correlation_matrix <- cor(correlation_soil, use = "pairwise.complete.obs")

corrplot(correlation_matrix, 
         method = "color",
         order = "hclust", 
         hclust.method = "ward.D",
         addrect = 2,
         rect.col = "black",
         tl.col = "black", 
         tl.srt = 45,
         tl.cex = 0.6,
         cl.pos = "b",
         cl.cex = 0.8,
         diag = FALSE)


soil_high_pair <- high_cor_pairs(correlation_soil, 0.7)

# Weather Variable
weather_data <- wheat [, 44:108]

precipitation_Temp <- weather_data [ , 1:10]
correlation_matrix <- cor(precipitation_Temp, use = "pairwise.complete.obs")

corrplot(correlation_matrix, 
         method = "color",
         order = "hclust", 
         hclust.method = "ward.D",
         addrect = 2,
         rect.col = "black",
         tl.col = "black", 
         tl.srt = 45,
         tl.cex = 0.6,
         cl.pos = "b",
         cl.cex = 0.8,
         diag = FALSE)


precipitation_Temp_hogh_corr <- high_cor_pairs(precipitation_Temp, 0.70)
precipitation_Temp_hogh_corr

soil_temp <- weather_data [, 11:30]
correlation_matrix <- cor(soil_temp, use = "pairwise.complete.obs")

corrplot(correlation_matrix, 
         method = "color",
         order = "hclust", 
         hclust.method = "ward.D",
         addrect = 2,
         rect.col = "black",
         tl.col = "black", 
         tl.srt = 45,
         tl.cex = 0.6,
         cl.pos = "b",
         cl.cex = 0.8,
         diag = FALSE)


soil_temp_hogh_corr <- high_cor_pairs(soil_temp, 0.80)
soil_temp_hogh_corr

soil_moist <- weather_data [,31:45]
correlation_matrix <- cor(soil_moist, use = "pairwise.complete.obs")

corrplot(correlation_matrix, 
         method = "color",
         order = "hclust", 
         hclust.method = "ward.D",
         addrect = 2,
         rect.col = "black",
         tl.col = "black", 
         tl.srt = 45,
         tl.cex = 0.6,
         cl.pos = "b",
         cl.cex = 0.8,
         diag = FALSE)

soil_moist_hogh_corr <- high_cor_pairs(soil_moist, 0.80)
soil_moist_hogh_corr

soil_water <- weather_data [, 46:60]

correlation_matrix <- cor(soil_water, use = "pairwise.complete.obs")

corrplot(correlation_matrix, 
         method = "color",
         order = "hclust", 
         hclust.method = "ward.D",
         addrect = 2,
         rect.col = "black",
         tl.col = "black", 
         tl.srt = 45,
         tl.cex = 0.6,
         cl.pos = "b",
         cl.cex = 0.8,
         diag = FALSE)
soil_water_hogh_corr <- high_cor_pairs(soil_water, 0.80)
soil_water_hogh_corr

wind_speed <- weather_data[,61:65]
correlation_matrix <- cor(wind_speed, use = "pairwise.complete.obs")

corrplot(correlation_matrix, 
         method = "color",
         order = "hclust", 
         hclust.method = "ward.D",
         addrect = 2,
         rect.col = "black",
         tl.col = "black", 
         tl.srt = 45,
         tl.cex = 0.6,
         cl.pos = "b",
         cl.cex = 0.8,
         diag = FALSE)
wind_speed_hogh_corr <- high_cor_pairs(wind_speed, 0.50)
wind_speed_hogh_corr
correlation_weather <- weather_data %>%
  select_if(is.numeric) %>%
  select_if(function(x) sd(x, na.rm = TRUE) != 0)

correlation_matrix <- cor(correlation_weather, use = "pairwise.complete.obs")

corrplot(correlation_matrix, 
         method = "color",
         order = "hclust", 
         hclust.method = "ward.D",
         addrect = 2,
         rect.col = "black",
         tl.col = "black", 
         tl.srt = 45,
         tl.cex = 0.6,
         cl.pos = "b",
         cl.cex = 0.8,
         diag = FALSE)
high_weather <- high_cor_pairs(correlation_weather, 0.70)
