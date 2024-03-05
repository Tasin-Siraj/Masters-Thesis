#Name: Tasin Siraj
#Date: 16/10/2023
#R version 4.3.0 (2023-04-21 ucrt)

rm(list = ls())

library(readxl)
library(dplyr)
library(tidyr)
set.seed(3944410)

dataset <- read.csv("Analysis_21-22.csv")


types_of_crops <- split(dataset, dataset$CropType_Long)
wheat <- types_of_crops$`Winter Soft Wheat`
rye <- types_of_crops$`Winter Rye`
barley <- types_of_crops$`Winter Barley`


sum(wheat$N_Fertilization_kg_ha == 0)
wheat <- wheat[wheat$Pgl_Bez != 'ohne N', ] 

rows_with_na_wheat <- wheat[!complete.cases(wheat), ]
length(unique(rows_with_na_wheat$Attempt))

unique(rows_with_na_wheat$Attempt)

columns_with_all_NAs_rye <- names(rye)[apply(is.na(rye), 2, all)]
rye <- rye[, !colnames(rye) %in% columns_with_all_NAs_rye]

length(unique(rye$Attempt))
sum(rye$N_Fertilization_kg_ha == 0)
rye <- rye[rye$Pgl_Bez != 'ohne N', ] 

rows_with_na_rye <- rye[!complete.cases(rye), ]
length(unique(rows_with_na_rye$Attempt))

unique(rows_with_na_rye$Attempt)

columns_with_all_NAs_barley <- names(barley)[apply(is.na(barley), 2, all)]
barley <- barley[, !colnames(barley) %in% columns_with_all_NAs_barley]

length(unique(barley$Attempt))
sum(barley$N_Fertilization_kg_ha == 0)
barley <- barley[barley$Pgl_Bez != 'ohne N', ] 

rows_with_na_barley <- barley[!complete.cases(barley), ]
length(unique(rows_with_na_barley$Attempt))

unique(rows_with_na_barley$Attempt)

