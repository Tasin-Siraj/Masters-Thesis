#Name: Tasin Siraj
#Date: 18/01/2024
#RStudio/2023.03.1+446

library(spdep)
library(sp)
library(readxl)
library(dplyr)

dataset <- read.csv("Analysis_21-22_Spatial.csv")  %>% 
  filter(N_Fertilization_kg_ha != 0 & Pgl_Bez != 'ohne N')

wheat <- dataset %>%
  filter(CropType_Long == "Winter Soft Wheat")
wheat <- wheat %>%
  rename( Latitude =GPS_noerdlB,
          Longitude =GPS_oestlL) 

wheat <- na.omit(wheat)

coordinates(wheat) <- ~Longitude + Latitude

neighbors <- knn2nb(knearneigh(coordinates(wheat), k = 5))
weights <- nb2listw(neighbors, style = "W")

moran_test <- moran.test(wheat$Ert_dt_ha, weights)
print(moran_test)

moran_test <- moran.test(wheat$RP_Go, weights)
print(moran_test)

moran_test <- moran.test(wheat$N_Efficiency, weights)
print(moran_test)

barley <- dataset %>%
  filter(CropType_Long == "Winter Barley")
barley <- barley %>%
  rename( Latitude =GPS_noerdlB,
          Longitude =GPS_oestlL) 

columns_with_all_NAs_barley <- names(barley)[apply(is.na(barley), 2, all)]
barley <- barley[, !colnames(barley) %in% columns_with_all_NAs_barley]
rows_with_na_barley <- barley[!complete.cases(barley), ]

barley <- na.omit(barley)

coordinates(barley) <- ~Longitude + Latitude

neighbors <- knn2nb(knearneigh(coordinates(barley), k = 5))
weights <- nb2listw(neighbors, style = "W")

moran_test <- moran.test(barley$Ert_dt_ha, weights)
print(moran_test)

moran_test <- moran.test(barley$RP_Go, weights)
print(moran_test)

moran_test <- moran.test(barley$N_Efficiency, weights)
print(moran_test)



rye <- dataset %>%
  filter(CropType_Long == "Winter Rye")
rye <- rye %>%
  rename( Latitude =GPS_noerdlB,
          Longitude =GPS_oestlL) 

columns_with_all_NAs_barley <- names(rye)[apply(is.na(rye), 2, all)]
rye <- rye[, !colnames(rye) %in% columns_with_all_NAs_barley]
rows_with_na_barley <- rye[!complete.cases(rye), ]

rye <- na.omit(rye)

coordinates(rye) <- ~Longitude + Latitude

neighbors <- knn2nb(knearneigh(coordinates(rye), k = 5))
weights <- nb2listw(neighbors, style = "W")

moran_test <- moran.test(rye$Ert_dt_ha, weights)
print(moran_test)

moran_test <- moran.test(rye$RP_Go, weights)
print(moran_test)

moran_test <- moran.test(rye$N_Efficiency, weights)
print(moran_test)
