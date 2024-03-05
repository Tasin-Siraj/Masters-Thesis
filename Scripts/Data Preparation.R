#Name: Tasin Siraj
#Date: 16/10/2023
#R version 4.3.0 (2023-04-21 ucrt)

rm(list = ls())

library(readxl)
library(dplyr)

Main_data <- read_xlsx("Dataset_Translated.xlsx", sheet= "Field Experiment Results")
colnames(Main_data)

Main_data <- select(Main_data,-N_Withdrawal_kg_ha, -HL_kg_hl, -Sedi_ccm, -ET, -TKM_g, -SF_Yield)

Attempt <- read_excel("Dataset_Translated.xlsx", sheet= "Attempts")

#Attempt_21 <- Attempt %>%filter(Year == 2021)
#Attempt_22 <- Attempt %>%filter(Year == 2022)

Crop_Types <- read_excel("Dataset_Translated.xlsx", sheet= "Crop Types")
Soil_Data_From_sampling <- read_excel("Dataset_Translated.xlsx", sheet = "Soil Data_From sampling")

Weather_Attempt_10d <- read_excel("Dataset_Translated.xlsx", sheet = "Weather_Attempt_10d")
colnames(Weather_Attempt_10d)

Weather_Attempt_10d <- select(Weather_Attempt_10d, -ID_WS_Attempt, -ID_WS)

Strategy <- read_excel("Dataset_Translated.xlsx", sheet = "Fertilisation Strategy")

write.csv(Main_data, "Main_data.csv")
write.csv(Attempt, "Attempt.csv")
write.csv(Crop_Types, "Crop_Types.csv")
write.csv(Soil_Data_From_sampling, "Soil_Data_From_sampling.csv")
write.csv(Weather_Attempt_10d, "Weather_Attempt_10d.csv")

Analysis <- merge(Main_data, Attempt[,c("Attempt", "Crop", "Real_sample") ], by.x = "Attempt", by.y = "Attempt", all.x = TRUE)

Analysis <- merge(Analysis, Crop_Types[,c("CropType_Short", "CropType_Long") ], by.x = "Crop", by.y = "CropType_Short", all.x = TRUE)
Analysis <- select(Analysis, -Crop)

#Analysis <- Analysis[complete.cases(Analysis$CropType_Long), ]

Analysis <- merge(Analysis, Attempt[,c("Attempt", "Soil_identification") ], by.x = "Attempt", by.y = "Attempt", all.x = TRUE)

Analysis <- merge(Analysis, Attempt[,c("Attempt", "Variety") ], by.x = "Attempt", by.y = "Attempt", all.x = TRUE)

Analysis <- merge(Analysis, Attempt[,c("Attempt", "Year") ], by.x = "Attempt", by.y = "Attempt", all.x = TRUE)


Analysis <- merge(Analysis, Soil_Data_From_sampling[,c(1, 5:36) ], by.x = "Soil_identification", by.y = "Soil_Identifier", all.x = TRUE)

Analysis <- merge(Analysis, Weather_Attempt_10d, by.x = "Attempt", by.y = "Attempt", all.x = TRUE)

Analysis <- merge(Analysis, Strategy, by.x = "Pgl_Bez", by.y = "variant_name", all.x = TRUE)

write.csv(Analysis,"Analysis_21-22.csv", row.names = FALSE)

sample_dataset <- Analysis[sample(nrow(Analysis), 15), ]
write.csv(sample_dataset, "Sample_Dataset.csv", row.names = FALSE)
