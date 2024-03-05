#Name: Tasin Siraj
#Date: 30/12/2023
#R version 4.3.0 (2023-04-21 ucrt)

rm(list = ls())
setwd("D:/Zalf/Dataset/Analysis/Update 2/Data")
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)

data <- read.csv("Analysis_21-22.csv") %>% 
  filter(N_Fertilization_kg_ha != 0 & Pgl_Bez != 'ohne N')

wheat <- data %>%
  filter(CropType_Long == "Winter Soft Wheat")

Attempts <- wheat %>% 
  group_by(Attempt) %>%
  summarise(count = n())

wheat <- data %>% filter(CropType_Long == "Winter Soft Wheat")
wheat_freq <- table(wheat$Pgl_Bez)
strategy_count_wheat <- as.data.frame(wheat_freq) %>% mutate(CropType = "Wheat")
colnames(strategy_count_wheat) <- c("Strategy", "Freq")


rye <- data %>% filter(CropType_Long == "Winter Rye")
rye_freq <- table(rye$Pgl_Bez)
strategy_count_rye <- as.data.frame(rye_freq) %>% mutate(CropType = "Rye")
colnames(strategy_count_rye) <- c("Strategy", "Freq")


barley <- data %>% filter(CropType_Long == "Winter Barley")
barley_freq <- table(barley$Pgl_Bez)
strategy_count_barley <- as.data.frame(barley_freq) %>% mutate(CropType ="Barley")

