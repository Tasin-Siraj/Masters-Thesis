#Name: Tasin Siraj
#Date: 22/12/2023
#R version 4.3.0 (2023-04-21 ucrt)

rm(list = ls())

library(readxl)
library(dplyr)
library(vtable)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(grid)

data <- read.csv("Analysis_21-22.csv")

wheat <- data %>%
  filter(CropType_Long == "Winter Soft Wheat" & 
           N_Fertilization_kg_ha != 0 & Pgl_Bez != 'ohne N')

rows_with_na_wheat <- wheat[!complete.cases(wheat), ]

rye <- data %>%
  filter(CropType_Long == "Winter Rye" & 
           N_Fertilization_kg_ha != 0 & Pgl_Bez != 'ohne N')

columns_with_all_NAs_rye <- names(rye)[apply(is.na(rye), 2, all)]
rye <- rye[, !colnames(rye) %in% columns_with_all_NAs_rye]
rows_with_na_rye <- rye[!complete.cases(rye), ]

barley <- data %>%
  filter(CropType_Long == "Winter Barley" & 
           N_Fertilization_kg_ha != 0 & Pgl_Bez != 'ohne N')

columns_with_all_NAs_barley <- names(barley)[apply(is.na(barley), 2, all)]
barley <- barley[, !colnames(barley) %in% columns_with_all_NAs_barley]
rows_with_na_barley <- barley[!complete.cases(barley), ]

wheat <- na.omit(wheat)
rye <- na.omit(rye)
barley <- na.omit(barley)

length(unique(wheat$Soil_identification))
length(unique(rye$Soil_identification))
length(unique(barley$Soil_identification))


wheat$CropType <- 'Wheat'
rye$CropType <- 'Rye'
barley$CropType <- 'Barley'

combined_dataset <- bind_rows(wheat, rye, barley)

K <- ggplot(data=combined_dataset, aes(x=K_Class, y=K, fill=CropType)) + 
  geom_boxplot(varwidth = TRUE, show.legend=TRUE, alpha=0.8,
               outlier.shape = 8, outlier.alpha=1, outlier.size = 3) + 
  geom_point(stat = "unique", position = "jitter", alpha=0.8) +
  facet_wrap(~CropType, scales = "free_x") +
  theme_minimal() + theme(panel.spacing.x = unit(2, "lines")) +
  labs(y = "K (mg/100g)", x = "K Class",
       caption = "Note: Stars indicate outliers.")

K

P <- ggplot(data=combined_dataset, aes(x=P_Class, y=P, fill=CropType)) + 
  geom_boxplot(varwidth = TRUE, show.legend=TRUE, alpha=0.8,
               outlier.shape = 8, outlier.alpha=1, outlier.size = 3) + 
  geom_point(stat = "unique", position = "jitter", alpha=0.8) +
  facet_wrap(~CropType, scales = "free_x") + 
  theme_minimal() + theme(panel.spacing.x = unit(2, "lines")) +
  labs(y = "P (mg/100g)", x = "P Class",
       caption = "Note: Stars indicate outliers.")

P

Mg <- ggplot(data=combined_dataset, aes(x=Mg_Class, y=Mg, fill=CropType)) + 
  geom_boxplot(varwidth = TRUE, show.legend=TRUE, alpha=0.8,
               outlier.shape = 8, outlier.alpha=1, outlier.size = 3) + 
  geom_point(stat = "unique", position = "jitter", alpha=0.8) +
  facet_wrap(~CropType, scales = "free_x") +
  theme_minimal() + theme(panel.spacing.x = unit(2, "lines")) +
  labs(y = "Mg (mg/100g)", x = "Mg Class",
       caption = "Note: Stars indicate outliers.")

Mg
B <- ggplot(data=combined_dataset, aes(x=B_Class, y=B, fill=CropType)) + 
  geom_boxplot(varwidth = TRUE, show.legend=TRUE, alpha=0.8,
               outlier.shape = 8, outlier.alpha=1, outlier.size = 3) + 
  geom_point(stat = "unique", position = "jitter", alpha=0.8) +
  facet_wrap(~CropType, scales = "free_x") +
  theme_minimal() + theme(panel.spacing.x = unit(2, "lines")) +
  labs(y = "B (mg/Kg)", x = "B Class",
       caption = "Note: Stars indicate outliers.")

B

Mn <- ggplot(data=combined_dataset, aes(x=Mn_Class, y=Mn, fill=CropType)) + 
  geom_boxplot(varwidth = TRUE, show.legend=TRUE, alpha=0.8,
               outlier.shape = 8, outlier.alpha=1, outlier.size = 3) + 
  geom_point(stat = "unique", position = "jitter", alpha=0.8) +
  facet_wrap(~CropType, scales = "free_x") + theme_minimal() + 
  theme(panel.spacing.x = unit(2, "lines")) +
  labs(y = "Mn (mg/Kg)", x = "Mn Class",
       caption = "Note: Stars indicate outliers.")

Mn

Cu <- ggplot(data=combined_dataset, aes(x=Cu_Class, y=Cu, fill=CropType)) + 
  geom_boxplot(varwidth = TRUE, show.legend=TRUE, alpha=0.8,
               outlier.shape = 8, outlier.alpha=1, outlier.size = 3) + 
  geom_point(stat = "unique", position = "jitter", alpha=0.8) +
  facet_wrap(~CropType, scales = "free_x") + theme_minimal() + 
  theme(panel.spacing.x = unit(2, "lines")) +
  labs(y = "Cu (mg/Kg)", x = "Cu Class",
       caption = "Note: Stars indicate outliers.")

Cu

pH <- ggplot(combined_dataset, aes(x = pH, color = CropType)) +
  geom_density(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("Wheat" = "blue", "Rye" = "lightgreen", "Barley" = "firebrick")) +
  labs( x = "pH", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

pH

corg <- ggplot(combined_dataset, aes(x = Corg, color = CropType)) +
  geom_density(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("Wheat" = "blue", "Rye" = "lightgreen", "Barley" = "firebrick")) +
  labs( x = "Organic Carbon in %", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

corg

Nges <- ggplot(combined_dataset, aes(x = Nges, color = CropType)) +
  geom_density(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("Wheat" = "blue", "Rye" = "lightgreen", "Barley" = "firebrick")) +
  labs( x = "Total Nitrogen in Kg/Ha", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

Nges

Smin <- ggplot(combined_dataset, aes(x = Smin, color = CropType)) +
  geom_density(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("Wheat" = "blue", "Rye" = "lightgreen", "Barley" = "firebrick")) +
  labs( x = "Mineral Sulfur in Kg/Ha", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

Smin

Macro_nutrients <- grid.arrange(pH, corg, Nges, Smin, ncol = 2,
                                top = textGrob("Density Plots for Macronutrients", 
                                               gp = gpar(fontface = "bold", 
                                                         fontsize = 12)))
Macro_nutrients

Zn <- ggplot(combined_dataset, aes(x = Zn, color = CropType)) +
  geom_density(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("Wheat" = "blue", "Rye" = "lightgreen", "Barley" = "firebrick")) +
  labs( x = "Zinc Concentration in mg/Kg", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

Zn

Mo <- ggplot(combined_dataset, aes(x = Mo, color = CropType)) +
  geom_density(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("Wheat" = "blue", "Rye" = "lightgreen", "Barley" = "firebrick")) +
  labs(x = "Molybdenum Concentraion in mg/Kg", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

Mo

Micro_Nutrients <- grid.arrange(Zn, Mo, ncol = 2, 
                                top = textGrob("Density Plots for Micronutrients", 
                                               gp = gpar(fontface = "bold", 
                                                         fontsize = 12)))

KAK_eff <- ggplot(combined_dataset, aes(x = KAK_eff, color = CropType)) +
  geom_density(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("Wheat" = "blue", "Rye" = "lightgreen", "Barley" = "firebrick")) +
  labs( x = "Effective cation exchange capacity in cmol/kg", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

KAK_eff

Ca_eff <- ggplot(combined_dataset, aes(x = Ca_eff, color = CropType)) +
  geom_density(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("Wheat" = "blue", "Rye" = "lightgreen", "Barley" = "firebrick")) +
  labs( x = "Effective cation occupancy with Ca in cmol/kg", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

Ca_eff

Mg_eff <- ggplot(combined_dataset, aes(x = Mg_eff, color = CropType)) +
  geom_density(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("Wheat" = "blue", "Rye" = "lightgreen", "Barley" = "firebrick")) +
  labs( x = "Effective cation occupancy with Mg cmol/kg", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

Mg_eff

K_eff <- ggplot(combined_dataset, aes(x = K_eff, color = CropType)) +
  geom_density(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("Wheat" = "blue", "Rye" = "lightgreen", "Barley" = "firebrick")) +
  labs( x = "Effective cation occupancy with K cmol/kg", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

K_eff

BS <- ggplot(combined_dataset, aes(x = BS, color = CropType)) +
  geom_density(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("Wheat" = "blue", "Rye" = "lightgreen", "Barley" = "firebrick")) +
  labs( x = "Base Saturation in %", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

BS

legend <- ggplotGrob(KAK_eff + theme(legend.position = "bottom"))$grobs[[which(sapply(ggplotGrob(KAK_eff + theme(legend.position = "bottom"))$grobs, function(x) x$name) == "guide-box")]]

Cation_Capacity <- grid.arrange(KAK_eff, Ca_eff, Mg_eff, K_eff, BS, legend, ncol = 2, 
                                top = textGrob("Density Distributions of Effective Cation Exchange Capacity", 
                                               gp = gpar(fontface = "bold", 
                                                         fontsize = 12)))
Cation_Capacity



sand <- ggplot(combined_dataset, aes(x = Sand, color = CropType)) +
  geom_density(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("Wheat" = "blue", "Rye" = "lightgreen", "Barley" = "firebrick")) +
  labs( x = "% of Sand", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

sand
silt <- ggplot(combined_dataset, aes(x = Silt, color = CropType)) +
  geom_density(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("Wheat" = "blue", "Rye" = "lightgreen", "Barley" = "firebrick")) +
  labs( x = "% of Silt", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

silt
clay <- ggplot(combined_dataset, aes(x = Clay, color = CropType)) +
  geom_density(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("Wheat" = "blue", "Rye" = "lightgreen", "Barley" = "firebrick")) +
  labs( x = "% of Clay", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

clay
Soil_Texture <- grid.arrange(sand, silt, clay, legend, ncol = 2, 
                             top = textGrob("Density Distributions of Sieve and Sludge Analysis", 
                                            gp = gpar(fontface = "bold", 
                                                      fontsize = 12)))
Soil_Texture


ggsave("Mn.png", plot = Mn , width = 16, height = 8, units = "in")

weather_wheat <- wheat[,44:108]
summary_wheat <- sumtable(weather_wheat, out='return') %>%  mutate(Crop = 'Wheat')
kable(summary_wheat, format = "markdown")

weather_rye <- rye[,44:82]
summary_rye <- sumtable(weather_rye, out='return') %>%  mutate(Crop = 'Rye')
summary_rye[summary_rye == Inf | summary_rye == -Inf] <- NA
kable(summary_rye, format = "markdown")

weather_barley <- barley[,44:82]
summary_barley <- sumtable(weather_barley, out='return') %>%  mutate(Crop = 'Barley')
summary_barley[summary_barley == Inf | summary_barley == -Inf] <- NA
kable(summary_barley, format = "markdown")

combined_summary <- bind_rows(summary_wheat, summary_rye, summary_barley) %>%
  arrange(Variable, Crop) %>%
  filter(!(Crop %in% c("Rye", "Barley") & N == 0))
