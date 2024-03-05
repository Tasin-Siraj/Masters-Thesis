#Name: Tasin Siraj
#Date: 30/12/2023
#R version 4.3.0 (2023-04-21 ucrt)

rm(list = ls())
setwd("D:/Zalf/Dataset/Analysis/Update 2/Data")
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)

#Function to calculate RC
process_strategy <- function(data, strategy_name) {
  baseline_data_strategy <- data %>%
    group_by(Attempt) %>%
    filter(Pgl_Bez == strategy_name) %>%
    select(Attempt, baseline_value_strategy = Ert_dt_ha)
  
  wheat_RC_strategy <- data %>%
    left_join(baseline_data_strategy, by = "Attempt") %>%
    mutate(RC_yield_strategy = (Ert_dt_ha - baseline_value_strategy) / baseline_value_strategy) %>%
    select(-baseline_value_strategy) %>%  
    filter(Pgl_Bez != strategy_name)
  
  return(wheat_RC_strategy)
}

neg_pos_0 <- function(dataset) {
  positive_count <- sum(dataset > 0)
  negative_count <- sum(dataset < 0)
  Zero <- sum((dataset == 0))
  result <- list(positive = positive_count, negative = negative_count, Zero = Zero)
  return(result)
}

#Data Loading
data <- read.csv("Analysis_21-22.csv") %>% 
  filter(N_Fertilization_kg_ha != 0 & Pgl_Bez != 'ohne N')

wheat <- data %>%
  filter(CropType_Long == "Winter Soft Wheat")

unique_strategies <- unique(wheat$Pgl_Bez)

results_list <- list()

for (strategy in unique_strategies) {
  result <- process_strategy(wheat, strategy)
  result_no_na <- na.omit(result$RC_yield_strategy)
  neg_pos_result <- neg_pos_0(result_no_na)
  results_list[[strategy]] <- neg_pos_result
}

combined_results <- do.call(rbind, results_list)

rownames(combined_results) <- unique_strategies

kable(combined_results, format = 'markdown')

strategy_analysis <- data.frame(combined_results)
strategy_analysis$positive <- unlist(strategy_analysis$positive)
strategy_analysis$negative <- unlist(strategy_analysis$negative)
strategy_analysis$Zero <- unlist(strategy_analysis$Zero)

strategy_performance <- data.frame(strategy = rownames(strategy_analysis))

strategy_performance$Relative_Performance_Better <- with(strategy_analysis, (negative / (positive + negative + Zero)) * 100)
strategy_performance$Relative_Performance_Worse <- with(strategy_analysis, (positive / (positive + negative + Zero)) * 100)

print(strategy_performance)

strategy_performance$Relative_Performance_Worse <- NULL


SP_Yield <- strategy_performance %>%
  rename(Probability_of_better_yield = Relative_Performance_Better)
