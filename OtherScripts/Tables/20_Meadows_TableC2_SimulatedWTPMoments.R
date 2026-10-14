#### Meadows: Table C.2 - simulated WTP summary statistics ###############
# AUTHOR: Peter King (p.king1@leeds.ac.uk)
# LAST CHANGE: 04/03/2025
# FUNCTION: Table C.2 - median, mean, SD, and 95% CI of simulated WTP per
#   attribute and meadow type, from the weighted main model's WTP draws
#   (script 06).

# *************************************************************
#### Section 0: Setup libraries ####
# *************************************************************

# install.packages("data.table",repos="http://cran.us.r-project.org")
options(scipen = 90)
library(tidyverse)
library(dplyr)
library(magrittr)
library(mded)
library(here)
library(data.table)
library(stats)
library(ggdist)
library(mvtnorm)
library(apollo)
# library(cmdlr)


# *************************************************************
#### Section 1: Import data  ####
# *************************************************************

Data <- here(
  "ModelOutputs",
  "SimulatedMeans_AllTypes_Weighted.csv") %>% 
  fread() %>% 
  data.frame()


# *************************************************************
#### Section 2: Setup mapping  ####
# *************************************************************


# 
# Custom mapping for y-axis labels
Labels <- c(
  "Appearance:\n white",
  "Appearance:\n white, yellow, blue",
  "Appearance:\n white, yellow, blue,\n pink, red",
  "Native:\n decrease",
  "Native:\n increase",
  "Plants:\n five species",
  "Plants:\n ten species",
  "Pollinator quality:\n medium",
  "Pollinator quality:\n high"
)

Labels_Tidied <-  c("Appearance_W",
                    "Appearance_WYB",
                    "Appearance_WYBPR",
                    "Native_Decrease",
                    "Native_Increase",
                    "Plants_5",
                    "Plants_10",
                    "PollinatorQuality_Medium",
                    "PollinatorQuality_High")


# *************************************************************
#### Section 1B: Summarise data  ####
# *************************************************************

# TableCX_WTPSummmary <- Data %>%
#   pivot_longer(cols = 1:9) %>%
#   dplyr::mutate(name = factor(name, levels = Labels_Tidied)) %>%
#   group_by(meadow_type, name) %>%
#   summarise("WTP" = paste0(
#     "£",
#     mean(value) %>% round(2) %>% sprintf("%.2f", .),
#     " (£",
#     sd(value) %>% round(2) %>% sprintf("%.2f", .),
#     ")"
#   )) %>%
#   pivot_wider(names_from = meadow_type, values_from = WTP)
# 
# 
# TableCX_WTPSummmary %>% 
#   data.frame() %>% 
#   fwrite(
#     sep = ",",
#     here("ModelOutputs", "TableCX_WTPSummmary_Weighted.txt")
#   )


TableCX_WTPSummmary_Detailed <- Data %>%
  pivot_longer(cols = 1:9) %>%
  ## Keep the logical level order (Plants_5 before Plants_10, Medium before High); the default
  ## alphabetical order previously led to swapped row labels in SI Table C.2
  dplyr::mutate(name = factor(name, levels = Labels_Tidied)) %>%
  group_by(meadow_type, name) %>%
  summarise(
    "Median" = paste0("£",
                      median(value) %>% round(2) %>% sprintf("%.2f", .)),
    "Mean" = paste0("£", mean(value) %>% round(2) %>% sprintf("%.2f", .)),
    "SD" = paste0("£", sd(value) %>% round(2) %>% sprintf("%.2f", .)),
    "Lower" = paste0("£", quantile(value, c(0.025)) %>% round(2) %>% sprintf("%.2f", .)),
    "Upper" = paste0("£", quantile(value, c(0.975)) %>% round(2) %>% sprintf("%.2f", .)))




# *************************************************************
#### Section Export  ####
# *************************************************************


TableCX_WTPSummmary_Detailed %>%
  data.frame() %>%
  fwrite(
    sep = ",",
    here("Outputs", "Tables", "Meadows_TableC2_SimulatedWTPMoments.txt")
  )

