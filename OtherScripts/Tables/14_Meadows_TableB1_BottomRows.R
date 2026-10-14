#### Meadows: Table B.1 - city/park demographic bottom rows ###############
## Function: Table B.1 - park names and per-city respondent counts, formatted
##   as the bottom summary rows of the city-demographics table.
## Author: Dr Peter King (p.king1@leeds.ac.uk)
## Last change: 24/07/2024
## Session info: see README.md ("Replication Information")

# *****************************
# Setup Environment: ####
# *****************************


library(magrittr)
library(dplyr)
library(apollo)
library(reshape2)
library(ggplot2)
library(ggridges)
library(distributional)
library(ggdist)
library(gridExtra)
library(data.table)
library(here)
library(tidyverse)


# ****************************************************************
#### Section 1: Import Data ####
# ****************************************************************



Data <-
  here("Data", "database_Meadows_Anonymised_Weighted.csv") %>% 
  fread() %>% 
  data.frame()


# ****************************************************************
#### Section 2: Prepare Data ####
# ****************************************************************


Data$Type_recode <- dplyr::recode(Data$Type,
                                  "1" = "Control",
                                  "2" = "Perennial",
                                  "3" = "Annual"
)

Data$city_recode <- dplyr::recode(Data$city,
                                  "1" = "Leeds",
                                  "2" = "Bristol", 
                                  "3" = "Edinburgh"
)



# 1) lookup table for prettifying the park IDs
park_labels <- c(
  Burley         = "Burley Park",
  Middleton      = "Middleton Park",
  Rodley         = "Rodley Park",
  King_Lane      = "King Lane",
  Gores_Marsh    = "Gores Marsh Park",
  Horfield_Common= "Horfield Common",
  St_Andrews     = "St. Andrews",
  Pilrig         = "Pilrig",
  Inch_Park      = "Inch Park",
  St_Marks       = "St. Marks",
  Stanhope_Rec   = "Stanhope Recreation ground"
)


## If one row per respondent needed
# Data <- Data %>% dplyr::filter(Task == 1)


# ****************************************************************
#### Section 3: Organise Data ####
# ****************************************************************


TableB1 <- Data %>%
  # select & dedupe
  dplyr::select(Clean_Park_ID, city_recode, Type_recode) %>%
  distinct() %>%
  
  # prettify
  mutate(
    park = recode(Clean_Park_ID, !!!park_labels),
    type = recode(Type_recode,
                  Control   = "Control sites",
                  Annual    = "Annual meadows",
                  Perennial = "Perennial meadows")
  ) %>%
  dplyr::select(-Clean_Park_ID, -Type_recode) %>%
  
  # collapse & widen in one chain
  group_by(type, city_recode) %>%
  summarise(
    parks = stringr::str_sort(park) %>% paste(collapse = ", "),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = city_recode,
    values_from = parks
  ) %>%
  
  # tidy up column names
  rename_with(~ c("Variable", "Bristol", "Edinburgh", "Leeds"), everything())


# ************************************************************
#### Section 3B: Add city composition rows + tests
# ************************************************************

# respondent-level one row only
CityData <- Data %>%
  dplyr::filter(Task == 1)

# counts + %
CitySummary <- CityData %>%
  dplyr::count(Type_recode, city_recode) %>%
  group_by(Type_recode) %>%
  mutate(
    pct = 100 * n / sum(n),
    value = paste0(n, " (", sprintf("%.2f", pct), "%)")
  ) %>%
  ungroup() %>%
  dplyr::select(Type_recode, city_recode, value) %>%
  pivot_wider(
    names_from = city_recode,
    values_from = value
  )

# reorder rows
CitySummary$Type_recode <- factor(
  CitySummary$Type_recode,
  levels = c("Control", "Perennial", "Annual")
)

CitySummary <- CitySummary %>%
  arrange(Type_recode) %>%
  rename(Variable = Type_recode)


# G-tests
tab_control <- table(CityData %>% filter(Type_recode == "Control") %>% pull(city_recode))
tab_annual  <- table(CityData %>% filter(Type_recode == "Annual") %>% pull(city_recode))
tab_perenn  <- table(CityData %>% filter(Type_recode == "Perennial") %>% pull(city_recode))

test_annual <- AMR::g.test(
  x = as.numeric(tab_annual),
  p = prop.table(tab_control)
)

test_perenn <- AMR::g.test(
  x = as.numeric(tab_perenn),
  p = prop.table(tab_control)
)

# optional test rows
fmt_p <- function(x){
  ifelse(x < 0.001, paste0(sprintf("%.3f", x), "***"),
         ifelse(x < 0.01,  paste0(sprintf("%.3f", x), "**"),
                ifelse(x < 0.05,  paste0(sprintf("%.3f", x), "*"),
                       sprintf("%.3f", x))))
}

Tests <- tibble(
  Variable = c("Annual vs Control (p)", "Perennial vs Control (p)"),
  Bristol = c(
    fmt_p(test_annual$p.value),
    fmt_p(test_perenn$p.value)
  ),
  Edinburgh = c("", ""),
  Leeds = c("", "")
)


# combine
TableB1 <- bind_rows(
  CitySummary,
  Tests,
  TableB1
)


# ****************************************************************
#### Section 4: Export this part of the table ####
# ****************************************************************


TableB1 %>% fwrite(sep = "#",
                           quote = FALSE,
                           paste0(here(
                             "Outputs", "Tables",
                             "Meadows_TableB1_BottomRows.txt"
                           )))



#### End of script
# ****************************************************************