#### Meadows: Table B.6 - respondents by meadow type, park, and city ###############
# Function: Table B.6 - total number of respondents and % of the total sample
#   interviewed at each of the three meadow types, in each park, in each of the
#   three cities.
# Script author: Peter King (p.king@Leeds.ac.uk)
# Last Edited: 21/05/2025
# Session info: see README.md ("Replication Information")


# ****************************************************************************
#### Section 0: Setup ####
# ****************************************************************************


## Libraries: **************************************************************
library(here)
library(tidyverse)
library(data.table)
library(AMR)
library(magrittr)


## ****************************************************************************
#### Step One: Read in data frame with all respondents ####
## ****************************************************************************



Data <-
  here("Data", "database_Meadows_Anonymised_Weighted.csv") %>%
  fread() %>%
  data.frame()

# Data <-
#   here("Data", "database_Meadows_Trimmed_Weighted_2022_07_19.csv") %>%
#   fread() %>%
#   data.frame()




## ****************************************************************************
#### Step Two: Trim and prepare data ####
## ****************************************************************************



## This is wide format so select only one row per respondent
Data <- Data %>% dplyr::filter(Task  == 1) 


# Assuming your data has numeric coding - let's first create proper labels
TableData <- Data %>%
  mutate(
    city = case_when(
      city == 1 ~ "Leeds",
      city == 2 ~ "Bristol",
      city == 3 ~ "Edinburgh"
    ),
    Type = case_when(
      Type == 1 ~ "Control",
      Type == 2 ~ "Perennial",
      Type == 3 ~ "Annual"
    )
  )


## ****************************************************************************
#### Step Three:  ####
## ****************************************************************************


## Create summary by city, meadow type, and park
Table1 <- TableData %>%
  # Get counts by group
  group_by(city, Type, park...34) %>%
  summarise(n = n(), .groups = "drop") %>%
  # Calculate percentages
  mutate(percent = n / nrow(TableData) * 100) %>%
  # Add totals for each city-type combination
  bind_rows(
    group_by(., city, Type) %>%
      summarise(
        park...34 = "All parks",
        n = sum(n),
        percent = sum(percent),
        .groups = "drop"
      )
  ) %>%
  # Add totals for each city (filter out subtotals first to avoid double-counting)
  bind_rows(
    dplyr::filter(., park != "All parks") %>%
    group_by(city) %>%
      summarise(
        Type = "All types",
        park = "All parks",
        n = sum(n),
        percent = sum(percent),
        .groups = "drop"
      )
  ) %>%
  # Add grand total
  bind_rows(
    summarise(TableData,
              city = "All cities",
              Type = "All types",
              park = "All parks",
              n = n(),
              percent = 100
    )
  ) %>%
  # Format for output
  arrange(
    factor(city, levels = c("Bristol", "Edinburgh", "Leeds", "All cities")),
    factor(Type, levels = c("Control", "Annual", "Perennial", "All types")),
    park
  ) %>%
  # Format percentage with 2 decimal places
  mutate(
    percent = sprintf("%.2f", percent),
    # Create a combined count and percentage column
    `n (%)` = paste0(n, " (", percent, "%)")
  ) %>%
  # Select and rename columns for final output
  dplyr::select(
    "City" = city,
    "Meadow Type" = Type,
    "Park" = park,
    "n (%)" = `n (%)`
  )





## ****************************************************************************
#### Step Four: Export data ####
## ****************************************************************************


Table1 %>% fwrite(sep = "#",
                           quote = FALSE,
                           paste0(here(
                             "Outputs", "Tables",
                             "Meadows_TableB6_SampleByMeadowAndCity.txt"
                           )))



## ****************************************************************************
#### OLD SIMPLE VERSION ####
## ****************************************************************************



# Create the summary table
Table1 <- TableData %>%
  # Get counts by group
  group_by(Type, city) %>%
  summarise(n = n(), .groups = "drop") %>%
  # Calculate percentages
  mutate(percent = n / nrow(Data) * 100) %>%
  # Add totals for each type
  bind_rows(
    group_by(., Type) %>%
      summarise(
        city = "Total",
        n = sum(n),
        percent = sum(percent)
      )
  ) %>%
  # Add grand total
  bind_rows(
    summarise(Data,
              Type = "All types",
              city = "All cities",
              n = n(),
              percent = 100
    )
  ) %>%
  # Format for output
  arrange(factor(Type, levels = c("Annual", "Perennial", "Control", "All types"))) %>%
  mutate(percent  = percent %>% sprintf("%.2f",.)) %>% 
  rename(
    "Meadow Type" = Type,
    "City" = city,
    "Number of respondents" = n,
    "Percentage of total sample (%)" = percent
  )



# End Of Script **************************************************************