#### Meadows: Table C.3 - Poe tests (unweighted, robustness) ###############
# AUTHOR: Peter King (p.king1@leeds.ac.uk)
# LAST CHANGE: 22/02/2025
# FUNCTION: Table C.3 - Poe et al. (2005) combinatorial test statistic and p-value
#   against the null of no difference in mean attribute WTP between meadow types,
#   using WTP simulated from the unweighted main model (via script 07).
#   Robustness counterpart to the main-text Table 3 (script 13).


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
library(coin)
library(mvtnorm)
library(apollo)
# library(cmdlr)


# *************************************************************
#### Section 1: Import data from SimulateWTPs ####
# *************************************************************


## Import if not in workspace
all_results <- here("ModelOutputs",
                    "SimulatedMeans_AllTypes_Unweighted.csv") %>% fread() %>% data.frame()


# *************************************************************
#### Section 2: Setup ####
# *************************************************************


# Define attributes as character vector (not function)
attributes_vec <- c(
  "Appearance_W",
  "Appearance_WYB",
  "Appearance_WYBPR",
  "Native_Decrease",
  "Native_Increase",
  "Plants_5",
  "Plants_10",
  "PollinatorQuality_Medium",
  "PollinatorQuality_High"
)


# Function to add significance stars to P values
add_stars <- function(p_value) {
  case_when(
    p_value < 0.001 ~ paste0(sprintf("%.3f", p_value), "****"),
    p_value < 0.01 ~ paste0(sprintf("%.3f", p_value), "***"),
    p_value < 0.05 ~ paste0(sprintf("%.3f", p_value), "**"),
    p_value < 0.10 ~ paste0(sprintf("%.3f", p_value), "*"),
    TRUE ~ sprintf("%.3f", p_value)
  )
}


# *************************************************************
#### Step 1: Calculate mean WTP per meadow type per attribute ####
# *************************************************************


mean_wtp_by_meadow <- all_results %>%
  group_by(meadow_type) %>%
  summarise(across(
    .cols = all_of(attributes_vec),  # Use .cols explicitly
    .fns = ~mean(.x, na.rm = TRUE),   # Use .fns explicitly
    .names = "{.col}"
  )) %>%
  pivot_longer(
    cols = -meadow_type,
    names_to = "attribute",
    values_to = "mean_wtp"
  ) %>%
  pivot_wider(
    names_from = meadow_type,
    values_from = mean_wtp,
    names_prefix = "WTP_"
  )


# *************************************************************
#### Step 2: Run Poe tests and extract P(Δ>0) only ####
# *************************************************************


# First make sure perform_poe_test function is loaded
perform_poe_test <- function(data, attribute, group1_name, group2_name) {
  
  # Extract distributions for each group
  distr1 <- data %>% 
    filter(meadow_type == group1_name) %>% 
    pull(!!sym(attribute))
  
  distr2 <- data %>% 
    filter(meadow_type == group2_name) %>% 
    pull(!!sym(attribute))
  
  # Perform Poe et al. test
  test_result <- mded(distr1 = distr1, 
                      distr2 = distr2, 
                      detail = FALSE,
                      independent = TRUE)
  
  # Return p-value directly (no transformation)
  return(list(p_value = test_result$stat))
}


# Run Poe tests
poe_results <- map_dfr(attributes_vec, function(attr) {
  
  # Annual vs Control
  test_ac <- perform_poe_test(all_results, attr, "Annual", "Control")
  
  # Annual vs Perennial  
  test_ap <- perform_poe_test(all_results, attr, "Annual", "Perennial")
  
  # Perennial vs Control
  test_pc <- perform_poe_test(all_results, attr, "Perennial", "Control")
  
  tibble(
    attribute = attr,
    P_Annual_vs_Control = test_ac$p_value,
    P_Annual_vs_Perennial = test_ap$p_value,
    P_Perennial_vs_Control = test_pc$p_value
  )
})


# *************************************************************
#### Step 3: Merge mean WTP with Poe test results ####
# *************************************************************


combined_table <- mean_wtp_by_meadow %>%
  left_join(poe_results, by = "attribute") %>%
  # Reorder columns for clarity
  dplyr::select(
    attribute,
    WTP_Annual,
    WTP_Control, 
    WTP_Perennial,
    P_Annual_vs_Control,
    P_Annual_vs_Perennial,
    P_Perennial_vs_Control
  ) %>%
  # Round values
  mutate(
    across(starts_with("WTP_"), ~round(.x, 2)),
    across(starts_with("P_"), ~round(.x, 3))
  ) %>%
  # Add nice attribute labels
  mutate(
    attribute_label = case_when(
      attribute == "Appearance_W" ~ "Flower colours: white",
      attribute == "Appearance_WYB" ~ "Flower colours: white, yellow, blue",
      attribute == "Appearance_WYBPR" ~ "Flower colours: white, yellow, blue, pink, red",
      attribute == "Native_Decrease" ~ "Native: decrease",
      attribute == "Native_Increase" ~ "Native: increase",
      attribute == "Plants_5" ~ "Plants: five additional species",
      attribute == "Plants_10" ~ "Plants: ten additional species",
      attribute == "PollinatorQuality_Medium" ~ "Quality for pollinating insects: medium",
      attribute == "PollinatorQuality_High" ~ "Quality for pollinating insects: high",
      TRUE ~ attribute  # fallback
    )
  ) %>%
  dplyr::select(attribute_label, everything(), -attribute) %>%
  rename(Attribute = attribute_label)


# *************************************************************
#### Step 4: Format for Word/LaTeX with currency symbols ####
# *************************************************************


# Apply stars for easier reading
combined_table_with_stars <- combined_table %>%
  mutate(
    P_Annual_vs_Control = add_stars(P_Annual_vs_Control),
    P_Annual_vs_Perennial = add_stars(P_Annual_vs_Perennial),
    P_Perennial_vs_Control = add_stars(P_Perennial_vs_Control)
  ) %>%
  mutate(
    WTP_Annual = paste0("£", format(WTP_Annual, nsmall = 2, trim = TRUE)),
    WTP_Control = paste0("£", format(WTP_Control, nsmall = 2, trim = TRUE)),
    WTP_Perennial = paste0("£", format(WTP_Perennial, nsmall = 2, trim = TRUE))
  )


# *************************************************************
#### Step 5: Export ####
# *************************************************************

## Get desired column order
combined_table_with_stars_reorder <- combined_table_with_stars %>% 
  dplyr::select(
    Attribute,
    WTP_Control,
    WTP_Annual,
    WTP_Perennial,
    P_Annual_vs_Control,
    P_Perennial_vs_Control,
    P_Annual_vs_Perennial
  ) %>% 
  dplyr::rename(
    Control = WTP_Control,
    Annual = WTP_Annual,
    Perennial = WTP_Perennial,
    `Control vs Annual` = P_Annual_vs_Control,
    `Control vs Perennial` = P_Perennial_vs_Control,
    `Annual vs Perennial` = P_Annual_vs_Perennial
  )


# Export with stars
# Export with stars
combined_table_with_stars_reorder %>%
  fwrite(
    here("Outputs", "Tables", "Meadows_TableC3_PoeTests_Unweighted.txt"),
    sep = "#"
  )

