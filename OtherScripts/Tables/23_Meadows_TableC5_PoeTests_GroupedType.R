#### Meadows: Table C.5 - Grouped Meadow Types (Treated vs Control) ###############
# AUTHOR: Peter King (p.king1@leeds.ac.uk)
# LAST CHANGE: 19/02/2026
# FUNCTION: Table C.5 - Mean WTP (SD) with Poe et al. (2005) tests comparing
#   Treated (Annual+Perennial pooled) vs Control, for both the weighted (04) and
#   unweighted (05) grouped-type models, using WTP simulated in script 08.
# *************************************************************
#### Section 0: Setup libraries ####
# *************************************************************
options(scipen = 999)
library(here)
library(data.table)
library(dplyr)
library(tidyr)
library(purrr)
library(mded)

# *************************************************************
#### Section 1: Import simulated WTP data ####
# *************************************************************


all_results <- here("ModelOutputs",
                    "SimulatedMeans_GroupedType.csv") %>% 
  fread() %>% 
  data.frame()


# *************************************************************
#### Section 2: Define attributes and helper functions ####
# *************************************************************


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


# Verify all attributes exist in data
missing_attrs <- setdiff(attributes_vec, names(all_results))
if(length(missing_attrs) > 0) {
  stop("Missing attributes in data: ", paste(missing_attrs, collapse = ", "))
}


# Function to perform Poe et al. (2005) test
perform_poe_test <- function(data, attribute, group1_name, group2_name) {
  
  # Extract distributions for each group
  distr1 <- data %>% 
    filter(meadow_type == group1_name) %>% 
    pull(!!sym(attribute))
  
  distr2 <- data %>% 
    filter(meadow_type == group2_name) %>% 
    pull(!!sym(attribute))
  
  # Check for empty distributions
  if(length(distr1) == 0 | length(distr2) == 0) {
    warning(sprintf("Empty distribution for %s: %s vs %s", 
                    attribute, group1_name, group2_name))
    return(list(p_value = NA))
  }
  
  # Perform Poe et al. test
  # H0: distr1 = distr2; H1: distr1 > distr2
  # Returns p-value for one-tailed test
  test_result <- mded(distr1 = distr1, 
                      distr2 = distr2, 
                      detail = FALSE,
                      independent = TRUE)
  
  return(list(p_value = test_result$stat))
}


# Function to add significance stars to p-values (vectorized)
add_stars <- function(p_values) {
  sapply(p_values, function(p_value) {
    if(is.na(p_value)) return("NA")
    
    if(p_value < 0.001) {
      paste0(sprintf("%.3f", p_value), "****")
    } else if(p_value < 0.01) {
      paste0(sprintf("%.3f", p_value), "***")
    } else if(p_value < 0.05) {
      paste0(sprintf("%.3f", p_value), "**")
    } else if(p_value < 0.10) {
      paste0(sprintf("%.3f", p_value), "*")
    } else {
      sprintf("%.3f", p_value)
    }
  })
}


# Function to create nice attribute labels
create_attribute_labels <- function(attribute) {
  case_when(
    attribute == "Appearance_W" ~ "Flower colours: white",
    attribute == "Appearance_WYB" ~ "Flower colours: white, yellow, blue",
    attribute == "Appearance_WYBPR" ~ "Flower colours: white, yellow, blue, pink, red",
    attribute == "Native_Decrease" ~ "Native: decrease",
    attribute == "Native_Increase" ~ "Native: increase",
    attribute == "Plants_5" ~ "Plants: five additional species",
    attribute == "Plants_10" ~ "Plants: ten additional species",
    attribute == "PollinatorQuality_Medium" ~ "Quality for pollinating insects: medium",
    attribute == "PollinatorQuality_High" ~ "Quality for pollinating insects: high",
    TRUE ~ attribute
  )
}


# *************************************************************
#### Step 1: Calculate mean WTP and SD per meadow type per model ####
# *************************************************************


## Nice Claude code to report mean (SD) per attribute and meadow and model
summary_stats <- all_results %>%
  group_by(Model, meadow_type) %>%
  summarise(across(
    .cols = all_of(attributes_vec),
    .fns = list(
      mean = ~mean(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  ), .groups = "drop") %>%
  pivot_longer(
    cols = -c(Model, meadow_type),
    names_to = c("attribute", ".value"),
    names_pattern = "(.+)_(mean|sd)"
  ) %>%
  mutate(
    wtp_formatted = paste0(
      "£", sprintf("%.2f", mean),
      " (£", sprintf("%.2f", sd), ")"
    )
  ) %>%
  select(Model, meadow_type, attribute, mean, sd, wtp_formatted) %>%
  pivot_wider(
    names_from = meadow_type,
    values_from = c(mean, sd, wtp_formatted),
    names_glue = "{meadow_type}_{.value}"
  )


# *************************************************************
#### Step 2: Run Poe tests for Treated vs Control ####
# *************************************************************


# For weighted model
poe_results_weighted <- map_dfr(attributes_vec, function(attr) {
  data_weighted <- all_results %>% filter(Model == "Weighted")
  test_result <- perform_poe_test(data_weighted, attr, "Treated", "Control")
  
  tibble(
    attribute = attr,
    Model = "Weighted",
    P_Treated_vs_Control = test_result$p_value
  )
})


# For unweighted model
poe_results_unweighted <- map_dfr(attributes_vec, function(attr) {
  data_unweighted <- all_results %>% filter(Model == "Unweighted")
  test_result <- perform_poe_test(data_unweighted, attr, "Treated", "Control")
  
  tibble(
    attribute = attr,
    Model = "Unweighted",
    P_Treated_vs_Control = test_result$p_value
  )
})


# Combine Poe results
poe_results_all <- bind_rows(poe_results_weighted, poe_results_unweighted)


# *************************************************************
#### Step 3: Merge summary stats with Poe test results ####
# *************************************************************


combined_table <- summary_stats %>%
  left_join(poe_results_all, by = c("Model", "attribute")) %>%
  # Round p-values
  mutate(P_Treated_vs_Control = round(P_Treated_vs_Control, 3)) %>%
  # Add nice attribute labels
  mutate(attribute_label = create_attribute_labels(attribute)) %>%
  # Reorder columns
  dplyr::select(
    Model,
    attribute_label,
    Control_wtp_formatted,
    Treated_wtp_formatted,
    P_Treated_vs_Control
  ) %>%
  rename(
    Attribute = attribute_label,
    `Mean WTP Control` = Control_wtp_formatted,
    `Mean WTP Treated` = Treated_wtp_formatted,
    `P(Treated>Control)` = P_Treated_vs_Control
  )


# *************************************************************
#### Step 4: Format for publication with stars ####
# *************************************************************


# Average WTP values across both models
# Get WTP values from weighted model only
wtp_values <- combined_table %>%
  filter(Model == "Weighted") %>%
  dplyr::select(Attribute, `Mean WTP Control`, `Mean WTP Treated`)


# Then pivot p-values and join
table_wide <- combined_table %>%
  dplyr::select(Model, Attribute, `P(Treated>Control)`) %>%
  mutate(`P(Treated>Control)` = add_stars(`P(Treated>Control)`)) %>%
  pivot_wider(
    names_from = Model,
    values_from = `P(Treated>Control)`
  ) %>%
  left_join(wtp_values, by = "Attribute") %>%
  # Reorder columns
  dplyr::select(
    Attribute,
    `Mean WTP Control`,
    `Mean WTP Treated`,
    Unweighted,
    Weighted
  ) %>%
  # Rename for publication
  rename(
    Control = `Mean WTP Control`,
    `Annually or perennially sown` = `Mean WTP Treated`
  )


# *************************************************************
#### Step 5: Export results ####
# *************************************************************


## As we have a "," in the table, let's separate with something else
table_wide %>% fwrite(sep = "#",
                      quote = FALSE,
                      paste0(here(
                        "Outputs", "Tables",
                        "Meadows_TableC5_PoeTestsGroupedType.txt"
                      )))
