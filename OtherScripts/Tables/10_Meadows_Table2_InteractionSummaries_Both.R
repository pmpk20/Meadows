#### Meadows: Table 2 - meadow-type interaction terms ###############
# Script author: Peter King (p.king1@leeds.ac.uk)
# Last Edited: 21/05/2025
# Function: Table 2 - meadow type (Annual/Perennial) x attribute-level interaction
#   terms from the weighted (02) and unweighted (03) correlated MXL models,
#   reported separately as two tables.

# *************************************************************************
#### Section 0: Import model estimates ####
# *************************************************************************


library(apollo)
library(data.table)
library(here)
library(tidyverse)


# *************************************************************************
#### Section 1: Importing Model,  Estimates,  and WTP for FULL SAMPLE models ####
# *************************************************************************

# *************************************************************************
#### Models
Model_Weighted <-
  here("ModelOutputs/Meadows_Pooled_Weighted_ParkFEs_V1_model.rds") %>%
  readRDS()

Model_Unweighted <-
  here("ModelOutputs/Meadows_Pooled_Unweighted_ParkFEs_V1_model.rds") %>%
  readRDS()



# *************************************************************************
#### Estimates
Estimates_Weighted <-
  here("ModelOutputs",
       "Meadows_Pooled_Weighted_ParkFEs_V1_estimates.csv") %>%
  fread() %>%
  data.frame()

Estimates_Unweighted <-
  here("ModelOutputs",
       "Meadows_Pooled_Unweighted_ParkFEs_V1_estimates.csv") %>%
  fread() %>%
  data.frame()




# *************************************************************************
#### Section 2: Model Summary Function ####
# *************************************************************************



## So this code outputs a table of estimate,  p.v stars and s.e in brackets ##
### To make it easy,  just change the model name here and the code will output the table for your model:
ModelOutputs <- function(Estimates) {
  ## Two-sided robust p-value from the t-ratio: Apollo >= 0.3 writes a one-sided
  ## Rob.p-val(0), older versions two-sided, so recompute for consistent stars
  Estimates$Rob.p.val.0. <- 2 * pnorm(-abs(Estimates$Rob.t.ratio.0.))
  data.frame("Variable" =  Estimates$V1,
             "Estimate" =  paste(
               ifelse(
                 Estimates$Rob.p.val.0. < 0.01,
                 paste0(Estimates$Estimate %>% round(3) %>% sprintf("%.3f", .),  "***"),
                 ifelse(
                   Estimates$Rob.p.val.0. < 0.05,
                   paste0(Estimates$Estimate %>% round(3) %>% sprintf("%.3f", .),  "**"),
                   ifelse(
                     Estimates$Rob.p.val.0. < 0.1,
                     paste0(Estimates$Estimate %>% round(3) %>% sprintf("%.3f", .),  "*"),
                     paste0(Estimates$Estimate %>% round(3) %>% sprintf("%.3f", .))
                   )
                 )
               ),
               paste0(
                 "(",
                 Estimates$Rob.std.err %>% round(3) %>% sprintf("%.3f", .),
                 ")"
               )
             ))
}


## New function to take model and report model stats
Diagnostics <- function(Model) {
  rbind(
    "N" = Model$nIndivs,
    "AIC" = Model$AIC %>% round(3) %>% sprintf("%.3f", .),
    "BIC" = Model$BIC %>% round(3) %>% sprintf("%.3f", .),
    "Adj.R2" = Model$adjRho2_C %>% round(3) %>% sprintf("%.3f", .),
    "LogLik" = Model$LLout %>% as.numeric() %>% round(3) %>% sprintf("%.3f", .)
  )
}


# *************************************************************************
#### Section 3A: Output Type1  part of the table ####
# *************************************************************************


## Store model outputs for easy organisation
Output_Weighted <- ModelOutputs(Estimates_Weighted)
## Change rownames to variable for easy reference
rownames(Output_Weighted) <- Output_Weighted$Variable

attributes <-
  c(
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


Table5_Interactions_Weighted <- Output_Weighted %>%
  filter(grepl("^b_type(Annual|Perennial)_", Variable)) %>%
  # Create clean names by removing prefix and standardizing
  mutate(
    type = case_when(
      grepl("Annual", Variable) ~ "Annual",
      grepl("Perennial", Variable) ~ "Perennial"
    ),
    attribute = case_when(
      grepl("Plants_5", Variable) ~ "Plants_5",
      grepl("Plants_10", Variable) ~ "Plants_10",
      grepl("Native_Decrease", Variable) ~ "Native_Decrease",
      grepl("Native_Increase", Variable) ~ "Native_Increase",
      grepl("PollinatorQuality_Medium", Variable) ~ "PollinatorQuality_Medium",
      grepl("PollinatorQuality_High", Variable) ~ "PollinatorQuality_High",
      grepl("Appearance_WYBPR", Variable) ~ "Appearance_WYBPR",
      grepl("Appearance_WYB", Variable) ~ "Appearance_WYB",
      grepl("Appearance_W$", Variable) ~ "Appearance_W"
    )) %>%
  dplyr::select(attribute, type, Estimate) %>%
  pivot_wider(
    names_from = type,
    values_from = Estimate
  ) %>%
  mutate(attribute = factor(attribute, levels = attributes)) %>%
  arrange(attribute)




# *************************************************************************
#### Section 3A: Output Type2  part of the table ####
# *************************************************************************


## Store model outputs for easy organisation
Output_Unweighted <- ModelOutputs(Estimates_Unweighted)
## Change rownames to variable for easy reference
rownames(Output_Unweighted) <- Output_Unweighted$Variable


Table5_Interactions_Unweighted <- Output_Unweighted %>%
  filter(grepl("^b_type(Annual|Perennial)_", Variable)) %>%
  # Create clean names by removing prefix and standardizing
  mutate(
    type = case_when(
      grepl("Annual", Variable) ~ "Annual",
      grepl("Perennial", Variable) ~ "Perennial"
    ),
    attribute = case_when(
      grepl("Plants_5", Variable) ~ "Plants_5",
      grepl("Plants_10", Variable) ~ "Plants_10",
      grepl("Native_Decrease", Variable) ~ "Native_Decrease",
      grepl("Native_Increase", Variable) ~ "Native_Increase",
      grepl("PollinatorQuality_Medium", Variable) ~ "PollinatorQuality_Medium",
      grepl("PollinatorQuality_High", Variable) ~ "PollinatorQuality_High",
      grepl("Appearance_WYBPR", Variable) ~ "Appearance_WYBPR",
      grepl("Appearance_WYB", Variable) ~ "Appearance_WYB",
      grepl("Appearance_W$", Variable) ~ "Appearance_W"
    )) %>%
  dplyr::select(attribute, type, Estimate) %>%
  pivot_wider(
    names_from = type,
    values_from = Estimate
  ) %>%
  mutate(attribute = factor(attribute, levels = attributes)) %>%
  arrange(attribute)




# *************************************************************************
#### Section 4: Export weighted tables ####
# *************************************************************************


## Output to screen in a nice format for making tables in word
Table5_Interactions_Weighted %>% write.csv(quote = FALSE)


## Output to a discrete file if that's helpful
Table5_Interactions_Weighted %>% fwrite(
  sep = ",",
  here(
    "Outputs", "Tables",
    "Meadows_Table2_InteractionSummaries_Weighted.txt"
  ),
  row.names = TRUE,
  quote = FALSE
)

# *************************************************************************
#### Section 5: Output tables ####
# *************************************************************************



## Output to screen in a nice format for making tables in word
Table5_Interactions_Unweighted %>% write.csv(quote = FALSE)


## Output to a discrete file if that's helpful
Table5_Interactions_Unweighted %>% fwrite(
  sep = ",",
  here(
    "Outputs", "Tables",
    "Meadows_Table2_InteractionSummaries_Unweighted.txt"
  ),
  row.names = TRUE,
  quote = FALSE
)


# *************************************************************************
#### END OF SCRIPT ####
# *************************************************************************
