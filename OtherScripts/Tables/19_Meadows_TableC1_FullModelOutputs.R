#### Meadows: Table C.1 - full model parameter output ###############
# Script author: Peter King (p.king1@leeds.ac.uk)
# Last Edited: 21/05/2025
# Function: Table C.1 - every parameter (all attributes, correlations,
#   sociodemographics, park fixed effects) from the weighted (02) and
#   unweighted (03) correlated MXL models. Full-length counterpart to
#   the selected-parameter Table 1 (script 09).

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
#### Section 3A: Output Weighted part of the table ####
# *************************************************************************


## Store model outputs for easy organisation
Output_Weighted <- ModelOutputs(Estimates_Weighted)
## Change rownames to variable for easy reference
rownames(Output_Weighted) <- Output_Weighted$Variable

# 
# Table_Weighted <-
#   bind_cols(
#     "Mean" =
#       bind_rows(
#         Output_Weighted["asc_D", 1:2],
#         Output_Weighted["b_Cost", 1:2],
#         Output_Weighted["mu_Appearance_W", 1:2],
#         Output_Weighted["mu_Appearance_WYB", 1:2],
#         Output_Weighted["mu_Appearance_WYBPR", 1:2],
#         Output_Weighted["mu_NativeDecrease", 1:2],
#         Output_Weighted["mu_NativeIncrease", 1:2],
#         Output_Weighted["mu_Plants5", 1:2],
#         Output_Weighted["mu_Plants10", 1:2],
#         Output_Weighted["mu_PollinatorQualityMedium", 1:2],
#         Output_Weighted["mu_PollinatorQualityHigh", 1:2],
#         
#         Output_Weighted["beta_AgeDummy", 1:2],
#         Output_Weighted["beta_CNS_ScaleMean", 1:2],
#         Output_Weighted["beta_Gender", 1:2],
#         Output_Weighted["beta_IncomeDummy", 1:2],
#         Output_Weighted["beta_visittime", 1:2],     
#         Output_Weighted["beta_freqv", 1:2],
#         Output_Weighted["b_TypeAnnual", 1:2],                                
#         Output_Weighted["b_TypePerennial", 1:2],                             
#         Output_Weighted["b_city_C1", 1:2],                                   
#         Output_Weighted["b_city_C2", 1:2] 
#       ),
#     
#     "Standard Deviation" =
#       bind_rows(
#         Output_Weighted["asc_D", 1:2],
#         Output_Weighted["b_Cost", 1:2],
#         Output_Weighted["sig_Appearance_W", 1:2],
#         Output_Weighted["sig_Appearance_WYB", 1:2],
#         Output_Weighted["sig_Appearance_WYBPR", 1:2],
#         Output_Weighted["sig_NativeDecrease", 1:2],
#         Output_Weighted["sig_NativeIncrease", 1:2],
#         Output_Weighted["sig_Plants5", 1:2],
#         Output_Weighted["sig_Plants10", 1:2],
#         Output_Weighted["sig_PollinatorQualityMedium", 1:2],
#         Output_Weighted["sig_PollinatorQualityHigh", 1:2],
#         
#         Output_Weighted["beta_AgeDummy", 1:2],
#         Output_Weighted["beta_CNS_ScaleMean", 1:2],
#         Output_Weighted["beta_Gender", 1:2],
#         Output_Weighted["beta_IncomeDummy", 1:2],
#         Output_Weighted["beta_visittime", 1:2],     
#         Output_Weighted["beta_freqv", 1:2],
#         Output_Weighted["b_TypeAnnual", 1:2],                                
#         Output_Weighted["b_TypePerennial", 1:2],                             
#         Output_Weighted["b_city_C1", 1:2],                                   
#         Output_Weighted["b_city_C2", 1:2] 
#       )
#   )



# *************************************************************************
#### Section 3B: Output Weighted part of the table ####
# *************************************************************************


## Store model outputs for easy organisation
Output_Unweighted <- ModelOutputs(Estimates_Unweighted)
## Change rownames to variable for easy reference
rownames(Output_Unweighted) <- Output_Unweighted$Variable


# 
# Table_Unweighted <-
#   bind_cols(
#     "Mean" =
#       bind_rows(
#         Output_Unweighted["asc_D", 1:2],
#         Output_Unweighted["b_Cost", 1:2],
#         Output_Unweighted["mu_Appearance_W", 1:2],
#         Output_Unweighted["mu_Appearance_WYB", 1:2],
#         Output_Unweighted["mu_Appearance_WYBPR", 1:2],
#         Output_Unweighted["mu_NativeDecrease", 1:2],
#         Output_Unweighted["mu_NativeIncrease", 1:2],
#         Output_Unweighted["mu_Plants5", 1:2],
#         Output_Unweighted["mu_Plants10", 1:2],
#         Output_Unweighted["mu_PollinatorQualityMedium", 1:2],
#         Output_Unweighted["mu_PollinatorQualityHigh", 1:2],
#         
#         Output_Unweighted["beta_AgeDummy", 1:2],
#         Output_Unweighted["beta_CNS_ScaleMean", 1:2],
#         Output_Unweighted["beta_Gender", 1:2],
#         Output_Unweighted["beta_IncomeDummy", 1:2],
#         Output_Unweighted["beta_visittime", 1:2],     
#         Output_Unweighted["beta_freqv", 1:2],
#         Output_Unweighted["b_TypeAnnual", 1:2],                                
#         Output_Unweighted["b_TypePerennial", 1:2],                             
#         Output_Unweighted["b_city_C1", 1:2],                                   
#         Output_Unweighted["b_city_C2", 1:2] 
#       ),
#     
#     "Standard Deviation" =
#       bind_rows(
#         Output_Unweighted["asc_D", 1:2],
#         Output_Unweighted["b_Cost", 1:2],
#         Output_Unweighted["sig_Appearance_W", 1:2],
#         Output_Unweighted["sig_Appearance_WYB", 1:2],
#         Output_Unweighted["sig_Appearance_WYBPR", 1:2],
#         Output_Unweighted["sig_NativeDecrease", 1:2],
#         Output_Unweighted["sig_NativeIncrease", 1:2],
#         Output_Unweighted["sig_Plants5", 1:2],
#         Output_Unweighted["sig_Plants10", 1:2],
#         Output_Unweighted["sig_PollinatorQualityMedium", 1:2],
#         Output_Unweighted["sig_PollinatorQualityHigh", 1:2],
#         
#         Output_Unweighted["beta_AgeDummy", 1:2],
#         Output_Unweighted["beta_CNS_ScaleMean", 1:2],
#         Output_Unweighted["beta_Gender", 1:2],
#         Output_Unweighted["beta_IncomeDummy", 1:2],
#         Output_Unweighted["beta_visittime", 1:2],     
#         Output_Unweighted["beta_freqv", 1:2],
#         Output_Unweighted["b_TypeAnnual", 1:2],                                
#         Output_Unweighted["b_TypePerennial", 1:2],                             
#         Output_Unweighted["b_city_C1", 1:2],                                   
#         Output_Unweighted["b_city_C2", 1:2] 
#       )
#   )



# *************************************************************************
#### Section 4: Create tables ####
# *************************************************************************

# 
# ## Top part with mean and SDs
# TableC3_Top <- bind_cols(
#   "Variable" = Table_Weighted[, 1] %>% 
#     gsub(pattern = "mu_", replacement = ""),
#   "Mean_Weighted" = Table_Weighted[, 2],
#   "SD_Weighted" = Table_Weighted[, 4],
#   
#   "Mean_Unweighted" = Table_Unweighted[, 2],
#   "SD_Unweighted" = Table_Unweighted[, 4]
# )
# 
# 
# 
# ## New reveiwer-requested bottom row
# TableC3_Bottom <-  bind_cols(
#   "Variable" = c("N", "AIC", "Adj.R2", "LogLike"),
#   "Mean_Weighted" = Diagnostics(Model_Weighted)[, 1],
#   "SD_Weighted" = 0,
#   
#   "Mean_Unweighted" = Diagnostics(Model_Unweighted)[, 1],
#   "SD_Unweighted" = 0
# )
# 

TableC3_Diagnostics <- data.frame(
  "Variable" = Diagnostics(Model_Weighted) %>% rownames(),
  "Estimate" = Diagnostics(Model_Weighted),
  "Variable" = Diagnostics(Model_Weighted) %>% rownames(),
  "Estimate" = Diagnostics(Model_Unweighted)
)

TableC3_Results <- cbind(Output_Weighted,
                Output_Unweighted)

colnames(TableC3_Diagnostics) <- colnames(TableC3_Results)

TableC3 <- rbind(TableC3_Results,
                 TableC3_Diagnostics)


# 

# *************************************************************************
#### Section 5: Output tables ####
# *************************************************************************

# 1. define the desired order of the V1‐names
desired_order <- c(
  # --- fixed coefficients
  "asc_D", "b_Cost", "b_TypeAnnual", "b_TypePerennial",
  # --- means
  paste0("mu_", c("Appearance_W",
                  "Appearance_WYB",
                  "Appearance_WYBPR",
                  "NativeDecrease",
                  "NativeIncrease",
                  "Plants5",
                  "Plants10",
                  "PollinatorQualityMedium",
                  "PollinatorQualityHigh")),
  # --- standard deviations
  paste0("sig_", c("Appearance_W",
                   "Appearance_WYB",
                   "Appearance_WYBPR",
                   "NativeDecrease",
                   "NativeIncrease",
                   "Plants5",
                   "Plants10",
                   "PollinatorQualityMedium",
                   "PollinatorQualityHigh")),
  # --- interactions with annual meadows
  paste0("b_typeAnnual_",
         c("Appearance_W",
           "Appearance_WYB",
           "Appearance_WYBPR",
           "Native_Decrease",
           "Native_Increase",
           "Plants_5",
           "Plants_10",
           "PollinatorQuality_Medium",
           "PollinatorQuality_High")),
  # --- interactions with perennial meadows
  paste0("b_typePerennial_",
         c("Appearance_W",
           "Appearance_WYB",
           "Appearance_WYBPR",
           "Native_Decrease",
           "Native_Increase",
           "Plants_5",
           "Plants_10",
           "PollinatorQuality_Medium",
           "PollinatorQuality_High")),
  # --- correlation (sig_NativeDecrease * …, sig_NativeIncrease * …, etc)
  paste0("sig_NativeDecrease_", 
         c("Plants5",
           "Plants10",
           "PollinatorQualityMedium",
           "PollinatorQualityHigh",
           "Appearance_W",
           "Appearance_WYB",
           "Appearance_WYBPR")),
  paste0("sig_NativeIncrease_",
         c("Plants5",
           "Plants10",
           "PollinatorQualityMedium",
           "PollinatorQualityHigh",
           "Appearance_W",
           "Appearance_WYB",
           "Appearance_WYBPR")),
  paste0("sig_Plants5_",
         c("NativeDecrease",
           "NativeIncrease",
           "PollinatorQualityMedium",
           "PollinatorQualityHigh",
           "Appearance_W",
           "Appearance_WYB",
           "Appearance_WYBPR")),
  paste0("sig_Plants10_",
         c("NativeDecrease",
           "NativeIncrease",
           "PollinatorQualityMedium",
           "PollinatorQualityHigh",
           "Appearance_W",
           "Appearance_WYB",
           "Appearance_WYBPR")),
  paste0("sig_PollinatorQualityMedium_",
         c("NativeDecrease",
           "NativeIncrease",
           "Plants5",
           "Plants10",
           "Appearance_W",
           "Appearance_WYB",
           "Appearance_WYBPR")),
  paste0("sig_PollinatorQualityHigh_",
         c("NativeDecrease",
           "NativeIncrease",
           "Plants5",
           "Plants10",
           "Appearance_W",
           "Appearance_WYB",
           "Appearance_WYBPR")),
  paste0("sig_Appearance_W_",
         c("NativeDecrease",
           "NativeIncrease",
           "Plants5",
           "Plants10",
           "PollinatorQualityMedium",
           "PollinatorQualityHigh")),
  paste0("sig_Appearance_WYB_",
         c("NativeDecrease",
           "NativeIncrease",
           "Plants5",
           "Plants10",
           "PollinatorQualityMedium",
           "PollinatorQualityHigh")),
  paste0("sig_Appearance_WYBPR_",
         c("NativeDecrease",
           "NativeIncrease",
           "Plants5",
           "Plants10",
           "PollinatorQualityMedium",
           "PollinatorQualityHigh")),
  # --- covariates
  "beta_AgeDummy",
  "beta_CNS_ScaleMean",
  "beta_Gender",
  "beta_IncomeDummy",
  "beta_visittime",
  "beta_freqv",
  # (if you have Leeds/Bristol dummies, put them here:)
  "b_EC_Park_Burley",
  "b_EC_Park_Gores_Marsh",
  "b_EC_Park_Horfield_Common",
  "b_EC_Park_Inch_Park",
  "b_EC_Park_King_Lane",
  "b_EC_Park_Middleton",
  "b_EC_Park_Pilrig",
  "b_EC_Park_Rodley",
  "b_EC_Park_St_Andrews",
  "b_EC_Park_St_Marks",
  # --- diagnostics
  "N", "AIC", "BIC", "Adj.R2", "LogLik"
)

# 2. reorder, dropping any that didn’t match (or filling with NAs)
TableC3 <- TableC3[match(desired_order, TableC3$Variable), , drop = FALSE]

# 3. (optionally) reset factor levels so any plotting/pprinting respects the new order
TableC3$Variable <- factor(TableC3$Variable, levels = desired_order)

# 4. write out
# write.csv(TableC3, "TableC3_reordered.csv", row.names = FALSE, quote = FALSE)




# *************************************************************************
#### Section 6: Export  ####
# *************************************************************************


## Output to screen in a nice format for making tables in word
TableC3 %>% write.csv(quote = FALSE, row.names = FALSE)


## Output to a discrete file if that's helpful
TableC3 %>% fwrite(
  sep = ",",
  here("Outputs", "Tables", "Meadows_TableC1_FullModelOutputs.txt"),
  row.names = TRUE,
  quote = FALSE
)



# *************************************************************************
#### END OF SCRIPT ####
# *************************************************************************
