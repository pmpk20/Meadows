#### Meadows: Table 1 - main model parameter summary ###############
# Script author: Peter King (p.king1@leeds.ac.uk)
# Last Edited: 01/10/2024
# Function: Table 1 - selected parameters (means, SDs, meadow-type effects,
#   diagnostics) from the weighted (02) and unweighted (03) correlated MXL
#   models, formatted as "Estimate (SE)" with significance stars.

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



## Formats "Estimate*** (SE)" per row. Uses Rob.std.err/Rob.p.val.0. (robust/
## sandwich SEs) throughout, not the classical Std.err. column also present in
## Estimates - keep this consistent with whichever SE type the manuscript reports
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


Table_Weighted <-
  bind_cols(
    "Mean" =
      bind_rows(
        Output_Weighted["asc_D", 1:2],
        Output_Weighted["b_Cost", 1:2],
        Output_Weighted["mu_Appearance_W", 1:2],
        Output_Weighted["mu_Appearance_WYB", 1:2],
        Output_Weighted["mu_Appearance_WYBPR", 1:2],
        Output_Weighted["mu_NativeDecrease", 1:2],
        Output_Weighted["mu_NativeIncrease", 1:2],
        Output_Weighted["mu_Plants5", 1:2],
        Output_Weighted["mu_Plants10", 1:2],
        Output_Weighted["mu_PollinatorQualityMedium", 1:2],
        Output_Weighted["mu_PollinatorQualityHigh", 1:2],
        
        Output_Weighted["beta_AgeDummy", 1:2],
        Output_Weighted["beta_CNS_ScaleMean", 1:2],
        Output_Weighted["beta_Gender", 1:2],
        Output_Weighted["beta_IncomeDummy", 1:2],
        Output_Weighted["beta_visittime", 1:2],     
        Output_Weighted["beta_freqv", 1:2],
        Output_Weighted["b_TypeAnnual", 1:2],                                
        Output_Weighted["b_TypePerennial", 1:2],
        
        Output_Weighted["b_EC_Park_Burley", 1:2],
        Output_Weighted["b_EC_Park_Gores_Marsh", 1:2],
        Output_Weighted["b_EC_Park_Horfield_Common", 1:2],
        Output_Weighted["b_EC_Park_Inch_Park", 1:2],
        Output_Weighted["b_EC_Park_King_Lane", 1:2],
        Output_Weighted["b_EC_Park_Middleton", 1:2],
        Output_Weighted["b_EC_Park_Pilrig", 1:2],
        Output_Weighted["b_EC_Park_Rodley", 1:2],
        Output_Weighted["b_EC_Park_St_Andrews", 1:2],
        Output_Weighted["b_EC_Park_St_Marks", 1:2]
      ),
    
    "Standard Deviation" =
      bind_rows(
        Output_Weighted["asc_D", 1:2],
        Output_Weighted["b_Cost", 1:2],
        Output_Weighted["sig_Appearance_W", 1:2],
        Output_Weighted["sig_Appearance_WYB", 1:2],
        Output_Weighted["sig_Appearance_WYBPR", 1:2],
        Output_Weighted["sig_NativeDecrease", 1:2],
        Output_Weighted["sig_NativeIncrease", 1:2],
        Output_Weighted["sig_Plants5", 1:2],
        Output_Weighted["sig_Plants10", 1:2],
        Output_Weighted["sig_PollinatorQualityMedium", 1:2],
        Output_Weighted["sig_PollinatorQualityHigh", 1:2],
        
        Output_Weighted["beta_AgeDummy", 1:2],
        Output_Weighted["beta_CNS_ScaleMean", 1:2],
        Output_Weighted["beta_Gender", 1:2],
        Output_Weighted["beta_IncomeDummy", 1:2],
        Output_Weighted["beta_visittime", 1:2],     
        Output_Weighted["beta_freqv", 1:2],
        Output_Weighted["b_TypeAnnual", 1:2],                                
        Output_Weighted["b_TypePerennial", 1:2],                             
        Output_Weighted["b_EC_Park_Burley", 1:2],
        Output_Weighted["b_EC_Park_Gores_Marsh", 1:2],
        Output_Weighted["b_EC_Park_Horfield_Common", 1:2],
        Output_Weighted["b_EC_Park_Inch_Park", 1:2],
        Output_Weighted["b_EC_Park_King_Lane", 1:2],
        Output_Weighted["b_EC_Park_Middleton", 1:2],
        Output_Weighted["b_EC_Park_Pilrig", 1:2],
        Output_Weighted["b_EC_Park_Rodley", 1:2],
        Output_Weighted["b_EC_Park_St_Andrews", 1:2],
        Output_Weighted["b_EC_Park_St_Marks", 1:2]
      )
  )



# *************************************************************************
#### Section 3B: Output Weighted part of the table ####
# *************************************************************************


## Store model outputs for easy organisation
Output_Unweighted <- ModelOutputs(Estimates_Unweighted)
## Change rownames to variable for easy reference
rownames(Output_Unweighted) <- Output_Unweighted$Variable



Table_Unweighted <-
  bind_cols(
    "Mean" =
      bind_rows(
        Output_Unweighted["asc_D", 1:2],
        Output_Unweighted["b_Cost", 1:2],
        Output_Unweighted["mu_Appearance_W", 1:2],
        Output_Unweighted["mu_Appearance_WYB", 1:2],
        Output_Unweighted["mu_Appearance_WYBPR", 1:2],
        Output_Unweighted["mu_NativeDecrease", 1:2],
        Output_Unweighted["mu_NativeIncrease", 1:2],
        Output_Unweighted["mu_Plants5", 1:2],
        Output_Unweighted["mu_Plants10", 1:2],
        Output_Unweighted["mu_PollinatorQualityMedium", 1:2],
        Output_Unweighted["mu_PollinatorQualityHigh", 1:2],
        
        Output_Unweighted["beta_AgeDummy", 1:2],
        Output_Unweighted["beta_CNS_ScaleMean", 1:2],
        Output_Unweighted["beta_Gender", 1:2],
        Output_Unweighted["beta_IncomeDummy", 1:2],
        Output_Unweighted["beta_visittime", 1:2],     
        Output_Unweighted["beta_freqv", 1:2],
        Output_Unweighted["b_TypeAnnual", 1:2],                                
        Output_Unweighted["b_TypePerennial", 1:2],                             
        Output_Unweighted["b_EC_Park_Burley", 1:2],
        Output_Unweighted["b_EC_Park_Gores_Marsh", 1:2],
        Output_Unweighted["b_EC_Park_Horfield_Common", 1:2],
        Output_Unweighted["b_EC_Park_Inch_Park", 1:2],
        Output_Unweighted["b_EC_Park_King_Lane", 1:2],
        Output_Unweighted["b_EC_Park_Middleton", 1:2],
        Output_Unweighted["b_EC_Park_Pilrig", 1:2],
        Output_Unweighted["b_EC_Park_Rodley", 1:2],
        Output_Unweighted["b_EC_Park_St_Andrews", 1:2],
        Output_Unweighted["b_EC_Park_St_Marks", 1:2]
      ),
    
    "Standard Deviation" =
      bind_rows(
        Output_Unweighted["asc_D", 1:2],
        Output_Unweighted["b_Cost", 1:2],
        Output_Unweighted["sig_Appearance_W", 1:2],
        Output_Unweighted["sig_Appearance_WYB", 1:2],
        Output_Unweighted["sig_Appearance_WYBPR", 1:2],
        Output_Unweighted["sig_NativeDecrease", 1:2],
        Output_Unweighted["sig_NativeIncrease", 1:2],
        Output_Unweighted["sig_Plants5", 1:2],
        Output_Unweighted["sig_Plants10", 1:2],
        Output_Unweighted["sig_PollinatorQualityMedium", 1:2],
        Output_Unweighted["sig_PollinatorQualityHigh", 1:2],
        
        Output_Unweighted["beta_AgeDummy", 1:2],
        Output_Unweighted["beta_CNS_ScaleMean", 1:2],
        Output_Unweighted["beta_Gender", 1:2],
        Output_Unweighted["beta_IncomeDummy", 1:2],
        Output_Unweighted["beta_visittime", 1:2],     
        Output_Unweighted["beta_freqv", 1:2],
        Output_Unweighted["b_TypeAnnual", 1:2],                                
        Output_Unweighted["b_TypePerennial", 1:2],                             
        Output_Unweighted["b_EC_Park_Burley", 1:2],
        Output_Unweighted["b_EC_Park_Gores_Marsh", 1:2],
        Output_Unweighted["b_EC_Park_Horfield_Common", 1:2],
        Output_Unweighted["b_EC_Park_Inch_Park", 1:2],
        Output_Unweighted["b_EC_Park_King_Lane", 1:2],
        Output_Unweighted["b_EC_Park_Middleton", 1:2],
        Output_Unweighted["b_EC_Park_Pilrig", 1:2],
        Output_Unweighted["b_EC_Park_Rodley", 1:2],
        Output_Unweighted["b_EC_Park_St_Andrews", 1:2],
        Output_Unweighted["b_EC_Park_St_Marks", 1:2]
      )
  )



# *************************************************************************
#### Section 4: Create tables ####
# *************************************************************************


## Top part with mean and SDs
Table2_Top <- bind_cols(
  "Variable" = Table_Weighted[, 1] %>% 
    gsub(pattern = "mu_", replacement = ""),
  "Mean_Weighted" = Table_Weighted[, 2],
  "SD_Weighted" = Table_Weighted[, 4],
  
  "Mean_Unweighted" = Table_Unweighted[, 2],
  "SD_Unweighted" = Table_Unweighted[, 4]
)



## New reveiwer-requested bottom row
Table2_Bottom <-  bind_cols(
  "Variable" = c("N", "AIC", "BIC","Adj.R2", "LogLike"),
  "Mean_Weighted" = Diagnostics(Model_Weighted)[, 1],
  "SD_Weighted" = 0,
  
  "Mean_Unweighted" = Diagnostics(Model_Unweighted)[, 1],
  "SD_Unweighted" = 0
)

Table2 <- rbind(Table2_Top,
                Table2_Bottom)


# *************************************************************************
#### Section 5: Output tables ####
# *************************************************************************


## Output to screen in a nice format for making tables in word
Table2 %>% write.csv(quote = FALSE)


## Output to a discrete file if that's helpful
Table2 %>% fwrite(
  sep = ",",
  here("Outputs", "Tables", "Meadows_Table1_ModelSummaries.txt"),
  row.names = TRUE,
  quote = FALSE
)



# *************************************************************************
#### END OF SCRIPT ####
# *************************************************************************
