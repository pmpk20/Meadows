#### Meadows: Simulate WTP draws (weighted main model) ###############
# AUTHOR: Peter King (p.king1@leeds.ac.uk)
# LAST CHANGE: 21/05/2025
# FUNCTION: Krinsky-Robb simulation of mean WTP per attribute for each meadow type
#   (Control/Annual/Perennial), from the weighted main model's (02) estimates and
#   robust covariance matrix. Produces intermediate draws consumed by the Table 3
#   Poe test script (08) and the Table C.2 WTP-moments script (26) - not a
#   manuscript output itself.
## WARNING: slow (10,000 x 10,000 simulation loop)


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
#### Section 1: Import data ####
# *************************************************************


# Model_Weighted <- here("ModelOutputs", "Meadows_AllTypes_Weighted_FewerInteractions_V2_model.rds") %>%
#   readRDS()

Model_Weighted <- here("ModelOutputs/Meadows_Pooled_Weighted_ParkFEs_V1_model.rds") %>%
  readRDS()


# *************************************************************
#### Section 2: Define simulator function ####
# *************************************************************

Simulator <- function(Model, meadow_type = NULL) {
  
  # Initialize simulation parameters
  N_Reps_KrinskyRobb <- 10000
  N_Reps_Draws <- 10000
  # N_Reps_KrinskyRobb <- 100000
  # N_Reps_Draws <- 1000000
  
  # Define attributes and levels
  names <- c("Appearance_W",
             "Appearance_WYB",
             "Appearance_WYBPR",
             "Native_Decrease",  # Changed to match parameter names
             "Native_Increase",  # Changed to match parameter names
             "Plants_5",         # Changed to match parameter names
             "Plants_10",        # Changed to match parameter names
             "PollinatorQuality_Medium",  # Changed to match parameter names
             "PollinatorQuality_High")    # Changed to match parameter names
  
  # Initialize matrices for WTP and random draws
  wtp_matrices <- lapply(names, function(x) matrix(0, nrow = N_Reps_KrinskyRobb, ncol = 1))
  draw_matrices <- lapply(names, function(x) rnorm(N_Reps_Draws, 0, 1))
  names(wtp_matrices) <- names(draw_matrices) <- names
  
  # Extract model parameters
  NParams <- length(Model$estimate)
  mean_v <- Model$estimate[1:NParams]
  covar <- as.matrix(Model$robvarcov)
  
  # Draw from multivariate normal distribution
  model_draws <- mvtnorm::rmvnorm(n = N_Reps_KrinskyRobb,
                                  mean = mean_v,
                                  sigma = covar)
  model_draws <- as.matrix(model_draws)
  
  # Parameter name mapping
  param_mapping <- list(
    "Appearance_W" = "Appearance_W",
    "Appearance_WYB" = "Appearance_WYB",
    "Appearance_WYBPR" = "Appearance_WYBPR",
    "Native_Decrease" = "NativeDecrease",
    "Native_Increase" = "NativeIncrease",
    "Plants_5" = "Plants5",
    "Plants_10" = "Plants10",
    "PollinatorQuality_Medium" = "PollinatorQualityMedium",
    "PollinatorQuality_High" = "PollinatorQualityHigh"
  )
  
  wtp_results <- vector("list", N_Reps_KrinskyRobb)
  
  # Main simulation loop
  for (i in 1:N_Reps_KrinskyRobb) {
    wtp_results[[i]] <- vector("list", length(names))
    names(wtp_results[[i]]) <- names
    
    for (param in names) {
      # Get base parameter name
      base_param <- param_mapping[[param]]
      
      # Base parameter names
      mu_name <- paste0("mu_", base_param)
      sig_name <- paste0("sig_", base_param)
      
      # Calculate base WTP
      base_wtp <- model_draws[i, mu_name] + 
        model_draws[i, sig_name] * draw_matrices[[param]]
      
      # Add meadow type effects if specified
      if (!is.null(meadow_type)) {
        ## Interaction terms keep the underscored level name (e.g. b_typePerennial_Native_Decrease),
        ## unlike the mu_/sig_ names in param_mapping, so build them from `param`
        type_param <- paste0("b_type", meadow_type, "_", param)
        # type_param <- paste0("b_type", tolower(meadow_type), "_", base_param)
        ## Fail loudly if an interaction name does not exist, rather than silently
        ## dropping the meadow-type effect
        stopifnot(type_param %in% colnames(model_draws))
        if (type_param %in% colnames(model_draws)) {
          type_effect <- model_draws[i, type_param]
          base_wtp <- base_wtp + type_effect
        }
      }
      
      # Store mean WTP for this iteration
      wtp_results[[i]][[param]] <- mean(base_wtp)
    }
  }
  
  # Combine results into a data frame
  Output <- rbindlist(wtp_results, use.names = TRUE, fill = TRUE) %>% 
    data.frame()
  
  # Add metadata columns
  Output$meadow_type <- meadow_type %||% "Control"
  # Output$city <- city %||% "Base"
  
  # Calculate confidence intervals
  summary_stats <- Output %>%
    gather(attribute, value, -meadow_type) %>%
    group_by(meadow_type, attribute) %>%
    summarize(
      mean_wtp = mean(value),
      lower_ci = quantile(value, 0.025),
      upper_ci = quantile(value, 0.975),
      .groups = 'drop'
    )
  
  return(list(
    raw_simulations = Output,
    summary = summary_stats
  ))
}


# *************************************************************
#### Section 3: Run simulator for all types ####
# *************************************************************



## Reproducible Krinsky-Robb parameter draws and individual-level draws
set.seed(13)
meadow_types <- c("Control", "Annual", "Perennial")
# cities <- c("C1", "C2")
results_list <- list()

for (type in meadow_types) {
  key <- paste(type, sep = "_")
  results_list[[key]] <- Simulator(Model = Model_Weighted,
                                   meadow_type = if (type == "Control")
                                     NULL
                                   else
                                     type)
}


# Combine all raw simulations into a single dataframe
all_results <- map_dfr(results_list, ~ .x$raw_simulations)



# *************************************************************
#### Section 5: Export data ####
# *************************************************************



all_results %>% fwrite(sep = ",",
                       quote = FALSE,
                       paste0(here(
                         "ModelOutputs",
                         "SimulatedMeans_AllTypes_Weighted.csv"
                       )))

