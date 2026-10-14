#### Meadows: Entropy balancing weights  ###############
# Function: Estimate entropy-balancing (EB) weights for the Control/Annual/Perennial
#   meadow-type groups (and a Control-vs-Other binary robustness version), then
#   export the weighted dataset used throughout the pipeline plus the balance
#   diagnostics behind Tables B.4 and B.5.
# Author: Dr Peter King (p.king1@leeds.ac.uk)
# Last Edited: 21/05/2025
# Session info: see README.md ("Replication Information")


## Libraries here: -----------------------------------------------------------------
## Setting up all relevant libraries
library(data.table)
library(magrittr)
library(dplyr)
library(tidyverse)
library(here)
library(janitor)
library(apollo)
library(ggplot2)
library(rstatix)
library(stats)
library(mvtnorm)
library(MatchIt)
library(WeightIt)
library(cobalt)
library(sessioninfo)


# **********************************************************************************
#### Section One: Import Data ####
# **********************************************************************************


## Read in latest version of data in wide format
database <- here("Data", "database_Meadows_Anonymised.csv") %>% 
  fread() %>% 
  data.frame() 




# **********************************************************************************
#### Section Two: Setup Data For Analysis ####
# **********************************************************************************

## Convert to numeric
database$Treatment <- ifelse(
  database$Control == 1,0, 
  ifelse(
    database$Annual == 1,1, 
    ifelse(
      database$Perennial == 1,2,3)))

## Pool annual and perennial
database$Treatment_Binary <- ifelse(
  database$Control == 1, 0, 1)



## Take one row per respondent
Data_Long <- database %>% dplyr::filter(Task == 1)
## Was filtering like this but poorly motivated
# Data_Long <- database[seq(1, nrow(database), 6), ]


## Trim missing values that are coded as -999
Data_Long_Trimmed <- Data_Long[!is.na(Data_Long$AgeLevel), ]
Data_Long_Trimmed <- Data_Long_Trimmed[Data_Long_Trimmed$Income > 0, ]
Data_Long_Trimmed <- Data_Long_Trimmed[Data_Long_Trimmed$Education > 0, ]


## Recode as dummy
Data_Long_Trimmed$AgeDummy <-
  ifelse(Data_Long_Trimmed$AgeLevel > median(Data_Long_Trimmed$AgeLevel),
         1,
         0)

## Convert back to characters
Data_Long_Trimmed$Treatment <- ifelse(
  Data_Long_Trimmed$Control == 1,"Control", 
  ifelse(
    Data_Long_Trimmed$Annual == 1,"Annual", 
    ifelse(
      Data_Long_Trimmed$Perennial == 1,"Perennial",3)))


## Binary version pools Annual+Perennial into "Other" for the robustness weights below
Data_Long_Trimmed$Treatment_Binary <- ifelse(
  Data_Long_Trimmed$Control == 1,
  "Control",
  "Other")


## Repeating for database object for use in APOLLO:
database_Trimmed <- database[!is.na(database$AgeLevel),]
database_Trimmed <- database_Trimmed[database_Trimmed$Income>0,]
database_Trimmed <- database_Trimmed[database_Trimmed$Education>0,]
database_Trimmed$AgeDummy <- ifelse(database_Trimmed$AgeLevel>median(database_Trimmed$AgeLevel),
                                    1,0)


## Flag (not currently used downstream) respondents who chose the status-quo
## alternative (alt == 4) in every task
database_Trimmed <- database_Trimmed %>%
  group_by(ID) %>%
  mutate(always_sq = as.integer(all(alt == 4))) %>%
  ungroup()



# **********************************************************************************
#### Section Three: Estimate and report weights ####
# **********************************************************************************

## Fit ternary (Control/Annual/Perennial) EB weights
## NOTE: Entropy Balancing for consistency with Hynes et al 2020, 2021
EB_Weights <- weightit(Treatment ~ Income + AgeDummy +Education +Employment+ Gender,
                       data = Data_Long_Trimmed, method = "ebal", 
                       estimand = "ATE")

## WeightIt >= 1.5 rescales ATE weights to sum to N within each treatment group; the
## published models used WeightIt 1.4.0 weights with mean 1 within each group.
## Re-normalise so the exported weights do not depend on the installed WeightIt version.
EB_Weights$weights <- ave(EB_Weights$weights, EB_Weights$treat, FUN = function(w) w / mean(w))

summary(EB_Weights)
bal.tab(EB_Weights)


EB_Weights_Summary <- bal.tab(
  EB_Weights,
  stats = c("mean.diffs"),
  thresholds = c(m = 0.01),
  disp = c("means"),
  un = TRUE
)


create_balance_table <- function(EB_Weights_Summary) {
  # Discover pair names dynamically — cobalt names them alphabetically (e.g. "Annual vs. Control")
  # so hardcoding is fragile across versions
  pair_names <- names(EB_Weights_Summary$Pair.Balance)
  ac_name    <- pair_names[grepl("Annual",    pair_names, fixed = TRUE) &
                             grepl("Control",   pair_names, fixed = TRUE)]
  ## Table B.4 reports Perennial vs Control (not Annual vs Perennial)
  ap_name    <- pair_names[grepl("Perennial", pair_names, fixed = TRUE) &
                             grepl("Control",   pair_names, fixed = TRUE)]
  
  # Variable names live in rownames, not in $Type (which gives statistic type e.g. "Contin.")
  var_names <- rownames(EB_Weights_Summary$Balance.Across.Pairs)
  
  result_df <- data.frame(
    Variable            = character(),
    Metric              = character(),
    `Annual vs Control`   = character(),
    `Perennial vs Control` = character(),
    stringsAsFactors    = FALSE,
    check.names         = FALSE
  )
  
  for (i in seq_along(var_names)) {
    ac_pre  <- EB_Weights_Summary$Pair.Balance[[ac_name]]$Balance$Diff.Un[i]
    ac_post <- EB_Weights_Summary$Pair.Balance[[ac_name]]$Balance$Diff.Adj[i]
    ap_pre  <- EB_Weights_Summary$Pair.Balance[[ap_name]]$Balance$Diff.Un[i]
    ap_post <- EB_Weights_Summary$Pair.Balance[[ap_name]]$Balance$Diff.Adj[i]
    
    result_df <- rbind(result_df,
                       data.frame(
                         Variable            = var_names[i],
                         Metric              = "Pre-balancing mean difference",
                         `Annual vs Control`   = as.character(round(as.numeric(ac_pre), 2)),
                         `Perennial vs Control` = as.character(round(as.numeric(ap_pre), 2)),
                         stringsAsFactors    = FALSE, check.names = FALSE
                       ))
    
    result_df <- rbind(result_df,
                       data.frame(
                         Variable            = var_names[i],
                         Metric              = "Post-balancing mean difference",
                         `Annual vs Control`   = ifelse(abs(as.numeric(ac_post)) < 0.005, "<0.01",
                                                        as.character(round(as.numeric(ac_post), 2))),
                         `Perennial vs Control` = ifelse(abs(as.numeric(ap_post)) < 0.005, "<0.01",
                                                        as.character(round(as.numeric(ap_post), 2))),
                         stringsAsFactors    = FALSE, check.names = FALSE
                       ))
  }
  
  return(result_df)
}



## Apply function to our summary object
balance_table <- create_balance_table(EB_Weights_Summary)


## Nice to specify variable names
## Rep 2 for pre+post
balance_table$Variable <- c("Income", 
                            "Age", 
                            "Education",
                            "Employment",
                            "Gender") %>% rep(., each = 2)


## Cosmetic change just reordering
TableX_Types <- balance_table %>% arrange(Variable)


create_full_balance_table <- function(weights_obj, data, var_labels) {
  
  # -- Effective sample sizes: stored in $Observations (row 1 = unadjusted, row 2 = adjusted) --
  ess <- bal.tab(weights_obj)$Observations
  
  ess_rows <- data.frame(
    Variable  = "Effective sample size",
    Measure   = c("N. participants unadjusted", "N. participants adjusted"),
    Control   = c(as.character(round(ess[1, "Control"])),
                  as.character(round(ess[2, "Control"]))),
    Annual    = c(as.character(round(ess[1, "Annual"])),
                  as.character(round(ess[2, "Annual"]))),
    Perennial = c(as.character(round(ess[1, "Perennial"])),
                  as.character(round(ess[2, "Perennial"]))),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  
  # -- Pairwise balance statistics --
  bt         <- bal.tab(weights_obj, stats = "mean.diffs", disp = "means", un = TRUE)
  pair_names <- names(bt$Pair.Balance)
  
  # Pair containing Annual and Control (not Perennial)
  ac_name <- pair_names[grepl("Annual",     pair_names, fixed = TRUE) &
                          grepl("Control",   pair_names, fixed = TRUE) &
                          !grepl("Perennial",pair_names, fixed = TRUE)]
  
  # Pair containing Perennial and Control (not Annual)
  pc_name <- pair_names[grepl("Perennial",  pair_names, fixed = TRUE) &
                          grepl("Control",   pair_names, fixed = TRUE) &
                          !grepl("Annual",   pair_names, fixed = TRUE)]
  
  # cobalt places the non-reference group as M.1, reference as M.0.
  # In "Control vs. Annual": M.0=Control, M.1=Annual -> Diff = Annual - Control.
  # In "Control vs. Perennial": M.0=Control, M.1=Perennial -> Diff = Perennial - Control.
  # Both are already in the (treatment - control) direction we want; no sign flip needed.
  
  var_names <- rownames(bt$Balance.Across.Pairs)
  
  # -- Formatters --
  fmt2 <- function(x) {
    x <- as.numeric(x)
    if (is.na(x)) return("")
    as.character(round(x, 2))
  }
  
  fmt_mean <- function(x, is_income) {
    x <- as.numeric(x)
    if (is.na(x)) return("")
    if (is_income) return(paste0("£", formatC(round(x, 2), format = "f",
                                              digits = 2, big.mark = ",")))
    fmt2(x)
  }
  
  fmt_post <- function(x) {
    x <- as.numeric(x)
    if (is.na(x)) return("")
    if (abs(x) < 0.005) return("<0.00")
    fmt2(x)
  }
  
  # -- Sort variables alphabetically by display label --
  var_labels_for_sort <- sapply(var_names, function(v) if (v %in% names(var_labels)) var_labels[[v]] else v)
  var_names <- var_names[order(var_labels_for_sort)]
  
  # -- Per-variable rows (index by variable name, not position) --
  result_df <- ess_rows
  
  for (v in var_names) {
    label  <- if (v %in% names(var_labels)) var_labels[[v]] else v
    is_inc <- identical(v, "Income")
    
    ac_bal <- bt$Pair.Balance[[ac_name]]$Balance
    pc_bal <- bt$Pair.Balance[[pc_name]]$Balance
    
    result_df <- rbind(result_df, data.frame(
      Variable  = label, Measure = "Mean",
      Control   = fmt_mean(mean(data[[v]][data$Treatment == "Control"],   na.rm = TRUE), is_inc),
      Annual    = fmt_mean(mean(data[[v]][data$Treatment == "Annual"],     na.rm = TRUE), is_inc),
      Perennial = fmt_mean(mean(data[[v]][data$Treatment == "Perennial"],  na.rm = TRUE), is_inc),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
    
    result_df <- rbind(result_df, data.frame(
      Variable  = label, Measure = "Variance",
      Control   = fmt2(var(data[[v]][data$Treatment == "Control"],   na.rm = TRUE)),
      Annual    = fmt2(var(data[[v]][data$Treatment == "Annual"],     na.rm = TRUE)),
      Perennial = fmt2(var(data[[v]][data$Treatment == "Perennial"],  na.rm = TRUE)),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
    
    result_df <- rbind(result_df, data.frame(
      Variable  = label, Measure = "Pre-balancing difference vs control",
      Control   = "",
      Annual    = fmt2(ac_bal[v, "Diff.Un"]),
      Perennial = fmt2(pc_bal[v, "Diff.Un"]),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
    
    result_df <- rbind(result_df, data.frame(
      Variable  = label, Measure = "Post-balancing difference vs control",
      Control   = "",
      Annual    = fmt_post(ac_bal[v, "Diff.Adj"]),
      Perennial = fmt_post(pc_bal[v, "Diff.Adj"]),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
  }
  
  result_df
}


## Binary equivalent of create_balance_table(): cobalt returns a single flat
## $Balance data frame for a 2-level treatment (no $Pair.Balance), so this needs
## its own reader rather than reusing the ternary function above
create_balance_table_binary <- function(bal_tab_bin) {
  bal <- bal_tab_bin$Balance
  var_names <- rownames(bal)

  result_df <- data.frame(
    Variable = character(), Metric = character(),
    `Control vs Other` = character(),
    stringsAsFactors = FALSE, check.names = FALSE
  )

  for (v in var_names) {
    result_df <- rbind(result_df, data.frame(
      Variable = v, Metric = "Pre-balancing mean difference",
      `Control vs Other` = as.character(round(bal[v, "Diff.Un"], 2)),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
    result_df <- rbind(result_df, data.frame(
      Variable = v, Metric = "Post-balancing mean difference",
      `Control vs Other` = ifelse(abs(bal[v, "Diff.Adj"]) < 0.005, "<0.01",
                                  as.character(round(bal[v, "Diff.Adj"], 2))),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
  }

  result_df
}


# **********************************************************************************
#### Section Four: Binary treatment weights (Control vs Other, robustness) ####
# **********************************************************************************

## Uses Treatment_Binary (Control vs Other), not Treatment, so this is a genuinely
## distinct robustness re-weighting rather than a repeat of the Section Three fit
EB_Weights_Binary <- weightit(Treatment_Binary ~ Income + AgeDummy +Education +Employment+ Gender,
                              data = Data_Long_Trimmed, method = "ebal",
                              estimand = "ATE")
EB_Weights_Binary$weights <- ave(EB_Weights_Binary$weights, EB_Weights_Binary$treat,
                                 FUN = function(w) w / mean(w))  # see note in Section Three

summary(EB_Weights_Binary)
bal.tab(EB_Weights_Binary)



EB_Weights_Binary_Summary <- bal.tab(
  EB_Weights_Binary,
  stats = c("mean.diffs"),
  thresholds = c(m = 0.01),
  disp = c("means"),
  un = TRUE
)



balance_table_Binary <- create_balance_table_binary(EB_Weights_Binary_Summary)


## rownames(bal.tab$Balance) come out in fit order (Income, AgeDummy, Education,
## Employment, Gender); relabel to match the ternary table's display names
balance_table_Binary$Variable <- c("Income",
                                   "Age",
                                   "Education",
                                   "Employment",
                                   "Gender") %>% rep(., each = 2)


TableX_Types_Binary <- balance_table_Binary %>% arrange(Variable)


# **********************************************************************************
#### Section Five: Combine weights ####
# **********************************************************************************

## Each respondent has 6 rows in the long-format choice data (one per task), so their
## respondent-level weight is repeated across all 6 rows here
database_Trimmed_Weighted <-  cbind(database_Trimmed,

                                    "Weights" = rep(EB_Weights$weights,
                                                    each = 6),

                                    "Weights_Binary" = rep(EB_Weights_Binary$weights,
                                                           each = 6))


# **********************************************************************************
#### Section Six: Export Data ####
# **********************************************************************************


## Export weighted datafrmae for use in estimation:
database_Trimmed_Weighted %>% 
  data.frame() %>% 
  fwrite(
    sep = ",",
    here("Data", "database_Meadows_Anonymised_Weighted.csv")
  )



## Export balance:
TableX_Types %>%
  data.frame() %>%
  fwrite(
    sep = ",",
    here("Outputs", "Tables", "Meadows_TableB4Input_EBWeightSummary.txt")
  )


## Export balance from robustness tests:
TableX_Types_Binary %>%
  data.frame() %>%
  fwrite(
    sep = ",",
    here("Outputs", "Tables", "Meadows_TableB4Input_EBWeightSummary_Binary.txt")
  )


# **********************************************************************************
#### Section Seven: Full balance summary table (Table B5) ####
# **********************************************************************************


full_balance_table <- create_full_balance_table(
  weights_obj = EB_Weights,
  data        = Data_Long_Trimmed,
  var_labels  = c(
    AgeDummy   = "Age group",
    Education  = "Education",
    Employment = "Employment",
    Gender     = "Gender",
    Income     = "Income"
  )
)


full_balance_table %>%
  data.frame() %>%
  fwrite(sep = ",", 
         here("Outputs", "Tables", "Meadows_TableB5_EBWeightSummary.txt"))


#### END OF SCRIPT  **********************************************************************************