#### Meadows Paper: Mixed Logit ####
## Function: Robustness check for 04: identical grouped-type (Annual+Perennial
##   pooled vs Control) correlated WTP-space MXL, but estimated unweighted.
##   Feeds Table C.4 (unweighted).
## Author: Dr Peter King (p.king1@leeds.ac.uk)
## Last change: 26/09/2024
## Session info: see README.md ("Replication Information")

# ****************************
# Setup Environment: ####
# ****************************


## Libraries that will come in handy later
library(apollo)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggridges)
library(reshape2)
library(mded)
library(here)
library(data.table)

# ****************************
# Import Data: ####
# ****************************


database <-
  here("Data", "database_Meadows_Anonymised_Weighted.csv") %>% 
  fread() %>% 
  data.frame()

# ****************************
# Recode/Tidy Data: ####
# ****************************

## Effects coding Leeds
database$City_1_Effect <- ifelse(database$city == 1, 1, 
                                 ifelse(database$city == 3, 
                                        -1,
                                        0))

## Effects coding Bristol
database$City_2_Effect <- ifelse(database$city == 2, 
                                 1, 
                                 ifelse(database$city == 3, 
                                        -1, 
                                        0))

database$City_1_Leeds <- ifelse(database$city == 1, 1, 0)
database$City_2_Bristol <- ifelse(database$city == 2, 1, 0)


## Dummy code as above/below median
database$IncomeDummy <- ifelse(database$Income < median(database$Income),
                               0,
                               1)

## Dummy code as above/below median
database$AgeDummy  <- ifelse(database$AgeLevel <= median(database$AgeLevel, 
                                                         na.rm = TRUE),
                             0,
                             1)


## I just don't like this variable name
database$FrequencyOfVisit <- database$freqv 


## Replace missing with mean
database$CNS_ScaleMean_MeanUpdated <- ifelse(database$CNS_ScaleMean == -999,
                                          database$CNS_ScaleMean[database$CNS_ScaleMean > -1] %>% 
                                            mean(na.rm = TRUE),
                                          database$CNS_ScaleMean)

## Replace NA with 0 
database$visittime <- ifelse(database$visittime %>% is.na(),
                             0, 
                             database$visittime)

database$TypeAnnual <- database$Annual 
database$TypePerennial <- database$Perennial 

## Group here
database$Treated <- ifelse(database$Control == 1, 0, 1)


# ****************************
# Effects-coded parks: ####
# ****************************


# Create a new column for the cleaned park IDs
# park...34/park...35 are duplicate "park" columns auto-renamed by fread on import
# (the raw data has two identical park columns); ...34 is the one used here
database$Clean_Park_ID <- database$park...34

# List of suffixes (and potential prefixes with underscore) to remove
# The regex pattern:
# (?:_?Control|_?Annual|_?Perennial)$
# Explanation:
# (?: ... )  -> non-capturing group
# _?         -> matches an underscore zero or one time (optional underscore)
# Control    -> matches the literal string "Control"
# |          -> OR operator
# $          -> asserts position at the end of the string
suffixes_to_remove <- c("_Control", "_Annual", "_Perennial")    # Without, just in case

# This pattern tries to remove "_Control", "_Annual", "_Perennial" at the end.
database$Clean_Park_ID <- gsub("(?:_Control|_Annual|_Perennial|_Perrenial)$", "", database$Clean_Park_ID)


## Start effects coding
database$Clean_Park_ID <- as.factor(database$Clean_Park_ID)
all_park_levels <- levels(database$Clean_Park_ID)

# Choose a reference level park consistently
ref_level_park <- all_park_levels[length(all_park_levels)] # Or pick one by name
# print(paste("Reference park level for effects coding:", ref_level_park))

parks_to_code <- all_park_levels[all_park_levels != ref_level_park]

for (p in parks_to_code) {
  # Sanitize park names for variable naming
  clean_var_suffix <- gsub("[^A-Za-z0-9_]", "", p) 
  var_name <- paste0("EC_Park_", clean_var_suffix)
  
  database[[var_name]] <- ifelse(database$Clean_Park_ID == p, 1, 
                                 ifelse(database$Clean_Park_ID == ref_level_park, -1, 0))
}



## Preserve only necessary columns
database <- database %>% dplyr::select(c(
  IncomeDummy,
  AgeDummy,
  Gender,
  FrequencyOfVisit,
  CNS_ScaleMean_MeanUpdated,
  visittime,
  EC_Park_Burley,
  EC_Park_Gores_Marsh,
  EC_Park_Horfield_Common,
  EC_Park_Inch_Park,
  EC_Park_King_Lane,
  EC_Park_Middleton,
  EC_Park_Pilrig,
  EC_Park_Rodley,
  EC_Park_St_Andrews,
  EC_Park_St_Marks,
  
  
  Plants_5_A,
  Plants_10_A,
  Native_Decrease_A,
  Native_Increase_A,
  Appearance_W_A,
  Appearance_WYB_A,
  Appearance_WYBPR_A,
  PollinatorQuality_Medium_A,
  PollinatorQuality_High_A,
  
  Plants_5_B,
  Plants_10_B,
  Native_Decrease_B,
  Native_Increase_B,
  Appearance_W_B,
  Appearance_WYB_B,
  Appearance_WYBPR_B,
  PollinatorQuality_Medium_B,
  PollinatorQuality_High_B,
  
  Plants_5_C,
  Plants_10_C,
  Native_Decrease_C,
  Native_Increase_C,
  Appearance_W_C,
  Appearance_WYB_C,
  Appearance_WYBPR_C,
  PollinatorQuality_Medium_C,
  PollinatorQuality_High_C,
  
  Treated,
  Type,
  TypeAnnual,
  TypePerennial,
  costA,
  costB,
  costC,
  ID,
  Weights,
  Weights_Binary,
  choice
))

# ****************************
# Estimation Basics: ####
# ****************************


apollo_initialise()

## Note 10 cores as I'm using the University of Kent 'Tesla' HPC:
apollo_control = list(
  nCores    = 40,
  analyticGrad    = TRUE, # following manual here
  mixing    = TRUE,
  modelDescr = "Meadows_Pooled_Unweighted_GroupedType_ParkFEs_V1",
  modelName  = "Meadows_Pooled_Unweighted_GroupedType_ParkFEs_V1", ## Added dates last verified
  indivID    = "ID", ## This is the name of a column in the database indicating each unique respondent
  outputDirectory = "ModelOutputs"
)


## Define parameters starting values:
### Note starting mean of tax at -3 to avoid issues later when using the lognormal distribution
apollo_beta = c(
  asc_D = -0.36850,
  b_Cost = -0.04160,
  mu_Plants5 = 3.66873,
  mu_Plants10 = 4.65657,
  mu_NativeDecrease = -19.04690,
  mu_NativeIncrease = 8.45058,
  mu_PollinatorQualityMedium = 23.53016,
  mu_PollinatorQualityHigh = 28.07809,
  mu_Appearance_W = 14.11302,
  mu_Appearance_WYB = 36.11471,
  mu_Appearance_WYBPR = 29.45230,
  sig_Plants5 = 18.90769,
  sig_Plants10 = -21.60554,
  sig_NativeDecrease = 31.20684,
  sig_NativeIncrease = 31.62685,
  sig_PollinatorQualityMedium = 46.79121,
  sig_PollinatorQualityHigh = 48.04617,
  sig_Appearance_W = -11.60190,
  sig_Appearance_WYB = 26.89039,
  sig_Appearance_WYBPR = 37.52188,
  
  b_type_Plants_5 = 0,
  b_type_Plants_10 = 0,
  b_type_Native_Decrease = 0,
  b_type_Native_Increase = 0,
  b_type_Appearance_W = 0,
  b_type_Appearance_WYB = 0,
  b_type_Appearance_WYBPR = 0,
  b_type_PollinatorQuality_Medium = 0,
  b_type_PollinatorQuality_High = 0,
  
  sig_NativeDecrease_Plants5 = 0,
  sig_NativeDecrease_Plants10 = 0,
  sig_NativeDecrease_PollinatorQualityMedium = 0,
  sig_NativeDecrease_PollinatorQualityHigh = 0,
  sig_NativeDecrease_Appearance_W = 0,
  sig_NativeDecrease_Appearance_WYB = 0,
  sig_NativeDecrease_Appearance_WYBPR = 0,
  
  sig_NativeIncrease_Plants5 = 0,
  sig_NativeIncrease_Plants10 = 0,
  sig_NativeIncrease_PollinatorQualityMedium = 0,
  sig_NativeIncrease_PollinatorQualityHigh = 0,
  sig_NativeIncrease_Appearance_W = 0,
  sig_NativeIncrease_Appearance_WYB = 0,
  sig_NativeIncrease_Appearance_WYBPR = 0,
  
  sig_Plants5_NativeDecrease = 0,
  sig_Plants5_NativeIncrease = 0,
  sig_Plants5_PollinatorQualityMedium = 0,
  sig_Plants5_PollinatorQualityHigh = 0,
  sig_Plants5_Appearance_W = 0,
  sig_Plants5_Appearance_WYB = 0,
  sig_Plants5_Appearance_WYBPR = 0,
  
  sig_Plants10_NativeDecrease = 0,
  sig_Plants10_NativeIncrease = 0,
  sig_Plants10_PollinatorQualityMedium = 0,
  sig_Plants10_PollinatorQualityHigh = 0,
  sig_Plants10_Appearance_W = 0,
  sig_Plants10_Appearance_WYB = 0,
  sig_Plants10_Appearance_WYBPR = 0,
  
  sig_PollinatorQualityMedium_NativeDecrease = 0,
  sig_PollinatorQualityMedium_NativeIncrease = 0,
  sig_PollinatorQualityMedium_Plants5 = 0,
  sig_PollinatorQualityMedium_Plants10 = 0,
  sig_PollinatorQualityMedium_Appearance_W = 0,
  sig_PollinatorQualityMedium_Appearance_WYB = 0,
  sig_PollinatorQualityMedium_Appearance_WYBPR = 0,
  
  sig_PollinatorQualityHigh_NativeDecrease = 0,
  sig_PollinatorQualityHigh_NativeIncrease = 0,
  sig_PollinatorQualityHigh_Plants5 = 0,
  sig_PollinatorQualityHigh_Plants10 = 0,
  sig_PollinatorQualityHigh_Appearance_W = 0,
  sig_PollinatorQualityHigh_Appearance_WYB = 0,
  sig_PollinatorQualityHigh_Appearance_WYBPR = 0,
  
  sig_Appearance_W_NativeDecrease = 0,
  sig_Appearance_W_NativeIncrease = 0,
  sig_Appearance_W_Plants5 = 0,
  sig_Appearance_W_Plants10 = 0,
  sig_Appearance_W_PollinatorQualityMedium = 0,
  sig_Appearance_W_PollinatorQualityHigh = 0,
  
  sig_Appearance_WYB_NativeDecrease = 0,
  sig_Appearance_WYB_NativeIncrease = 0,
  sig_Appearance_WYB_Plants5 = 0,
  sig_Appearance_WYB_Plants10 = 0,
  sig_Appearance_WYB_PollinatorQualityMedium = 0,
  sig_Appearance_WYB_PollinatorQualityHigh = 0,
  
  sig_Appearance_WYBPR_NativeDecrease = 0,
  sig_Appearance_WYBPR_NativeIncrease = 0,
  sig_Appearance_WYBPR_Plants5 = 0,
  sig_Appearance_WYBPR_Plants10 = 0,
  sig_Appearance_WYBPR_PollinatorQualityMedium = 0,
  sig_Appearance_WYBPR_PollinatorQualityHigh = 0,
  
  beta_IncomeDummy = 0,
  beta_AgeDummy = 0,
  beta_Gender = 0,
  beta_freqv = 0,
  beta_CNS_ScaleMean = 0,
  beta_visittime = 0,
  
  b_type = 0,
  
  ## Dropping city FEs in favour of parks
  # b_city_C1 = 0,
  # b_city_C2 = 0
  
  ## Park FEs but not Stanhope Rec is the reference level
  b_EC_Park_Burley = 0,
  b_EC_Park_Gores_Marsh = 0,
  b_EC_Park_Horfield_Common = 0,
  b_EC_Park_Inch_Park = 0,
  b_EC_Park_King_Lane = 0,
  b_EC_Park_Middleton = 0,
  b_EC_Park_Pilrig = 0,
  b_EC_Park_Rodley = 0,
  b_EC_Park_St_Andrews = 0,
  b_EC_Park_St_Marks = 0
  
)

## Hold Alternative-Specific Constants for non-status-quo options at zero
apollo_fixed = c()


### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = 2000,
  interUnifDraws = c(),
  interNormDraws = c(
    "draws_Plants5" ,
    "draws_Plants10" ,
    "draws_NativeDecrease",
    "draws_NativeIncrease",
    "draws_PollinatorQualityMedium" ,
    "draws_PollinatorQualityHigh" ,
    "draws_Appearance_W",
    "draws_Appearance_WYB",
    "draws_Appearance_WYBPR"
  ), 
  intraDrawsType = "pmc",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)



## Defines each attribute's random (WTP-space) coefficient as its mean plus a
## correlated mixture of Sobol draws, allowing correlation across attributes
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_NativeDecrease"]] =  (
    mu_NativeDecrease + sig_NativeDecrease * draws_NativeDecrease +
      
      sig_NativeDecrease_Plants5 * draws_Plants5 +
      sig_NativeDecrease_Plants10 * draws_Plants10 +
      
      sig_NativeDecrease_PollinatorQualityMedium * draws_PollinatorQualityMedium +
      sig_NativeDecrease_PollinatorQualityHigh * draws_PollinatorQualityHigh +
      
      sig_NativeDecrease_Appearance_W * draws_Appearance_W +
      sig_NativeDecrease_Appearance_WYB * draws_Appearance_WYB +
      sig_NativeDecrease_Appearance_WYBPR * draws_Appearance_WYBPR
  )
  
  
  randcoeff[["b_NativeIncrease"]] =  (
    mu_NativeIncrease + sig_NativeIncrease * draws_NativeIncrease +
      
      sig_NativeIncrease_Plants5 * draws_Plants5 +
      sig_NativeIncrease_Plants10 * draws_Plants10 +
      
      sig_NativeIncrease_PollinatorQualityMedium * draws_PollinatorQualityMedium +
      sig_NativeIncrease_PollinatorQualityHigh * draws_PollinatorQualityHigh +
      
      sig_NativeIncrease_Appearance_W * draws_Appearance_W +
      sig_NativeIncrease_Appearance_WYB * draws_Appearance_WYB +
      sig_NativeIncrease_Appearance_WYBPR * draws_Appearance_WYBPR
  )
  
  
  randcoeff[["b_Plants5"]] =   (
    mu_Plants5 + sig_Plants5 * draws_Plants5 +
      
      sig_Plants5_NativeDecrease * draws_NativeDecrease +
      sig_Plants5_NativeIncrease * draws_NativeIncrease +
      
      sig_Plants5_PollinatorQualityMedium * draws_PollinatorQualityMedium +
      sig_Plants5_PollinatorQualityHigh * draws_PollinatorQualityHigh +
      
      sig_Plants5_Appearance_W * draws_Appearance_W +
      sig_Plants5_Appearance_WYB * draws_Appearance_WYB +
      sig_Plants5_Appearance_WYBPR * draws_Appearance_WYBPR
  )
  
  
  randcoeff[["b_Plants10"]] =  (
    mu_Plants10 + sig_Plants10 * draws_Plants10 +
      
      sig_Plants10_NativeDecrease * draws_NativeDecrease +
      sig_Plants10_NativeIncrease * draws_NativeIncrease +
      
      sig_Plants10_PollinatorQualityMedium * draws_PollinatorQualityMedium +
      sig_Plants10_PollinatorQualityHigh * draws_PollinatorQualityHigh +
      
      sig_Plants10_Appearance_W * draws_Appearance_W +
      sig_Plants10_Appearance_WYB * draws_Appearance_WYB +
      sig_Plants10_Appearance_WYBPR * draws_Appearance_WYBPR
  )
  
  randcoeff[["b_PollinatorQualityMedium"]] = (
    mu_PollinatorQualityMedium +
      sig_PollinatorQualityMedium * draws_PollinatorQualityMedium +
      
      sig_PollinatorQualityMedium_NativeDecrease * draws_NativeDecrease +
      sig_PollinatorQualityMedium_NativeIncrease * draws_NativeIncrease +
      
      sig_PollinatorQualityMedium_Plants5 * draws_Plants5 +
      sig_PollinatorQualityMedium_Plants10 * draws_Plants10 +
      
      sig_PollinatorQualityMedium_Appearance_W * draws_Appearance_W +
      sig_PollinatorQualityMedium_Appearance_WYB * draws_Appearance_WYB +
      sig_PollinatorQualityMedium_Appearance_WYBPR * draws_Appearance_WYBPR
  )
  
  
  randcoeff[["b_PollinatorQualityHigh"]] =  (
    mu_PollinatorQualityHigh +
      sig_PollinatorQualityHigh * draws_PollinatorQualityHigh +
      
      sig_PollinatorQualityHigh_NativeDecrease * draws_NativeDecrease +
      sig_PollinatorQualityHigh_NativeIncrease * draws_NativeIncrease +
      
      sig_PollinatorQualityHigh_Plants5 * draws_Plants5 +
      sig_PollinatorQualityHigh_Plants10 * draws_Plants10 +
      
      sig_PollinatorQualityHigh_Appearance_W * draws_Appearance_W +
      sig_PollinatorQualityHigh_Appearance_WYB * draws_Appearance_WYB +
      sig_PollinatorQualityHigh_Appearance_WYBPR * draws_Appearance_WYBPR
  )
  
  randcoeff[["b_Appearance_W"]] =
    (
      mu_Appearance_W + sig_Appearance_W * draws_Appearance_W +
        
        sig_Appearance_W_NativeDecrease * draws_NativeDecrease +
        sig_Appearance_W_NativeIncrease * draws_NativeIncrease +
        
        sig_Appearance_W_Plants5 * draws_Plants5 +
        sig_Appearance_W_Plants10 * draws_Plants10 +
        
        sig_Appearance_W_PollinatorQualityMedium * draws_PollinatorQualityMedium +
        sig_Appearance_W_PollinatorQualityHigh * draws_PollinatorQualityHigh
    )
  
  randcoeff[["b_Appearance_WYB"]] = 
    (
      mu_Appearance_WYB + sig_Appearance_WYB * draws_Appearance_WYB +
        
        sig_Appearance_WYB_NativeDecrease * draws_NativeDecrease +
        sig_Appearance_WYB_NativeIncrease * draws_NativeIncrease +
        
        sig_Appearance_WYB_Plants5 * draws_Plants5 +
        sig_Appearance_WYB_Plants10 * draws_Plants10 +
        
        sig_Appearance_WYB_PollinatorQualityMedium * draws_PollinatorQualityMedium +
        sig_Appearance_WYB_PollinatorQualityHigh * draws_PollinatorQualityHigh
    )
  
  
  randcoeff[["b_Appearance_WYBPR"]] = 
    (
      mu_Appearance_WYBPR + sig_Appearance_WYBPR* draws_Appearance_WYBPR +
        
        sig_Appearance_WYBPR_NativeDecrease * draws_NativeDecrease +
        sig_Appearance_WYBPR_NativeIncrease * draws_NativeIncrease +
        
        sig_Appearance_WYBPR_Plants5 * draws_Plants5 +
        sig_Appearance_WYBPR_Plants10 * draws_Plants10 +
        
        sig_Appearance_WYBPR_PollinatorQualityMedium * draws_PollinatorQualityMedium +
        sig_Appearance_WYBPR_PollinatorQualityHigh * draws_PollinatorQualityHigh
    )
  
  
  return(randcoeff)
}



apollo_inputs = apollo_validateInputs() ## Required to check inputs are fine


# ****************************
# Estimation Specification: ####
### Note: Model in WTP-space.
# ****************************
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  
  P = list()
  
  V = list()
  
  ## Utility for alternative A: WTP-space, so each attribute coefficient is already
  ## in £ terms and utility is scaled by -b_Cost (see Train & Weeks 2005 WTP-space MXL)
  V[["A"]]  = -b_Cost * (
    b_Plants5  * (Plants_5_A == 1) +
      b_Plants10  * (Plants_10_A == 1) +
      b_NativeDecrease  * (Native_Decrease_A == 1)  +
      b_NativeIncrease  * (Native_Increase_A == 1) +
      b_Appearance_W * (Appearance_W_A == 1) +
      b_Appearance_WYB * (Appearance_WYB_A == 1) +
      b_Appearance_WYBPR * (Appearance_WYBPR_A == 1) +
      b_PollinatorQualityMedium * (PollinatorQuality_Medium_A == 1) +
      b_PollinatorQualityHigh * (PollinatorQuality_High_A == 1) +
      (b_type * Treated * 1) + #Adding main effect of annual indicator as you have perennial indicator as interaction
      (b_type_Plants_5 * Plants_5_A * Treated) + ##Adding type interaction terms
      (b_type_Plants_10 * Plants_10_A * Treated) +
      (b_type_Native_Decrease * Native_Decrease_A * Treated) +
      (b_type_Native_Increase * Native_Increase_A * Treated) +
      (b_type_Appearance_W * Appearance_W_A * Treated) +
      (b_type_Appearance_WYB * Appearance_WYB_A * Treated) +
      (b_type_Appearance_WYBPR * Appearance_WYBPR_A * Treated) +
      (b_type_PollinatorQuality_Medium * PollinatorQuality_Medium_A * Treated) +
      (b_type_PollinatorQuality_High * PollinatorQuality_High_A * Treated) -
      costA
  )
  
  V[["B"]]  = -b_Cost * (
    b_Plants5  * (Plants_5_B==1) +
      b_Plants10  * (Plants_10_B==1) +
      b_NativeDecrease  * (Native_Decrease_B==1)  +
      b_NativeIncrease  * (Native_Increase_B==1)  +
      b_Appearance_W * (Appearance_W_B==1) +
      b_Appearance_WYB * (Appearance_WYB_B==1) +
      b_Appearance_WYBPR * (Appearance_WYBPR_B==1) +
      b_PollinatorQualityMedium * (PollinatorQuality_Medium_B==1)+
      b_PollinatorQualityHigh * (PollinatorQuality_High_B == 1) +
      (b_type * Treated * 1) + #Adding main effect of annual indicator as you have perennial indicator as interaction
      (b_type_Plants_5 * Plants_5_B * Treated) + ##Adding type interaction terms
      (b_type_Plants_10 * Plants_10_B * Treated) +
      (b_type_Native_Decrease * Native_Decrease_B * Treated) +
      (b_type_Native_Increase * Native_Increase_B * Treated) +
      (b_type_Appearance_W * Appearance_W_B * Treated) +
      (b_type_Appearance_WYB * Appearance_WYB_B * Treated) +
      (b_type_Appearance_WYBPR * Appearance_WYBPR_B * Treated) +
      (b_type_PollinatorQuality_Medium * PollinatorQuality_Medium_B * Treated) +
      (b_type_PollinatorQuality_High * PollinatorQuality_High_B * Treated) -
      costB
  )
  
  
  V[["C"]]  = -b_Cost * (
    b_Plants5  * (Plants_5_C==1) +
      b_Plants10  * (Plants_10_C==1) +
      b_NativeDecrease  * (Native_Decrease_C== 1)  +
      b_NativeIncrease  * (Native_Increase_C==1)  +
      b_Appearance_W * (Appearance_W_C==1) +
      b_Appearance_WYB * (Appearance_WYB_C==1) +
      b_Appearance_WYBPR * (Appearance_WYBPR_C==1) +
      b_PollinatorQualityMedium * (PollinatorQuality_Medium_C==1)+
      b_PollinatorQualityHigh * (PollinatorQuality_High_C ==1) +
      (b_type * Treated * 1) + #Adding main effect of annual indicator as you have perennial indicator as interaction
      (b_type_Plants_5 * Plants_5_C * Treated) + ##Adding type interaction terms
      (b_type_Plants_10 * Plants_10_C * Treated) +
      (b_type_Native_Decrease * Native_Decrease_C * Treated) +
      (b_type_Native_Increase * Native_Increase_C * Treated) +
      (b_type_Appearance_W * Appearance_W_C * Treated) +
      (b_type_Appearance_WYB * Appearance_WYB_C * Treated) +
      (b_type_Appearance_WYBPR * Appearance_WYBPR_C * Treated) +
      (b_type_PollinatorQuality_Medium * PollinatorQuality_Medium_C * Treated) +
      (b_type_PollinatorQuality_High * PollinatorQuality_High_C * Treated) -
      costC
  )
  
  
  ## Alternative D is the status-quo/opt-out; asc_D plus sociodemographic and park
  ## fixed-effect shifters, no attribute levels (SQ has none to trade off)
  V[["D"]] = asc_D +
    beta_IncomeDummy * IncomeDummy +
    beta_AgeDummy * AgeDummy +
    beta_Gender * Gender +
    beta_freqv * FrequencyOfVisit +
    beta_CNS_ScaleMean * CNS_ScaleMean_MeanUpdated +
    beta_visittime * visittime +
    b_EC_Park_Burley * EC_Park_Burley +
    b_EC_Park_Gores_Marsh * EC_Park_Gores_Marsh +
    b_EC_Park_Horfield_Common * EC_Park_Horfield_Common +
    b_EC_Park_Inch_Park * EC_Park_Inch_Park +
    b_EC_Park_King_Lane * EC_Park_King_Lane +
    b_EC_Park_Middleton * EC_Park_Middleton +
    b_EC_Park_Pilrig * EC_Park_Pilrig +
    b_EC_Park_Rodley * EC_Park_Rodley +
    b_EC_Park_St_Andrews * EC_Park_St_Andrews +
    b_EC_Park_St_Marks * EC_Park_St_Marks
  # b_city_C1 * City_1_Leeds +
  # b_city_C2 * City_2_Bristol
  
  
  mnl_settings = list(
    alternatives  = c(A=1, B=2, C=3, D=4),
    avail         = list(A=1, B=1, C=1, D=1),
    choiceVar     = choice,
    utilities     = V
  )
  
  
  ## Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ## Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ## Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  # P = apollo_weighting(P,apollo_inputs,functionality)
  
  ## Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}




# ****************************
# Model Outputs: ####
# ****************************

#
# ## Actually estimates the model
Meadows_Pooled_Unweighted_GroupedType_ParkFEs_V1 = apollo_estimate(apollo_beta,
                                                                               apollo_fixed,
                                                                               apollo_probabilities,
                                                                               apollo_inputs)

# Model output and results here alongside saving information
apollo_modelOutput(Meadows_Pooled_Unweighted_GroupedType_ParkFEs_V1,
                   modelOutput_settings = list(printPVal = 2)) ## 2-sided P values
apollo_saveOutput(Meadows_Pooled_Unweighted_GroupedType_ParkFEs_V1,
                  saveOutput_settings = list(printPVal = 2)) ## 2-sided P values

