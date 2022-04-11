#### Meadows Paper ####
## Function: Estimates a multinomial model without covariates
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 11/03/2022
## TODO: replicate WTP from thesis


rm(list=ls())
Sys.setlocale("LC_ALL","C")

library(magrittr)
library(dplyr)
library(apollo)


database <- data.frame(read.csv("database_Meadows_2022_03_11.csv",encoding="latin1"))


Discard <- Reduce(intersect,list(
  database$ID[database$Task == 1 &
                database$alt == 4],
  database$ID[database$Task == 2 &
                database$alt == 4],
  database$ID[database$Task == 3 &
                database$alt == 4],
  database$ID[database$Task == 4 &
                database$alt == 4],
  database$ID[database$Task == 5 &
                database$alt == 4],
  database$ID[database$Task == 6 &
                database$alt == 4]))

database <- database[!database$ID %in% Discard,]
database <- database[!is.na(database$AgeLevel),]
database$AgeDummy <- ifelse(database$AgeLevel< median(database$AgeLevel),1,0)


database$Control <- ifelse(database$park...35 %in% c(1,4,7,10,11,15),1,0)
database$Perennial <- ifelse(database$park...35 %in% c(2,5,12,16,17),1,0)
database$Annual <- ifelse(database$park...35 %in% c(3,6,8,9,13,14),1,0)
database$Type <-
  ifelse(
    database$park...35 %in% c(1, 4, 7, 10, 11, 15),
    1,
    ifelse(
      database$park...35 %in% c(2, 5, 12, 16, 17),
      2,
      ifelse(database$park...35 %in% c(3, 6, 8, 9, 13, 14), 3, 0)
    )
  )





apollo_initialise()


### Set core controls
apollo_control = list(
  modelName       = "Meadows_MNL_2022_03_02",
  modelDescr      = "Meadows_MNL_2022_03_02",
  indivID         = "ID"
)


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_A      = 0,
              asc_B      = 0,
              asc_C      = 0,
              asc_D     = 0,
              b_Plants     = 0,
              b_SpeciesHigh     = 0,
              b_cost    = 0,
              b_pollin     = 0,
              b_PlantsHigh     = 0,
              b_SpeciesLow     = 0,
              b_appearMedium     = 0,
              b_appearHigh     = 0,
              b_appearHighest     = 0,
              b_pollinHigh     = 0,
              beta_City=0,
              beta_Type=0,
              beta_Charity=0,
              beta_AgeLevel=0,
              beta_Gender=0,
              beta_Income=0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_A","asc_B","asc_C")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  
  asc_D1 = (asc_D +
    (beta_City*city)+
    (beta_Type*Type)+
    (beta_Charity*Charity)+
    (beta_AgeLevel*AgeDummy)+
    (beta_Gender*Gender)+
    (beta_Income*Income))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["A"]]  = asc_A  + b_cost *(costA+ 
    b_Plants  * (plantsA==1) + 
    b_PlantsHigh  * (plantsA==2) + 
    b_SpeciesLow  * (nativeA==-1)  +
    b_SpeciesHigh  * (nativeA==1)  +
    b_appearMedium * (appearA==1) +
    b_appearHigh * (appearA==2) +
    b_appearHighest * (appearA==3) +
    b_pollin * (pollinA==1)+ 
    b_pollinHigh * (pollinA ==2))
  V[["B"]]  = asc_B  + b_cost *(costB+
    b_Plants  * (plantsB==1) + 
    b_PlantsHigh  * (plantsB==2) + 
    b_SpeciesLow  * (nativeB==-1)  +
    b_SpeciesHigh  * (nativeB==1)  +
    b_appearMedium * (appearB==1) +
    b_appearHigh * (appearB==2) +
    b_appearHighest * (appearB==3) +
    b_pollin * (pollinB==1)+ 
    b_pollinHigh * (pollinB ==2))
  V[["C"]]  = asc_C  +b_cost * (costC+
    b_Plants  * (plantsC==1) + 
    b_PlantsHigh  * (plantsC==2) + 
    b_SpeciesLow  * (nativeC==-1)  +
    b_SpeciesHigh  * (nativeC==1)  +
    b_appearMedium * (appearC==1) +
    b_appearHigh * (appearC==2) +
    b_appearHighest * (appearC==3) +
    b_pollin * (pollinC==1)+ 
    b_pollinHigh * (pollinC ==2))
  V[["D"]] = asc_D1 
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(A=1, B=2, C=3, D=4), 
    avail         = list(A=1, B=1, C=1, D=1), 
    choiceVar     = alt,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

Meadows_MNL_2022_03_02 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(Meadows_MNL_2022_03_02,modelOutput_settings = list(printPVal=TRUE))
-1*(Meadows_MNL_2022_03_02$estimate/Meadows_MNL_2022_03_02$estimate["b_cost"])


apollo_saveOutput(Meadows_MNL_2022_03_02,saveOutput_settings = list(printPVal=TRUE))


apollo_deltaMethod(Meadows_MNL_2022_03_02,
                   deltaMethod_settings = list(operation="ratio",
                                               parName1="b_pollin",
                                               parName2="b_cost"))
