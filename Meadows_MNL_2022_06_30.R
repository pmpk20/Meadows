#### Meadows ####
## Function: Converts data in NLogit to usable R data
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 30/06/2022
## TODO: setup RENV


#------------------------------
# Replication Information: ####
# Selected output of 'sessionInfo()'
#------------------------------

# R version 4.1.3 (2022-03-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)
# [1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252   
# other attached packages:
#   [1] lubridate_1.8.0    tidygeocoder_1.0.5 PostcodesioR_0.3.1 DCchoice_0.1.0    
# [5] here_1.0.1         forcats_0.5.1      stringr_1.4.0      dplyr_1.0.8       
# [9] purrr_0.3.4        readr_2.1.2        tidyr_1.2.0        tibble_3.1.6      
# [13] ggplot2_3.3.5      tidyverse_1.3.1  

## Any issues installing packages try:
# Sys.setenv(RENV_DOWNLOAD_METHOD="libcurl")
# Sys.setenv(RENV_DOWNLOAD_FILE_METHOD=getOption("download.file.method"))

# renv::snapshot()
rm(list=ls())
library(here)
library(DCchoice)
library(lubridate)
library(tidyr)
library(apollo)
library(ggridges)
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(data.table)


#-------------------------------------------
# Section 1: Import Data ####
# Load database latest version: 30/06/2022
#-------------------------------------------


database <- data.frame(read.csv("database_Meadows_2022_06_03.csv"))


#-------------------------------------------
# Section 2: Start Apollo ####
#-------------------------------------------


apollo_initialise()

apollo_control = list(
  modelName  ="Meadows_MNL_2022_06_30",
  modelDescr ="Meadows_MNL_2022_06_30",
  indivID    ="oid",
  analyticGrad = FALSE 
)


#-------------------------------------------
# Section 3: Prepare Estimation Basics ####
#-------------------------------------------



# Beta parameters to be estimated
apollo_beta=c(
  b_PlantsMedium = 0,
  b_PlantsHigh = 0,
  b_NativeLow = 0.00,
  b_NativeHigh=0,
  b_PollenMedium = 0,
  b_PollenHigh = 0,
  b_White = 0.0,
  b_Blue = 0,
  b_All=-0.0000,
  b_Cost=-0.4,
  b_Asc = 0.000
)
apollo_fixed = c()


apollo_inputs = apollo_validateInputs()


#-------------------------------------------
# Section 4: Define utility functions ####
#-------------------------------------------



apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  
  ### Attach inputs and detach after function exit - this just makes sure you can address vars directly in this function
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  # b_distance*get(paste0("NEAR_DIST_",j))  +
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  J<-4
  V = list()
  avail=list()
  for(j in 1:J){
    V[[paste0("alt",j)]]   = 
      b_Asc * get(paste0("asc",j))  + 
      b_Cost * get(paste0("cost",j))  + 
      b_PlantsMedium * get(paste0("mplants",j))+
      b_PlantsHigh*get(paste0("hplants",j)) +
      b_NativeLow*get(paste0("lnative",j)) +
      b_NativeHigh*get(paste0("hnative",j))  +
      b_PollenMedium*get(paste0("mpollin",j))   +
      b_PollenHigh*get(paste0("hpollin",j))   +
      b_White * get(paste0("whiapp",j)) +
      b_Blue*get(paste0("bluapp",j))   +
      b_All*get(paste0("allapp",j)) 
    
    
    avail[[paste0("alt",j)]] = get(paste0("av",j)) # this line changed
  }
  
  mnl_settings = list (
    alternatives = setNames ( 1:J , names(V) ) ,
    avail=avail,        
    choiceVar = choice ,
    V = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  # Take product across observation for same individual (This one has to be outcommented if there is only one obs per individual)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}
apollo_probabilities (apollo_beta, apollo_inputs, functionality="validate")


#-------------------------------------------
# Section 5: Estimate and save ####
#-------------------------------------------


Meadows_MNL_2022_06_30 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


#-------------------------------------------
# Section 6: Post Analysis ####
#-------------------------------------------


apollo_modelOutput(Meadows_MNL_2022_06_30,modelOutput_settings = list(printPVal=TRUE))
Meadows_MNL_2022_06_30$estimate



# End Of Script ------------------------------------------------------------------------------------------------------