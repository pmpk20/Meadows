#### Meadows Paper ####
## Function: Estimates a 3-class LCM with city and type covariates
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 11/04/2022
## TODO: add covariates


#------------------------------
# Setup: ####
#------------------------------


rm(list = ls())
Sys.setlocale("LC_ALL", "C")
library(apollo)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggridges)
# library(ggpubr)
library(reshape2)


database <-
  data.frame(read.csv("database_Meadows_2022_03_11.csv", encoding = "latin1"))


## Discard those respondents who always pick the status quo opt-out
Discard <- Reduce(
  intersect,
  list(
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
                  database$alt == 4]
  )
)

## Remove protestors
database <- database[!database$ID %in% Discard, ]
# database <- database[!is.na(database$AgeLevel), ]
# database <- database[database$Income>0, ]
# 
# database$AgeDummy <-
#   ifelse(database$AgeLevel < median(database$AgeLevel), 1, 0)
# database$IncomeDummy <-
#   ifelse(database$Income < median(database$Income), 1, 0)


## Classify meadows as 'Control','Perennial', or 'Annual'
database$Control <-
  ifelse(database$park...35 %in% c(1, 4, 7, 10, 11, 15), 1, 0)
database$Perennial <-
  ifelse(database$park...35 %in% c(2, 5, 12, 16, 17), 1, 0)
database$Annual <-
  ifelse(database$park...35 %in% c(3, 6, 8, 9, 13, 14), 1, 0)
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

# database <- database[!is.na(database$AgeLevel), ]
# database <- database[!is.na(database$traveltime), ]
# database <- database[!is.na(database$visittime), ]


#------------------------------
# Setup Apollo: ####
#------------------------------

apollo_initialise()

apollo_control = list(
  modelName  = "Meadows_LCM_3Class_AllCovariates_2022_03_24",
  modelDescr = "Meadows_LCM_3Class_AllCovariates_2022_03_24",
  indivID    = "ID",
  nCores     = 10
)


apollo_beta = c(
  asc_D = 0,
  b_cost_Class1    = 0,
  b_Plants5_Class1     = 0,
  b_Plants10_Class1     = 0,
  b_NativeDecrease_Class1     = 0,
  b_NativeIncrease_Class1     = 0,
  b_PollinatorQualityMedium_Class1     = 0,
  b_PollinatorQualityHigh_Class1     = 0,
  b_Appearance_W_Class1     = 0,
  b_Appearance_WYB_Class1     = 0,
  b_Appearance_WYBPR_Class1     = 0,
  b_cost_Class2    = 0,
  b_Plants5_Class2     = 0,
  b_Plants10_Class2     = 0,
  b_NativeDecrease_Class2     = 0,
  b_NativeIncrease_Class2     = 0,
  b_PollinatorQualityMedium_Class2     = 0,
  b_PollinatorQualityHigh_Class2     = 0,
  b_Appearance_W_Class2     = 0,
  b_Appearance_WYB_Class2     = 0,
  b_Appearance_WYBPR_Class2     = 0,
  b_cost_Class3    = 0,
  b_Plants5_Class3     = 0,
  b_Plants10_Class3     = 0,
  b_NativeDecrease_Class3     = 0,
  b_NativeIncrease_Class3     = 0,
  b_PollinatorQualityMedium_Class3     = 0,
  b_PollinatorQualityHigh_Class3     = 0,
  b_Appearance_W_Class3     = 0,
  b_Appearance_WYB_Class3     = 0,
  b_Appearance_WYBPR_Class3     = 0,
  gamma_City_Class1 = 0,
  gamma_City_Class2 = 0,
  gamma_Type_Class1 = 0,
  gamma_Type_Class2 = 0,
  delta_A = 0.5,
  delta_B = 1.1,
  delta_C = 0
)


apollo_fixed = c("delta_C")


#------------------------------
# Define Latent-Class Parameters: ####
#------------------------------


apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  
  ### Define lists of parameters for each class
  lcpars[["beta_Cost"]] = list(b_cost_Class1, b_cost_Class2,b_cost_Class3)
  lcpars[["beta_NativeDecrease"]] =  list(b_NativeDecrease_Class1,b_NativeDecrease_Class2,b_NativeDecrease_Class3 )
  lcpars[["beta_NativeIncrease"]] =  list(b_NativeIncrease_Class1,b_NativeIncrease_Class2,b_NativeIncrease_Class3 )
  lcpars[["beta_Plants5"]] =  list(b_Plants5_Class1,b_Plants5_Class2,b_Plants5_Class3 )
  lcpars[["beta_Plants10"]] =  list(b_Plants10_Class1,b_Plants10_Class2,b_Plants10_Class3 )
  lcpars[["beta_PollinatorQualityMedium"]] =  list(b_PollinatorQualityMedium_Class1,b_PollinatorQualityMedium_Class2,b_PollinatorQualityMedium_Class3 )
  lcpars[["beta_PollinatorQualityHigh"]] =  list(b_PollinatorQualityHigh_Class1,b_PollinatorQualityHigh_Class2,b_PollinatorQualityHigh_Class3 )
  lcpars[["beta_Appearance_W"]] =  list(b_Appearance_W_Class1,b_Appearance_W_Class2,b_Appearance_W_Class3 )
  lcpars[["beta_Appearance_WYB"]] =  list(b_Appearance_WYB_Class1,b_Appearance_WYB_Class2,b_Appearance_WYB_Class3 )
  lcpars[["beta_Appearance_WYBPR"]] =  list(b_Appearance_WYBPR_Class1,b_Appearance_WYBPR_Class2,b_Appearance_WYBPR_Class3)
  
  V=list()
  V[["class_A"]] = delta_A+
    (gamma_City_Class1*city)+
    (gamma_Type_Class1*Type)
  V[["class_B"]] = delta_B+  
    (gamma_City_Class2*city)+
    (gamma_Type_Class2*Type)
  V[["class_C"]] = delta_C
  
  classAlloc_settings = list(
    classes      = c(class_A=1, class_B=2,class_C=3), 
    utilities    = V
  )
  
  lcpars[["pi_values"]] = apollo_classAlloc(classAlloc_settings)
  
  return(lcpars)
}


apollo_inputs = apollo_validateInputs()


#------------------------------
# Define Model: ####
# Note: Estimated in preference-space
#------------------------------


apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities
  P = list()
  
  
  
  mnl_settings = list(
    alternatives = c(altA=1, altB=2, altC=3,altD=4),
    avail        = list(altA=1, altB=1, altC=1,altD=1),
    choiceVar    = alt)
  ### Loop over classes
  S = 3 # number of classes
  for(s in 1:S){
    V=list()
    
    V[["altA"]] = beta_Cost[[s]] * (costA>0)+ 
      beta_Plants5[[s]]  * (plantsA==1) + 
      beta_Plants10[[s]]  * (plantsA==2) + 
      beta_NativeDecrease[[s]]  * (nativeA==-1)  +
      beta_NativeIncrease[[s]]  * (nativeA==1)  +
      beta_Appearance_W[[s]] * (appearA==1) +
      beta_Appearance_WYB[[s]] * (appearA==2) +
      beta_Appearance_WYBPR[[s]] * (appearA==3) +
      beta_PollinatorQualityMedium[[s]] * (pollinA==1)+ 
      beta_PollinatorQualityHigh[[s]] * (pollinA ==2)
    V[["altB"]]  = beta_Cost[[s]] *(costB>0)+ 
      beta_Plants5[[s]]  * (plantsB==1) + 
      beta_Plants10[[s]]  * (plantsB==2) + 
      beta_NativeDecrease[[s]]  * (nativeB==-1)  +
      beta_NativeIncrease[[s]]  * (nativeB==1)  +
      beta_Appearance_W[[s]] * (appearB==1) +
      beta_Appearance_WYB[[s]] * (appearB==2)+
      beta_Appearance_WYBPR[[s]] * (appearB==3) +
      beta_PollinatorQualityMedium[[s]] * (pollinB==1)+ 
      beta_PollinatorQualityHigh[[s]] * (pollinB ==2)
    V[["altC"]]  = beta_Cost[[s]] *(costC>0)+ 
      beta_Plants5[[s]]  * (plantsC==1) + 
      beta_Plants10[[s]]  * (plantsC==2) + 
      beta_NativeDecrease[[s]]  * (nativeC== -1)  +
      beta_NativeIncrease[[s]]  * (nativeC==1)  +
      beta_Appearance_W[[s]] * (appearC==1) +
      beta_Appearance_WYB[[s]] * (appearC==2) +
      beta_Appearance_WYBPR[[s]] * (appearC==3) +
      beta_PollinatorQualityMedium[[s]] * (pollinC==1)+ 
      beta_PollinatorQualityHigh[[s]] * (pollinC ==2)
    V[["altD"]] = asc_D 
    
    mnl_settings$utilities = V
    #mnl_settings$componentName = paste0("Class_",s)
    
    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
  }
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)


#------------------------------
# Estimation: ####
#------------------------------


Meadows_LCM_3Class_AllCovariates_2022_03_24 = apollo_estimate(apollo_beta, apollo_fixed, 
                                                              apollo_probabilities, apollo_inputs)

apollo_modelOutput(Meadows_LCM_3Class_AllCovariates_2022_03_24,modelOutput_settings = list(printPVal=TRUE))

## Save Results:
apollo_saveOutput(Meadows_LCM_3Class_AllCovariates_2022_03_24,saveOutput_settings = list(printPVal=TRUE))
saveRDS(Meadows_LCM_3Class_AllCovariates_2022_03_24,"Meadows_LCM_3Class_AllCovariates_2022_03_24.rds") 


#------------------------------
# Save Unconditional WTP Estimates: ####
#------------------------------

Model <- readRDS("Meadows_LCM_3Class_AllCovariates_2022_03_24.rds") 
Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP <- apollo_lcUnconditionals(model = Model, apollo_probabilities, apollo_inputs)
saveRDS(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP,"Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP.rds")        



#------------------------------
# Calculate class-specific WTP: ####
#------------------------------


# Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP
Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP <-readRDS("Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP.rds")
PIValues <- (Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$pi_values)
Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$pi_values <- NULL
WTP_Class1 <- cbind("beta_NativeDecrease"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_NativeDecrease[1]),
                    "beta_NativeIncrease"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_NativeIncrease[1]),
                    "beta_Plants5"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_Plants5[1]),
                    "beta_Plants10"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_Plants10[1]),
                    "beta_PollinatorQualityMedium"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_PollinatorQualityMedium[1]),
                    "beta_PollinatorQualityHigh"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_PollinatorQualityHigh[1]),
                    "beta_Appearance_W"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_Appearance_W[1]),
                    "beta_Appearance_WYB"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_Appearance_WYB[1]),
                    "beta_Appearance_WYBPR"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_Appearance_WYBPR[1]),
                    "beta_Cost"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_Cost[1]))


WTP_Class2 <- cbind("beta_NativeDecrease"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_NativeDecrease[2]),
                    "beta_NativeIncrease"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_NativeIncrease[2]),
                    "beta_Plants5"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_Plants5[2]),
                    "beta_Plants10"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_Plants10[2]),
                    "beta_PollinatorQualityMedium"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_PollinatorQualityMedium[2]),
                    "beta_PollinatorQualityHigh"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_PollinatorQualityHigh[2]),
                    "beta_Appearance_W"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_Appearance_W[2]),
                    "beta_Appearance_WYB"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_Appearance_WYB[2]),
                    "beta_Appearance_WYBPR"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_Appearance_WYBPR[2]),
                    "beta_Cost"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_Cost[2]))


WTP_Class3 <- cbind("beta_NativeDecrease"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_NativeDecrease[3]),
                    "beta_NativeIncrease"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_NativeIncrease[3]),
                    "beta_Plants5"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_Plants5[3]),
                    "beta_Plants10"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_Plants10[3]),
                    "beta_PollinatorQualityMedium"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_PollinatorQualityMedium[3]),
                    "beta_PollinatorQualityHigh"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_PollinatorQualityHigh[3]),
                    "beta_Appearance_W"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_Appearance_W[3]),
                    "beta_Appearance_WYB"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_Appearance_WYB[3]),
                    "beta_Appearance_WYBPR"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_Appearance_WYBPR[3]),
                    "beta_Cost"=as.numeric(Meadows_LCM_3Class_AllCovariates_2022_03_24_UCWTP$beta_Cost[3]))


ThreeClassWTP <-rbind(-1*(WTP_Class1/WTP_Class1[10]),
                      -1*(WTP_Class2/WTP_Class2[10]),
                      -1*(WTP_Class3/WTP_Class3[10]))

rownames(ThreeClassWTP) <- c("Class1","Class2","Class3")
ThreeClassWTP <- cbind(ThreeClassWTP[, 1:9], data.frame("Cost" = rbind(WTP_Class1[10], WTP_Class2[10], WTP_Class3[10])))
ThreeClassWTP <- cbind(ThreeClassWTP,data.frame("Class"=c("Class1","Class2","Class3")))
colnames(ThreeClassWTP)<-    c("NativeDecrease",
                               "NativeIncrease",
                               "Plants5",
                               "Plants10",
                               "PollinatorQualityMedium",
                               "PollinatorQualityHigh",
                               "Appearance_W",
                               "Appearance_WYB",
                               "Appearance_WYBPR","Cost","Class")

#------------------------------
# Calculate class-specific Standard Errors: ####
## StdErr calculated using delta method
#------------------------------


Model <- readRDS("Meadows_LCM_3Class_AllCovariates_2022_03_24.rds") 



LCM3C_SD_Class1 <- cbind("SD_NativeDecrease"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_NativeDecrease_Class1/b_cost_Class1")))["Robust s.e."]),
                         "SD_NativeIncrease"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_NativeIncrease_Class1/b_cost_Class1")))["Robust s.e."]),
                         "SD_Plants5"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_Plants5_Class1/b_cost_Class1")))["Robust s.e."]),
                         "SD_Plants10"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_Plants10_Class1/b_cost_Class1")))["Robust s.e."]),
                         "SD_PollinatorQualityMedium"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_PollinatorQualityMedium_Class1/b_cost_Class1")))["Robust s.e."]),
                         "SD_PollinatorQualityHigh"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_PollinatorQualityHigh_Class1/b_cost_Class1")))["Robust s.e."]),
                         "SD_Appearance_W"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_Appearance_W_Class1/b_cost_Class1")))["Robust s.e."]),
                         "SD_Appearance_WYB"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_Appearance_WYB_Class1/b_cost_Class1")))["Robust s.e."]),
                         "SD_Appearance_WYBPR"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_Appearance_WYBPR_Class1/b_cost_Class1")))["Robust s.e."]),
                         "SD_Cost"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_cost_Class1")))["Robust s.e."]))


LCM3C_SD_Class2 <- cbind("SD_NativeDecrease"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_NativeDecrease_Class2/b_cost_Class2")))["Robust s.e."]),
                         "SD_NativeIncrease"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_NativeIncrease_Class2/b_cost_Class2")))["Robust s.e."]),
                         "SD_Plants5"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_Plants5_Class2/b_cost_Class2")))["Robust s.e."]),
                         "SD_Plants10"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_Plants10_Class2/b_cost_Class2")))["Robust s.e."]),
                         "SD_PollinatorQualityMedium"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_PollinatorQualityMedium_Class2/b_cost_Class2")))["Robust s.e."]),
                         "SD_PollinatorQualityHigh"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_PollinatorQualityHigh_Class2/b_cost_Class2")))["Robust s.e."]),
                         "SD_Appearance_W"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_Appearance_W_Class2/b_cost_Class2")))["Robust s.e."]),
                         "SD_Appearance_WYB"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_Appearance_WYB_Class2/b_cost_Class2")))["Robust s.e."]),
                         "SD_Appearance_WYBPR"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_Appearance_WYBPR_Class2/b_cost_Class2")))["Robust s.e."]),
                         "SD_Cost"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_cost_Class2")))["Robust s.e."]))


LCM3C_SD_Class3 <- cbind("SD_NativeDecrease"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_NativeDecrease_Class3/b_cost_Class3")))["Robust s.e."]),
                         "SD_NativeIncrease"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_NativeIncrease_Class3/b_cost_Class3")))["Robust s.e."]),
                         "SD_Plants5"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_Plants5_Class3/b_cost_Class3")))["Robust s.e."]),
                         "SD_Plants10"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_Plants10_Class3/b_cost_Class3")))["Robust s.e."]),
                         "SD_PollinatorQualityMedium"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_PollinatorQualityMedium_Class3/b_cost_Class3")))["Robust s.e."]),
                         "SD_PollinatorQualityHigh"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_PollinatorQualityHigh_Class3/b_cost_Class3")))["Robust s.e."]),
                         "SD_Appearance_W"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_Appearance_W_Class3/b_cost_Class3")))["Robust s.e."]),
                         "SD_Appearance_WYB"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_Appearance_WYB_Class3/b_cost_Class3")))["Robust s.e."]),
                         "SD_Appearance_WYBPR"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_Appearance_WYBPR_Class3/b_cost_Class3")))["Robust s.e."]),
                         "SD_Cost"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_cost_Class3")))["Robust s.e."]))


ThreeClassSD <-rbind(LCM3C_SD_Class1,
                     LCM3C_SD_Class2,
                     LCM3C_SD_Class3)

rownames(ThreeClassSD) <- c("Class1SD","Class2SD","Class3SD")
ThreeClassSD <- cbind(ThreeClassSD,data.frame("Class"=c("Class1","Class2","Class3")))
colnames(ThreeClassSD)<-    c("NativeDecrease",
                              "NativeIncrease",
                              "Plants5",
                              "Plants10",
                              "PollinatorQualityMedium",
                              "PollinatorQualityHigh",
                              "Appearance_W",
                              "Appearance_WYB",
                              "Appearance_WYBPR","Cost","Class")


#------------------------------
# Merge and Melt together in one place: ####
#------------------------------

MeanAndSD_3C<- cbind(melt(ThreeClassWTP,id.vars="Class"),melt(ThreeClassSD,id.vars="Class"))
MeanAndSD_3C <- data.frame(cbind(MeanAndSD_3C[,1:3],"SD"=MeanAndSD_3C[,6]))


#------------------------------
# Produce Bar-Chart of WTP with Error bars: ####
#------------------------------

ggsave(
  ggplot(data=MeanAndSD_3C,aes(x=variable,y=value,fill=variable))+
    geom_bar(stat="identity")+
    geom_errorbar(aes(ymin = value - SD, ymax = value + SD)) + 
    facet_wrap( ~ Class,scales="free_x",labeller = as_labeller(
      c('Class1' = paste0("Class1: ",round(100*as.numeric(substr(Model$componentReport$model$param[4],start=12,stop = 20)),2),"%"), 
        'Class2' = paste0("Class2: ",round(100*as.numeric(substr(Model$componentReport$model$param[5],start=12,stop = 20)),2),"%"),
        'Class3' = paste0("Class3: ",round(100*as.numeric(substr(Model$componentReport$model$param[6],start=12,stop = 20)),2),"%")
      )))+scale_fill_manual(
        "Attributes",
        values = c("NativeDecrease" = "yellow",
                   "NativeIncrease" = "yellow",
                   "Plants5" = "green",
                   "Plants10" = "green",
                   "PollinatorQualityMedium" = "red",
                   "PollinatorQualityHigh" = "darkred",
                   "Appearance_W" = "lightblue",
                   "Appearance_WYB" = "blue",
                   "Appearance_WYBPR" = "darkblue",
                   "Cost" = "black"
        )
      ) + ggtitle("3-Class Model: Attribute WTP.") + ylab("Mean Attribute WTP in #py") +
    theme(legend.position = "bottom") +
    guides(fill=guide_legend(nrow=3,byrow=TRUE))+coord_flip()+geom_hline(yintercept = 0),
  device = "jpeg",
  filename = "Meadows_LCM_3ClassWTP_BarHError_2022_03_24.jpeg",
  width=30,height=25,units = "cm",dpi=500)
