#### Meadows Paper ####
## Function: Estimates a basic Mixed Logit model with truncated sample
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 14/03/2022
## TODO: replicate WTP from thesis


rm(list=ls())
Sys.setlocale("LC_ALL","C")

library(magrittr)
library(dplyr)
library(apollo)
library(ggplot2)
library(reshape2)
library(ggridges)



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




# database <- database[!database$ID %in% Discard,]
# database <- database[!is.na(database$AgeLevel),]
# database$AgeDummy <- ifelse(database$AgeLevel< median(database$AgeLevel),1,0)
# 
# 
# database$Control <- ifelse(database$park...35 %in% c(1,4,7,10,11,15),1,0)
# database$Perennial <- ifelse(database$park...35 %in% c(2,5,12,16,17),1,0)
# database$Annual <- ifelse(database$park...35 %in% c(3,6,8,9,13,14),1,0)
# database$Type <-
#   ifelse(
#     database$park...35 %in% c(1, 4, 7, 10, 11, 15),
#     1,
#     ifelse(
#       database$park...35 %in% c(2, 5, 12, 16, 17),
#       2,
#       ifelse(database$park...35 %in% c(3, 6, 8, 9, 13, 14), 3, 0)
#     )
#   )


apollo_initialise()



apollo_control = list(
  nCores    = 10,
  mixing    = TRUE,
  modelDescr = "Meadows_MXL_Levels_2022_03_14",
  modelName  = "Meadows_MXL_Levels_2022_03_14",
  indivID    = "ID"
)

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_A      = 0,
              asc_B      = 0,
              asc_C      = 0,
              asc_D     = 0,
              mu_Plants5     = 1,
              mu_Plants10     = 1,
              mu_NativeDecrease     = 1,
              mu_NativeIncrease     = 1,
              mu_cost    = -3,
              mu_PollinatorQualityMedium     = 0,
              mu_PollinatorQualityHigh     = 0,
              mu_Appearance_W     = 1,
              mu_Appearance_WYB     = 1,
              mu_Appearance_WYBPR     = 1,
              sig_Plants5     = 0,
              sig_Plants10 =0     ,
              sig_NativeDecrease     = 0,
              sig_NativeIncrease     = 0,
              sig_cost    = -0.01,
              sig_PollinatorQualityMedium    = 0,
              sig_PollinatorQualityHigh     = 0,
              sig_Appearance_W     = 0,
              sig_Appearance_WYB     = 0,
              sig_Appearance_WYBPR     = 0)

apollo_fixed = c("asc_A","asc_B","asc_C")


### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "pmc",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_Cost"  ,
                     "draws_Plants5" ,"draws_Plants10" ,
                     "draws_NativeDecrease","draws_NativeIncrease",
                     "draws_PollinatorQualityMedium" ,"draws_PollinatorQualityHigh" ,
                     "draws_Appearance_W","draws_Appearance_WYB","draws_Appearance_WYBPR"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)
### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Cost"]] = -exp(mu_cost + sig_cost * draws_Cost )
  randcoeff[["b_NativeDecrease"]] =  (mu_NativeDecrease + sig_NativeDecrease * draws_NativeDecrease )
  randcoeff[["b_NativeIncrease"]] =  (mu_NativeIncrease + sig_NativeIncrease * draws_NativeIncrease )
  randcoeff[["b_Plants5"]] =  (mu_Plants5 + sig_Plants5 * draws_Plants5 )
  randcoeff[["b_Plants10"]] =  (mu_Plants10 + sig_Plants10 * draws_Plants10 )
  randcoeff[["b_PollinatorQualityMedium"]] =  (mu_PollinatorQualityMedium + sig_PollinatorQualityMedium * draws_PollinatorQualityMedium )
  randcoeff[["b_PollinatorQualityHigh"]] =  (mu_PollinatorQualityHigh + sig_PollinatorQualityHigh * draws_PollinatorQualityHigh )
  randcoeff[["b_Appearance_W"]] =  (mu_Appearance_W + sig_Appearance_W * draws_Appearance_W )
  randcoeff[["b_Appearance_WYB"]] =  (mu_Appearance_WYB + sig_Appearance_WYB * draws_Appearance_WYB )
  randcoeff[["b_Appearance_WYBPR"]] =  (mu_Appearance_WYBPR + sig_Appearance_WYBPR* draws_Appearance_WYBPR )
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  V = list()
  
  V[["A"]]  = asc_A  + b_Cost *((costA>0)+
                                  b_Plants5  * (plantsA==1) + 
                                  b_Plants10  * (plantsA==2) + 
                                  b_NativeDecrease  * (nativeA==-1)  +
                                  b_NativeIncrease  * (nativeA==1)  +
                                  b_Appearance_W * (appearA==1) +
                                  b_Appearance_WYB * (appearA==2) +
                                  b_Appearance_WYBPR * (appearA==3) +
                                  b_PollinatorQualityMedium * (pollinA==1)+ 
                                  b_PollinatorQualityHigh * (pollinA ==2))
  V[["B"]]  = asc_B  + b_Cost *((costB>0)+
                                  b_Plants5  * (plantsB==1) + 
                                  b_Plants10  * (plantsB==2) + 
                                  b_NativeDecrease  * (nativeB==-1)  +
                                  b_NativeIncrease  * (nativeB==1)  +
                                  b_Appearance_W * (appearB==1) +
                                  b_Appearance_WYB * (appearB==2) +
                                  b_Appearance_WYBPR * (appearB==3) +
                                  b_PollinatorQualityMedium * (pollinB==1)+ 
                                  b_PollinatorQualityHigh * (pollinB ==2))
  V[["C"]]  = asc_C  +b_Cost *((costC>0)+
                                 b_Plants5  * (plantsC==1) + 
                                 b_Plants10  * (plantsC==2) + 
                                 b_NativeDecrease  * (nativeC== -1)  +
                                 b_NativeIncrease  * (nativeC==1)  +
                                 b_Appearance_W * (appearC==1) +
                                 b_Appearance_WYB * (appearC==2) +
                                 b_Appearance_WYBPR * (appearC==3) +
                                 b_PollinatorQualityMedium * (pollinC==1)+ 
                                 b_PollinatorQualityHigh * (pollinC ==2))
  V[["D"]] = asc_D 
  
  mnl_settings = list(
    alternatives  = c(A=1, B=2, C=3, D=4), 
    avail         = list(A=1, B=1, C=1, D=1), 
    choiceVar     = alt,
    utilities     = V
  )
  
  ## Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ## Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ## Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ## Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

## Actually estimates the model
Meadows_MXL_Levels_2022_03_14 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

## Model output and results here alongside saving information
apollo_modelOutput(Meadows_MXL_Levels_2022_03_14,modelOutput_settings = list(printPVal=TRUE))
apollo_saveOutput(Meadows_MXL_Levels_2022_03_14,saveOutput_settings = list(printPVal=TRUE))
saveRDS(Meadows_MXL_Levels_2022_03_14, file="Meadows_MXL_Levels_2022_03_14.rds")


Model <- readRDS("Meadows_MXL_Levels_2022_03_14.rds") ## Enter model of interest RDS here
Meadows_ConditionalWTP <- apollo_conditionals(Model,apollo_probabilities,apollo_inputs )
# Meadows_UnconditionalWTP <- apollo_unconditionals(Model,apollo_probabilities,apollo_inputs )

write.csv(Meadows_ConditionalWTP,"Meadows_MXL_Levels_2022_03_14_WTP.csv")
Meadows_ConditionalWTP <- data.frame(read.csv("Meadows_MXL_Levels_2022_03_14_WTP.csv"))

Meadows_ConditionalWTPSummary <-data.frame(cbind("b_Cost"=Meadows_ConditionalWTP$b_Cost.post.mean,
                                                 "b_Plants5"=Meadows_ConditionalWTP$b_Plants5.post.mean,
                                                 "b_Plants10"=Meadows_ConditionalWTP$b_Plants10.post.mean,
                                                 "b_NativeDecrease"=Meadows_ConditionalWTP$b_NativeDecrease.post.mean,
                                                 "b_NativeIncrease"=Meadows_ConditionalWTP$b_NativeIncrease.post.mean,
                                                 "b_Appearance_W"=Meadows_ConditionalWTP$b_Appearance_W.post.mean,
                                                 "b_Appearance_WYB"=Meadows_ConditionalWTP$b_Appearance_WYB.post.mean,
                                                 "b_Appearance_WYBPR"=Meadows_ConditionalWTP$b_Appearance_WYBPR.post.mean,
                                                 "b_PollinatorQualityMedium"=Meadows_ConditionalWTP$b_PollinatorQualityMedium.post.mean,
                                                 "b_PollinatorQualityHigh"=Meadows_ConditionalWTP$b_PollinatorQualityHigh.post.mean))
apollo_sink()
Meadows_ConditionalWTPSummary %>% summarise(across(everything(),list(mean)))
apollo_sink()

ggsave(melt(Meadows_ConditionalWTPSummary) %>% ggplot(aes(x=value,y=variable,group=variable,fill=variable))+
         geom_density_ridges()+geom_vline(xintercept=0,linetype='dashed')+
         scale_x_continuous(name="mWTP in pounds.", limits=c(-15,15),breaks = seq(-15,15,2)), 
       device = "jpeg",
       filename = "Meadows_ConditionalWTPSummary_2022_03_14.jpeg",
       width=20,height=15,units = "cm",dpi=500)


