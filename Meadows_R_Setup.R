#### Meadows Paper ####
## Function: Estimates a basic MNL model
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 30/03/2022 added some geocoding
## TODO: updated


rm(list = ls())
setwd("Z:/Meadows")

library(readxl)
library(PostcodesioR)
library(tidygeocoder)
library(dplyr)

Data2 <- data.frame(read_excel("meadowslatest4.xls"))
# Data2$alt <- rep(1:4,nrow(Data2)/4)
Data2$Task <- rep(rep(1:6,each=4),times=nrow(Data2)/24)
Data2$ID <- rep(1:(nrow(Data2)/24),each=24)
# Data2$Task <- rep(1:6,nrow(Data2)/6)
# 
# ## Find IDs of serial non-participators
Reduce(intersect,list(
  Data2$oid[Data2$Task == 1 &
           Data2$alt == 4],
Data2$oid[Data2$Task == 2 &
            Data2$alt == 4],
Data2$oid[Data2$Task == 3 &
              Data2$alt == 4],
Data2$oid[Data2$Task == 4 &
              Data2$alt == 4],
Data2$oid[Data2$Task == 5 &
            Data2$alt == 4],
Data2$oid[Data2$Task == 6 &
            Data2$alt == 4]))



Data2$plantsA <- Data2$plants[Data2$alt==1]
Data2$plantsB <- Data2$plants[Data2$alt==2]
Data2$plantsC <- Data2$plants[Data2$alt==3]
Data2$plantsD <- Data2$plants[Data2$alt==4]

Data2$nativeA <- Data2$native[Data2$alt==1]
Data2$nativeB <- Data2$native[Data2$alt==2]
Data2$nativeC <- Data2$native[Data2$alt==3]
Data2$nativeD <- Data2$native[Data2$alt==4]

Data2$pollinA <- Data2$pollin[Data2$alt==1]
Data2$pollinB <- Data2$pollin[Data2$alt==2]
Data2$pollinC <- Data2$pollin[Data2$alt==3]
Data2$pollinD <- Data2$pollin[Data2$alt==4]

Data2$appearA <- Data2$appear[Data2$alt==1]
Data2$appearB <- Data2$appear[Data2$alt==2]
Data2$appearC <- Data2$appear[Data2$alt==3]
Data2$appearD <- Data2$appear[Data2$alt==4]

# Data2$cost <- Data2$cost/100
Data2$costA <- Data2$cost[Data2$alt==1]
Data2$costB <- Data2$cost[Data2$alt==2]
Data2$costC <- Data2$cost[Data2$alt==3]
Data2$costD <- Data2$cost[Data2$alt==4]


colnames(Data2)[which(names(Data2)=="inc")] <- "Income"
colnames(Data2)[which(names(Data2)=="envmem")] <- "Charity"

colnames(Data2)[which(names(Data2)=="cnsmean")] <- "CNS_ScaleMean"
colnames(Data2)[which(names(Data2)=="employ")] <- "Employment"
colnames(Data2)[which(names(Data2)=="edu")] <- "Education"
colnames(Data2)[which(names(Data2)=="ageb")] <- "AgeLevel"
colnames(Data2)[which(names(Data2)=="male")] <- "Gender"

colnames(Data2)[which(names(Data2)=="plest")] <- "PlantGuess"
colnames(Data2)[which(names(Data2)=="natest")] <- "NativeGuess"
colnames(Data2)[which(names(Data2)=="colest")] <- "ColourGuess"
colnames(Data2)[which(names(Data2)=="polest")] <- "PollinGuess"


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
# 


Data2 <- Data2[Data2$choice==1,]
# Data2$ID <- as.numeric(rep(1:(nrow(Data2)/6),each=6))
Data2 <- Data2[order(Data2$ID),]
#### Trying to truncate 570 to 563 ####





# Data2$ID <- as.numeric(factor(Data2$Questionnaire_ID,levels=unique(Data2$Questionnaire_ID)))



# ## To Test for those who always choose status quo: 
# Data2$NonParticipating <- rep(0,nrow(Data2)) ## Initialize vector of zeroes
# # Now test whether one person (ID==i), always chooses 4.
# for (i in 1:length(Data2$ID)) {
#   Data2$NonParticipating[i] = sum(4 == (Data2$alt[Data2$ID==Data2$ID[i]]))
# }




database <- data.frame(Data2)

colnames(database)[which(names(database)=="inc")] <- "Income"
colnames(database)[which(names(database)=="envmem")] <- "Charity"

colnames(database)[which(names(database)=="cnsmean")] <- "CNS_ScaleMean"
colnames(database)[which(names(database)=="employ")] <- "Employment"
colnames(database)[which(names(database)=="edu")] <- "Education"
colnames(database)[which(names(database)=="ageb")] <- "AgeLevel"
colnames(database)[which(names(database)=="male")] <- "Gender"

colnames(database)[which(names(database)=="plest")] <- "PlantGuess"
colnames(database)[which(names(database)=="natest")] <- "NativeGuess"
colnames(database)[which(names(database)=="colest")] <- "ColourGuess"
colnames(database)[which(names(database)=="polest")] <- "PollinGuess"



write.csv(database,"database_Meadows_2022_03_11.csv")


#-----------------------------
#### Some basic geocoding #### 
#-----------------------------


## Code Cities by Name to facilitate lookup
database$CityName <- ifelse(database$city==1,
                            "Leeds",
                            ifelse(database$city==2,
                                           "Bristol","Edinburgh"))

## Combine park name with city name
M1 <- paste0(
  gsub("_Control", replacement = "",x = unique(database$park...34)[1]),
  ",",
  unique(database$CityName[database$park...34==unique(database$park...34)[1]]))


## Geocode park
M1 <-geo(address=c(M1),full_results = TRUE)$display_name


## Lookup postcode details of park:
postcode_lookup(substring(M1,unlist(gregexpr("[0-9]", M1))[1]-2,unlist(gregexpr("[0-9]", M1))[1]+5))