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

#------------------------------
# Section 1: Import Data ####
# Load RData from TL dropbox
#------------------------------


## From dropbox:
load("finalmeadows_R.rdata")


#------------------------------
# Section 2: Reshape data ####
# not pretty but works
#------------------------------


finalmeadows_R$alt<-1:4
data<- finalmeadows_R %>% dplyr::select( alt, oid,card,choice,cost, sq,cost, mplants,hplants,lnative, hnative, mpollin, hpollin, whiapp, bluapp, allapp)
data$av<-1
data1 <-subset(data, data$alt==1)
data2 <-subset(data, data$alt==2)
data3 <-subset(data, data$alt==3)
data4 <-subset(data, data$alt==4)


colnames(data1) <- paste(colnames(data1), 1:1, sep = "")
colnames(data2) <- paste(colnames(data2), 2:2, sep = "")
colnames(data3) <- paste(colnames(data3), 3:3, sep = "")
colnames(data4) <- paste(colnames(data4), 4:4, sep = "")
# data1<-rename (data1, oid=oid1)
# data2<-rename (data2, oid=oid2)
# data3<-rename (data3, oid=oid3)
# data4<-rename (data4, oid=oid4)
# 
# data1<-rename (data1, card=card1)
# data2<-rename (data2, card=card2)
# data3<-rename (data3, card=card3)
# data4<-rename (data4, card=card4)

#Deleting the alt column (which is the first)
data1 <- data1[ -c(1) ]
data2 <- data2[ -c(1) ]
data3 <- data3[ -c(1) ]
data4 <- data4[ -c(1) ]

df<-cbind(data1, data2, data3, data4)

drops <- c("oid2","card2", "oid3","card3","oid4","card4")
df<-df[ , !(names(df) %in% drops)]

df<-rename(df, card=card1)
df<-rename(df, oid=oid1)
df$choice<-ifelse(df$choice1==1,1,ifelse(df$choice2==1,2,ifelse(df$choice3==1,3,ifelse(df$choice4==1,4,0))))

df$asc1<-1
df$asc2<-1
df$asc3<-1
df$asc4<-0



#------------------------------
# Section 2: Save as CSV ####
# NOTE: Apollo needs data to be called "database"
#------------------------------


## Export:
write.csv(database,"database_Meadows_2022_06_03.csv")



# End Of Script ------------------------------------------------------------------------------------------------------