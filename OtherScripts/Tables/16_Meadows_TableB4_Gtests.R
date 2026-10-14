#### Meadows: Table B.4 - sociodemographic G-tests by meadow type ###############
# Script author: Peter King (p.king1@leeds.ac.uk)
# Last Edited: 25/04/2026
# Function: Table B.4 - respondent counts/frequencies by meadow type for each
#   quota-sampling sociodemographic (city, age, education, employment, gender,
#   income), with G-tests for association, plus pre/post entropy-balancing mean
#   differences imported from script 01's output (Table B.4 input file).
# Session info: see README.md ("Replication Information")


# **************************************************************************
#### Libraries:
rm(list = ls())
library(apollo)
library(tidyverse)
library(dplyr)
library(magrittr)
library(mded)
library(here)
library(data.table)
library(matrixStats)
library(ggplot2)
library(ggridges)
library(ggdist)
library(AMR)
library(janitor)


# **************************************************************************
# #### Section 1: Data import and transform ####
# **************************************************************************




# Data <-
#   here("Data", "database_Meadows_2022_07_04.csv") %>% 
#   fread() %>% 
#   data.frame()


Data <-
  here("Data/database_Meadows_Anonymised.csv") %>% 
  fread() %>% 
  data.frame()


# TRANSFORM *****************************************************************


## This is wide format so select only one row per respondent
Data <- Data %>% dplyr::filter(Task  == 1) 

Data <- Data[Data$Education != -999,]
Data <- Data[Data$Employment != -999,]
Data <- Data[!is.na(Data$AgeLevel), ]
Data <- Data[Data$Income > 0, ]



## Convert back to characters
Data$Treatment <- ifelse(Data$Control == 1,
                         "Control",
                         ifelse(
                           Data$Annual == 1,
                           "Annual",
                           ifelse(Data$Perennial == 1, "Perennial", 3)
                         ))


## Convert back to characters
Data$City_category <- ifelse(Data$city == 1,
                         "Leeds",
                         ifelse(
                           Data$city  == 2,
                           "Bristol",
                           ifelse(Data$city  == 3, "Edinburgh", 4)
                         ))




## Total number of respondents. Defined here for repeated later use
Total <- Data %>% nrow()


# **************************************************************************
#### Section 2: Setup vectors ####
# **************************************************************************


## Vector of variable names here
### These are the ones used in the quota sampling
Variable <- c(
  "city",
  "AgeLevel",
  "Education",
  "Employment",
  "Gender",
  "Income"
)


## Trying some alternative variables here to check:
# Variable <- c("Gender","AgeLevels","IncomeBracket","Urbanicity",
#               "ClassDummy","EthnicityDummyWhite","Country")


## Define here such that
### changing the variable list changes the initialised vector lengths
Nvars <- Variable %>% length()


## Initialise results vectors:
## Length = 4 because of four seasonss
Lists_Means <- vector("list", length = 4)
Lists_Freqs <- vector("list", length = 4)
Table_Means <- vector("list", length = Nvars)
Table_Freqs <- vector("list", length = Nvars)
GTest_Ps <- vector("list", length = 4)
GTest_Stats <- vector("list", length = 4)
Table_gtest <- vector("list", length = Nvars)
Table_gtestStats <- vector("list", length = Nvars)
TableData_Means <- matrix(0, 2 * Nvars, 5)


## Custom function that adds significance stars
AddStars <- function(Data) {
  ifelse(Data < 0.01,
         paste0("***"),
         ifelse(Data < 0.05,
                paste0("**"),
                ifelse(Data < 0.1,
                       paste0("*"),
                       paste0(" "))))
}


# **************************************************************************
#### Section 3A: Calculate mean and freq per group using loop ####
# **************************************************************************



## Loop through season then variable:
for (V in Variable){
  for (i in 1:3){
    
    
    Result <- Data %>%
      filter(Type == i) %>%
      dplyr::select(ends_with(V, ignore.case = FALSE)) %>%
      table()
    
    ## Calculate subgroup counts per season and variable
    Lists_Means[[i + 1]] <-
      data.frame(Result %>%
                   data.frame())[, 2]
    
    ## Calculate frequency per season and variable
    Lists_Freqs[[i + 1]] <- data.frame(Result %>%
                                         prop.table() %>%
                                         data.frame())[, 2]
    
  }
  # this function colbinds results for each season
  ## Have to use grep and unlist to format it how I want
  Table_Means[[grep(V, unlist(Variable))]] <- do.call(bind_cols, Lists_Means)
  Table_Freqs[[grep(V, unlist(Variable))]] <- do.call(bind_cols, Lists_Freqs)
  
}



# **************************************************************************
#### Section 3B: Manipulating loop outputs ####
# **************************************************************************


# this function rbinds results for each variable
TableData_Means <-
  rbindlist(use.names = FALSE, idcol = TRUE, Table_Means) %>% data.frame()

TableData_Freqs <-
  rbindlist(use.names = FALSE, idcol = TRUE, Table_Freqs) %>% data.frame()



## Dynamically rename id index with column names:
Index <- TableData_Means$.id
TableData_Means$.id <- Variable[TableData_Means$.id]
colnames(TableData_Means) <-
  c("Category", "Control", "Perennial", "Annual")


## Same as above for consistency and ease of merge
TableData_Freqs$.id <- Variable[TableData_Freqs$.id]
colnames(TableData_Freqs) <-
  c("Category", "Control", "Perennial", "Annual")



# **************************************************************************
#### Section 4A: G test per group and season using loop ####
# **************************************************************************



## Loop through variable: compare Annual and Perennial against Control as base
for (V in Variable) {
  ## Annual vs Control
  Test <- g.test(
    x = c(TableData_Means[TableData_Means$Category == V, "Annual"]) %>% as.numeric(),
    p = c(TableData_Freqs[TableData_Freqs$Category == V, "Control"]) %>% as.numeric()
  )
  GTest_Ps[[1]]    <- Test$p.value
  GTest_Stats[[1]] <- Test$statistic

  ## Perennial vs Control
  Test <- g.test(
    x = c(TableData_Means[TableData_Means$Category == V, "Perennial"]) %>% as.numeric(),
    p = c(TableData_Freqs[TableData_Freqs$Category == V, "Control"]) %>% as.numeric()
  )
  GTest_Ps[[2]]    <- Test$p.value
  GTest_Stats[[2]] <- Test$statistic

  ## stitch together for each loop, this here for P values
  Table_gtest[[grep(V, unlist(Variable))]] <-
    do.call(bind_cols, GTest_Ps)

  ## and this for statistics
  Table_gtestStats[[grep(V, unlist(Variable))]] <-
    do.call(bind_cols, GTest_Stats)
}

# **************************************************************************
#### Section 4B: Loop outputs ####
# **************************************************************************


## Combine all results
Table_GTest_Results <- rbindlist(Table_gtest, use.names = FALSE) %>% data.frame()
Table_GTest_ResultsStats <- rbindlist(Table_gtestStats, use.names = FALSE) %>% data.frame()


## Label dimensions
rownames(Table_GTest_Results) <- Variable
colnames(Table_GTest_Results) <- c("Annual", "Perennial")

rownames(Table_GTest_ResultsStats) <- Variable
colnames(Table_GTest_ResultsStats) <- c("Annual", "Perennial")

Table_GTest_ResultsPV_Data <- Table_GTest_Results

## Add stars
Table_GTest_ResultsPVs <- Table_GTest_ResultsPV_Data %>% lapply(function(Data) ifelse(
  Data < 0.01,
  paste0(Data %>% round(3) %>% sprintf("%.3f", .), " ***"),
  ifelse(
    Data < 0.05,
    paste0(Data %>% round(3) %>% sprintf("%.3f", .), " **"),
    ifelse(
      Data < 0.1,
      paste0(Data %>% round(3) %>% sprintf("%.3f", .), " *"),
      paste0(Data %>% round(3) %>% sprintf("%.3f", .), " "))))) %>% as.data.frame()


## make row names consistent
rownames(Table_GTest_ResultsPVs) <- rownames(Table_GTest_ResultsPV_Data)


Table_GTest_Results_Transform <- AddStars(Table_GTest_Results) %>% data.frame()
Table_GTest_Results_Transform <- Table_GTest_Results_Transform[Index, ]
# Table_GTest_Results_Transform$Category <-
# data.frame("Category"=Table_GTest_Results_Transform %>% rownames())



# **************************************************************************
#### Section 5A: Combining outputs ####
# **************************************************************************


## simpler as no tests for winter
Annual <- data.frame(
  "Annual" = paste0(
    TableData_Means$Annual,
    " (",
    TableData_Freqs$Annual %>% multiply_by(100) %>% round(2) %>% sprintf("%.2f", .),
    "%) "
  )
)



## spring only
Perennial <- data.frame(
  "Perennial" = paste0(
    TableData_Means$Perennial,
    " (",
    TableData_Freqs$Perennial %>% multiply_by(100) %>% round(2) %>% sprintf("%.2f", .),
    "%) "
  )
)

## summer here
Control <- data.frame(
  "Control" = paste0(
    TableData_Means$Control,
    " (",
    TableData_Freqs$Control %>% multiply_by(100) %>% round(2) %>% sprintf("%.2f", .),
    "%) "
  )
)



# *********************************************
# Tidying up G test part
# *********************************************

# Replace your existing CategoryList object with this reordered version

CategoryList <- c(
  # city (alphabetical)
  "Bristol",
  "Edinburgh",
  "Leeds",
  
  # AgeLevel
  "18 – 24",
  "25 – 34",
  "35 – 44",
  "45 – 54",
  "55 – 64",
  "65 and over",
  
  # Education
  "None",
  "1 - 4 O Levels or equivalent",
  "5 + O Levels/CSEs/GCSEs or equivalent",
  "2 + A Levels or equivalent",
  "Degree, Higher Degree, NVQ level 4-5, BTEC Higher Level, professional qualifications",
  "Other qualifications",
  
  # Employment
  "Employed",
  "Unemployed, but looking for work",
  "Not working",
  "Retired",
  "Student",
  
  # Gender
  "Female",
  "Male",
  
  # Income
  "Up to £5,199",
  "£5,200 – 10,399",
  "£10,400 – £15,599",
  "£16,000 - £20,779",
  "£20,800 - £25,999",
  "£26,000 - £31,199",
  "£31,200 - £36,399",
  "£36,400 - £51,999",
  "£52,000 and above"
)


## Stitching all together column wise
TestOutput <- bind_cols(
  "Variable" = TableData_Means$Category,
  "Category" = CategoryList,
  "Control" = Control,
  "Annual" = Annual,
  "Perennial" = Perennial
)


# *********************************************
# City breakdown rows (tabyl avoids the city-code label mismatch in the loop)
# *********************************************

city_n   <- Data %>% tabyl(City_category, Treatment)
city_pct <- Data %>% tabyl(City_category, Treatment) %>% adorn_percentages("col")

city_ord <- c("Bristol", "Edinburgh", "Leeds")
city_n   <- city_n[match(city_ord, city_n$City_category), ]
city_pct <- city_pct[match(city_ord, city_pct$City_category), ]

City_Rows <- data.frame(
  Variable  = "city",
  Category  = city_n$City_category,
  Control   = paste0(city_n$Control,   " (", sprintf("%.2f", city_pct$Control   * 100), "%) "),
  Annual    = paste0(city_n$Annual,    " (", sprintf("%.2f", city_pct$Annual    * 100), "%) "),
  Perennial = paste0(city_n$Perennial, " (", sprintf("%.2f", city_pct$Perennial * 100), "%) "),
  stringsAsFactors = FALSE
)


# *********************************************
# Importing balance report
# *********************************************


Table1X_EBWeights <- here("Outputs/Tables/Meadows_TableB4Input_EBWeightSummary.txt") %>%
  fread() %>%
  data.frame()




## Not elegant but it works at stitching the p values where I want
Final <- rbind(
  City_Rows,

  TestOutput[TestOutput$Variable == Variable[2], ],
  cbind("Variable" = Variable[2], "Category" = 0, Table_GTest_ResultsPVs[2, ], "Control" = 0),
  cbind("Variable" = "AgeLevel", 
        "Category" = "Pre-balancing", 
        "Annual" = Table1X_EBWeights[1, 3], 
        "Perennial" = Table1X_EBWeights[1, 4], 
        "Control" = 0),
  cbind("Variable" = "AgeLevel", 
        "Category" = "Post-balancing", 
        "Annual" = Table1X_EBWeights[2, 3], 
        "Perennial" = Table1X_EBWeights[2, 4], 
        "Control" = 0),

    
  TestOutput[TestOutput$Variable == Variable[3], ],
  cbind("Variable" = Variable[3], "Category" = 0, Table_GTest_ResultsPVs[3, ], "Control" = 0),
  cbind("Variable" = "Education", 
        "Category" = "Pre-balancing", 
        "Annual" = Table1X_EBWeights[3, 3], 
        "Perennial" = Table1X_EBWeights[3, 4], 
        "Control" = 0),
  cbind("Variable" = "Education", 
        "Category" = "Post-balancing", 
        "Annual" = Table1X_EBWeights[4, 3], 
        "Perennial" = Table1X_EBWeights[4, 4], 
        "Control" = 0),
  
  
  TestOutput[TestOutput$Variable == Variable[4], ],
  cbind("Variable" = Variable[4], "Category" = 0, Table_GTest_ResultsPVs[4, ], "Control" = 0),
  cbind("Variable" = "Employment", 
        "Category" = "Pre-balancing", 
        "Annual" = Table1X_EBWeights[5, 3], 
        "Perennial" = Table1X_EBWeights[5, 4], 
        "Control" = 0),
  cbind("Variable" = "Employment", 
        "Category" = "Post-balancing", 
        "Annual" = Table1X_EBWeights[6, 3], 
        "Perennial" = Table1X_EBWeights[6, 4], 
        "Control" = 0),
  
  
  TestOutput[TestOutput$Variable == Variable[5], ],
  cbind("Variable" = Variable[5], "Category" = 0, Table_GTest_ResultsPVs[5, ], "Control" = 0),
  cbind("Variable" = "Gender", 
        "Category" = "Pre-balancing", 
        "Annual" = Table1X_EBWeights[7, 3], 
        "Perennial" = Table1X_EBWeights[7, 4], 
        "Control" = 0),
  cbind("Variable" = "Gender", 
        "Category" = "Post-balancing", 
        "Annual" = Table1X_EBWeights[8, 3], 
        "Perennial" = Table1X_EBWeights[8, 4], 
        "Control" = 0),
  
  
  TestOutput[TestOutput$Variable == Variable[6], ],
  cbind("Variable" = Variable[6], "Category" = 0, Table_GTest_ResultsPVs[6, ], "Control" = 0),
  cbind("Variable" = "Income", 
        "Category" = "Pre-balancing", 
        "Annual" = Table1X_EBWeights[9, 3], 
        "Perennial" = Table1X_EBWeights[9, 4], 
        "Control" = 0),
  cbind("Variable" = "Income", 
        "Category" = "Post-balancing", 
        "Annual" = Table1X_EBWeights[10, 3], 
        "Perennial" = Table1X_EBWeights[10, 4], 
        "Control" = 0)
  
)


# *********************************************
# Exporting
# *********************************************


Final <- Final[, c("Variable", "Category", "Control", "Annual", "Perennial")]

Final %>% fwrite(sep = "#",
                      quote = FALSE,
                      file = paste0(here(
                        "Outputs", "Tables",
                        "Meadows_TableB4_Gtests.txt"
                      )))


# End Of Script **************************************************************
