#### Meadows: Figure B.2 (binary variant) - balancing distributions, Control vs Treated ####
## Function: Companion to script 17: same one-panel-per-variable histograms, but
##   with meadow type collapsed to binary Control vs Treated (Annual+Perennial).
## Author: Dr Peter King (p.king1@leeds.ac.uk)
## Last change: 29/05/2025
## Session info: see README.md ("Replication Information")

# *****************************
# Setup Environment: ####
# *****************************


library(magrittr)
library(dplyr)
library(apollo)
library(reshape2)
library(ggplot2)
library(ggridges)
library(distributional)
library(ggdist)
library(gridExtra)
library(data.table)
library(here)
library(tidyverse)


# ****************************************************************
#### Section 1: Import Data ####
# ****************************************************************



Data <-
  here("Data", "database_Meadows_Anonymised_Weighted.csv") %>% 
  fread() %>% 
  data.frame()


# ****************************************************************
#### Section 2: Prepare Data ####
# ****************************************************************


Data <- Data %>% dplyr::filter(Task  == 1) 
Data <- Data %>% dplyr::filter(Income > -999 & 
                                 Employment > -999 &
                                 Education > -999)

Data$Treated <- ifelse(Data$Type == 3, 0, 1)


Data$Treated_recode <- dplyr::recode(Data$Treated,
                                  "1" = "Annual or Perennial", 
                                  "0" = "Control"
)



Data$Type_recode <- dplyr::recode(Data$Type,
                                  "1" = "Annual",
                                  "2" = "Perennial", 
                                  "3" = "Control"
)

Data$city_recode <- dplyr::recode(Data$city,
                                  "1" = "Leeds",
                                  "2" = "Bristol", 
                                  "3" = "Edinburgh"
)

Data$AgeLevel_recode <- dplyr::recode(Data$AgeLevel,
                                      "18" = "18-24",
                                      "25" = "25-34",
                                      "35" = "35-44",
                                      "45" = "45-54",
                                      "55" = "55-64",
                                      "65" = "65+"
)

Data$Education_recode <- dplyr::recode(Data$Education,
                                       "1" = "No qualifications",
                                       "2" = "1-4 GCSEs",
                                       "3" = "5+ GCSEs",
                                       "4" = "A Levels",
                                       "5" = "Degree+",
                                       "6" = "Other"
)

Data$Employment_recode <- dplyr::recode(Data$Employment,
                                        "1" = "Employed",
                                        "2" = "Unemployed",
                                        "3" = "Not working",
                                        "4" = "Retired",
                                        "5" = "Student"
)

Data$Income_recode <- dplyr::recode(Data$Income,
                                    "2500" = "< £5.2k",
                                    "5200" = "£5.2-10.4k",
                                    "10400" = "£10.4-15.6k",
                                    "15559" = "£15.6-20.8k",
                                    "20800" = "£20.8-26k",
                                    "26000" = "£26-31.2k",
                                    "31200" = "£31.2-36.4k",
                                    "36400" = "£36.4-52k",
                                    "52000" = "£52k+"
)


Data$Gender_recode <- dplyr::recode(Data$Gender,
                                    "0" = "Male",
                                    "1" = "Female"
)



PlotData <- Data %>% 
  dplyr::select(
    Treated_recode,
    city_recode,
    AgeLevel_recode,
    Education_recode,
    Employment_recode,
    Gender_recode,
    Income_recode
  ) %>% 
  pivot_longer(cols = 2:7, 
               names_to = "Variable", 
               values_to = "Value")



# ****************************************************************
#### Section 3: Plot setup ####
# ****************************************************************


TextSize <- 14


TextSetup <- element_text(size = TextSize,
                          colour = "black",
                          family = "serif")


# ****************************************************************
#### Section 4: Plot ####
# ****************************************************************


FigureX <- 
  PlotData  %>%
  
  ggplot(aes(
    x = Value %>% as.factor(),
    y = Treated_recode %>% as.factor(),
    group = Treated_recode %>% as.factor(),
    fill = Treated_recode %>% as.factor()
  )) +
  
  ggdist::stat_histinterval(normalize = "groups",
                            outline_bars = TRUE,
                            slab_colour = "black",
                            slab_linewidth = 0.45) +
  
  theme_bw() +
  
  facet_wrap(~ Variable, 
             scales = "free",
             nrow = 3, 
             ncol = 2, 
             labeller = as_labeller(c(
               "Treated_recode" = "Meadow Type,",
               "city_recode" = "City",
               "AgeLevel_recode" = "Age",
               "Education_recode" = "Education",
               "Employment_recode" = "Employment",
               "Gender_recode" = "Gender",
               "Income_recode" = "Income"
             ))) +
  
  scale_y_discrete(name = "Meadow Type", labels = NULL) + 
  
  xlab("Categorical Value") +
  
  scale_fill_manual(
    name = "Meadow Type", 
    values = c("skyblue", "blue"),
    labels = c("Control", "Annual or Perennial")) +
  
  scale_point_size_continuous(guide = NULL) +
  
  scale_slab_colour_discrete(guide = NULL) +
  
  theme(
    strip.background = element_rect(fill = "white"),
    legend.position = "bottom",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.text = TextSetup,
    legend.title = TextSetup,
    axis.text.y = TextSetup,
    axis.title.y = TextSetup,
    strip.text = TextSetup,
    axis.text.x = element_text(
      angle = 25, 
      hjust = 0.9,
      size = 8,
      colour = "black",
      family = "serif")
  )


# ****************************************************************
#### Section 5: Export Plot ####
# ****************************************************************


ggsave(
  FigureX,
  device = "jpeg",
  filename = here("Outputs", "Plots",
                  "Meadows_FigureB2_SampleDistributionByMeadow_Binary.jpeg"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)


#### END OF SCRIPT ****************************************************************