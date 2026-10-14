#### Meadows: Figure B.2 - balancing variable distributions by meadow type ####
## Function: Figure B.2 - one histogram panel per variable (age, education,
##   employment, gender, income, plus city), each showing its distribution by
##   meadow type (y-axis), to visually check pre-balancing sample differences.
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
  here("Data/database_Meadows_Anonymised_Weighted.csv") %>% 
  fread() %>% 
  data.frame()


# ****************************************************************
#### Section 2: Prepare Data ####
# ****************************************************************


Data <- Data %>% dplyr::filter(Task  == 1) 
Data <- Data %>% dplyr::filter(Income > -999 & 
                                 Employment > -999 &
                                 Education > -999)


Data$Type_recode <- dplyr::recode(Data$Type,
                                  "1" = "Control",
                                  "2" = "Perennial",
                                  "3" = "Annual"
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



# Data$Clean_Park_ID <- Data$park...34
# 
# # List of suffixes (and potential prefixes with underscore) to remove
# # The regex pattern:
# # (?:_?Control|_?Annual|_?Perennial)$
# # Explanation:
# # (?: ... )  -> non-capturing group
# # _?         -> matches an underscore zero or one time (optional underscore)
# # Control    -> matches the literal string "Control"
# # |          -> OR operator
# # $          -> asserts position at the end of the string
# suffixes_to_remove <- c("_Control", "_Annual", "_Perennial")    # Without, just in case
# 
# # This pattern tries to remove "_Control", "_Annual", "_Perennial" at the end.
# Data$Clean_Park_ID <- gsub("(?:_Control|_Annual|_Perennial|_Perrenial)$", "", Data$Clean_Park_ID)
# 
# 
# ## Start effects coding
# Data$Clean_Park_ID <- as.factor(Data$Clean_Park_ID)
# all_park_levels <- levels(Data$Clean_Park_ID)
# 
# # Choose a reference level park consistently
# ref_level_park <- all_park_levels[length(all_park_levels)] # Or pick one by name
# # print(paste("Reference park level for effects coding:", ref_level_park))
# 
# parks_to_code <- all_park_levels[all_park_levels != ref_level_park]
# 
# for (p in parks_to_code) {
#   # Sanitize park names for variable naming
#   clean_var_suffix <- gsub("[^A-Za-z0-9_]", "", p) 
#   var_name <- paste0("EC_Park_", clean_var_suffix)
#   
#   Data[[var_name]] <- ifelse(Data$Clean_Park_ID == p, 1, 
#                                  ifelse(Data$Clean_Park_ID == ref_level_park, -1, 0))
# }




PlotData <- Data %>% 
  dplyr::select(
    Type_recode,
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


TextSize <- 16


TextSetup <- element_text(size = TextSize,
                          colour = "black",
                          family = "serif")


# ****************************************************************
#### Section 4: Plot ####
# ****************************************************************


FigureC2 <- 
  PlotData  %>%
  
  ggplot(aes(
    x = Value %>% as.factor(),
    y = Type_recode %>% as.factor(),
    group = Type_recode %>% as.factor(),
    fill = Type_recode %>% as.factor()
  )) +
  
  ggdist::stat_histinterval(normalize = "groups",
                            outline_bars = TRUE,
                            height = 0.6,
                            slab_colour = "black",
                            slab_linewidth = 0.6) +
  
  theme_bw() +
  
  facet_wrap(~ Variable, 
             scales = "free",
             nrow = 3, 
             ncol = 2, 
             labeller = as_labeller(c(
               "Type_recode" = "Meadow Type,",
               "city_recode" = "City",
               "AgeLevel_recode" = "Age",
               "Education_recode" = "Education",
               "Employment_recode" = "Employment",
               "Gender_recode" = "Gender",
               "Income_recode" = "Income"
             ))) +
  
  scale_y_discrete(name = "Meadow Type", 
                   labels = NULL,
                   expand = expansion(add = c(0.2, 0.8))) +
  
  xlab("Categorical Value") +
  
  scale_fill_manual(
    name = "Meadow Type",
    values = c("Control" = "dodgerblue", "Annual" = "skyblue", "Perennial" = "blue"),
    breaks = c("Control", "Annual", "Perennial"),
    labels = c("Control", "Annual", "Perennial")) +
  
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
    panel.spacing.y = unit(0.25, "cm"),
    plot.margin = unit(c(0.5, 0.3, 0.3, 0.3), "cm"),
    legend.text = TextSetup,
    legend.title = TextSetup,
    strip.text.x = TextSetup,
    axis.text.y = TextSetup,
    axis.title.x = TextSetup,
    axis.title.y = TextSetup,
    strip.text = TextSetup,
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = TextSize,
      colour = "black",
      family = "serif")
  )

# ****************************************************************
#### Section 5: Export Plot ####
# ****************************************************************


ggsave(
  FigureC2,
  device = "jpeg",
  filename = here("Outputs", "Plots",
                  "Meadows_FigureB2_SampleDistributionByMeadow.jpeg"),
  width = 28,
  height = 38,
  units = "cm",
  dpi = 500
)


#### END OF SCRIPT ****************************************************************