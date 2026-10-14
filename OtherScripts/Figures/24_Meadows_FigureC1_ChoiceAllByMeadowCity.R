#### Meadows: Figure C.1 - choice frequencies by meadow type and city ###############
## Function: Figure C.1 - bar chart of % choices per alternative (A/B/C/SQ),
##   split by meadow type and city, from the full (all-tasks) choice data.
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
  here("Data","database_Meadows_Anonymised_Weighted.csv") %>% 
  fread() %>% 
  data.frame()



## Task == 1 keeps one row per respondent (their first choice task) so each
## respondent contributes once to the frequency counts below
Data <- Data %>% dplyr::filter(Task  == 1)


# ****************************************************************
#### Section 2: Plot data setup ####
# ****************************************************************


PlotData <-
  Data %>%
  dplyr::select(choice, Type, city) %>%
  group_by(Type, city, choice) %>%
  summarise(Freq = n(), .groups = "drop") %>% 
  group_by(Type, city) %>%
  mutate(percentage = Freq / sum(Freq) * 100) 



PlotData_Complete <-
  expand.grid(
    Type = 1:3,
    city = 1:3,
    choice = c(1, 2, 3, 4)
  ) %>%
  # Join with existing data
  left_join(PlotData, by = c("Type", "city", "choice")) %>%
  # Replace NA values with 0
  mutate(
    Freq = replace_na(Freq, 0),
    percentage = replace_na(percentage, 0)
  )



# ****************************************************************
#### Section 3: Plot setup ####
# ****************************************************************

TextSize <- 12

TextSetup <- element_text(size = TextSize,
                          colour = "black",
                          family = "serif")


PlotData_Complete$city <- dplyr::recode(PlotData_Complete$city,
              '1' = "Leeds",
              '2' = "Bristol",
              '3' = "Edinburgh") %>% as.factor()


PlotData_Complete$Type <- dplyr::recode(PlotData_Complete$Type,
                                        '1' = "Control",
                                        '2' = "Perennial",
                                        '3' = "Annual") %>%
  factor(levels = c("Control", "Annual", "Perennial"))
# 
# Data$city <- Data$city_label


# ****************************************************************
#### Section 4: Plot  ####
# ****************************************************************


FigureC1_ChoiceAllByMeadowCity <- 
  PlotData_Complete %>%
  
  ggplot(aes(
    x = Type,
    # x = factor(Type, labels = c("Annual",  "Control", "Perennial")),
    y = percentage,
    fill = factor(choice)
  )) +
  
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.9),
    color = "black"
  ) +
  
  scale_x_discrete(name = "Meadow Type") +
  
  scale_y_continuous(
    name = "Percentage of choices for each alternative by meadow type and city ",
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10),
    labels = function(x)
      paste0(x, "%")
  ) +
  
  geom_hline(yintercept = 50,
             alpha = 0.5,
             linetype = "dotted") +
  
  theme_bw() +
  
  facet_grid( ~ city, labeller = as_labeller(
    c(
      "Bristol" = "Bristol",
      "Edinburgh" = "Edinburgh",
      "Leeds" = "Leeds"
    )
  )) +
  
  scale_fill_manual(
    name = "Choice:",
    labels = c("Option A",
               "Option B",
               "Option C",
               "Option D"),
    values = c("darkblue",      # Distinctly medium-toned blue
               "steelblue",       # Deep blue
               "skyblue",        # Lighter but clearly visible
               "powderblue"      # As requested)
    )) +
      
  theme(
        strip.background = element_rect(fill = "white"),
        legend.position = "bottom",
        legend.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.text = TextSetup,
        legend.title = TextSetup,
        axis.text.x = TextSetup,
        axis.text.y = TextSetup,
        axis.title.y = TextSetup,
        axis.title.x = TextSetup
      ) +
  
      coord_cartesian(ylim = c(0, 60))



# ****************************************************************
#### Section 4: Export Plot ####
# ****************************************************************


ggsave(
  FigureC1_ChoiceAllByMeadowCity,
  device = "jpeg",
  filename = here("Outputs", "Plots",
                  "Meadows_FigureC1_ChoiceAllByMeadowCity.jpeg"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)

#### END OF SCRIPT ****************************************************************
