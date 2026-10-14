#### Meadows: Figure C.1 (SQ variant) - status-quo choice share by type/city #########
## Function: Companion to script 24: collapses the 4-alternative choice down to a
##   binary status-quo (choice == 4) vs any-treatment indicator, then plots %
##   choosing the status quo per meadow type and city.
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

## Task == 1 keeps one row per respondent (their first choice task) so each
## respondent contributes once to the frequency counts below
Data <- Data %>% dplyr::filter(Task  == 1)



# ****************************************************************
#### Section 2: Plot data setup ####
# ****************************************************************


PlotData <-
  Data %>%
  dplyr::select(choice, Type, city) %>%
  mutate(binary_choice = ifelse(choice == 4, 1, 0)) %>%
  group_by(Type, city, binary_choice) %>%
  summarise(Freq = n(), .groups = "drop") %>% 
  group_by(Type, city) %>%
  mutate(percentage = Freq / sum(Freq) * 100)


# Necessary for adding a zero bar
PlotData_Complete <-
expand.grid(
  Type = 1:3,
  city = 1:3,
  binary_choice = c(0,1)
) %>%
  # Join with existing data
  left_join(PlotData, by = c("Type", "city", "binary_choice")) %>%
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



# ****************************************************************
#### Section 4: Plot  ####
# ****************************************************************


FigureD1_ChoiceSQByMeadowCity <- 
PlotData_Complete %>%
  ggplot(aes(
    x = factor(Type, labels = c("Annual", "Perennial", "Control")),
    y = percentage,
    fill = factor(binary_choice)
  )) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.9),
    color = "black"
  ) +
  scale_x_discrete(name = "Meadow Type") +
  scale_y_continuous(
    name = "Percentage of choices for the status quo by meadow type and city ",
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10),
    labels = function(x)
      paste0(x, "%")
  ) +
  geom_hline(yintercept = 50,
             alpha = 0.5,
             linetype = "dotted") +
  theme_bw() +
  facet_grid(~ city, labeller = as_labeller(c(
    "1" = "Leeds",
    "2" = "Bristol",
    "3" = "Edinburgh"
  ))) +
  scale_fill_manual(
    name = "Choice:",
    labels = c("Options A-C", "Option D"),
    values = c("lightblue", "darkblue")
  ) +
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
  )



# ****************************************************************
#### Section 4: Export Plot ####
# ****************************************************************


ggsave(
  FigureD1_ChoiceSQByMeadowCity,
  device = "jpeg",
  filename = here("Outputs", "Plots",
                  "Meadows_FigureC1_ChoiceSQByMeadowCity.jpeg"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)

#### END OF SCRIPT ****************************************************************
