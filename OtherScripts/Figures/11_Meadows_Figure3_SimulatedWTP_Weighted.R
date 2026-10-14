#### Meadows: Figure 3 - simulated WTP boxplots (weighted) ###############
# AUTHOR: Peter King (p.king1@leeds.ac.uk)
# FUNCTION: Figure 3 - box-and-whisker plot of simulated mean WTP per attribute
#   and meadow type (weighted main model, 10,000 Krinsky-Robb draws from
#   script 06). Also writes two non-manuscript WTP-summary diagnostic files.
# LAST CHANGE: 24/09/2026


# *************************************************************
#### Section 0: Setup libraries ####
# *************************************************************

# install.packages("data.table",repos="http://cran.us.r-project.org")
options(scipen = 90)
library(tidyverse)
library(dplyr)
library(magrittr)
library(mded)
library(here)
library(data.table)
library(stats)
library(ggdist)
library(mvtnorm)
library(apollo)
# library(cmdlr)


# *************************************************************
#### Section 1: Import data  ####
# *************************************************************

Data <- here(
  "ModelOutputs",
  "SimulatedMeans_AllTypes_Weighted.csv") %>% 
  fread() %>% 
  data.frame()


# *************************************************************
#### Section 2: Setup mapping  ####
# *************************************************************


# 
# Custom mapping for y-axis labels
Labels <- c(
  "Flower colours:\n white",
  "Flower colours:\n white, yellow, blue",
  "Flower colours:\n white, yellow, blue,\n pink, red",
  "Native:\n decrease",
  "Native:\n increase",
  "Plants:\n five species",
  "Plants:\n ten species",
  "Quality for pollinating insects:\n medium",
  "Quality for pollinating insects:\n high"
)

Labels_Tidied <-  c("Appearance_W",
                    "Appearance_WYB",
                    "Appearance_WYBPR",
                    "Native_Decrease",
                    "Native_Increase",
                    "Plants_5",
                    "Plants_10",
                    "PollinatorQuality_Medium",
                    "PollinatorQuality_High")


## Plotting order of meadow types within each attribute level
MeadowOrder <- c("Control", "Annual", "Perennial")


TextSetup <- element_text(size = 14,
                          colour = "black",
                          family = "serif")

custom_colors <- c(
  "dodgerblue",        # Annual
  "powderblue",      # Control meadows
  "navyblue"      # Perennial
)


# *************************************************************
#### Section 1B: Summarise data  ####
# *************************************************************


## Reshape here
## Outputs/Old/ is not tracked, so create it before writing the intermediate summaries
dir.create(here("Outputs", "Old", "Tables"), recursive = TRUE, showWarnings = FALSE)

TableCX_WTPSummmary <- Data %>%
  pivot_longer(cols = 1:9) %>%
  dplyr::mutate(name = factor(name, levels = Labels_Tidied)) %>%
  group_by(meadow_type, name) %>%
  summarise("WTP" = paste0(
    "£",
    mean(value) %>% round(2) %>% sprintf("%.2f", .),
    " (£",
    sd(value) %>% round(2) %>% sprintf("%.2f", .),
    ")"
  )) %>%
  pivot_wider(names_from = meadow_type, values_from = WTP)


## Diagnostic only (mean (SD) per attribute/meadow type) - not a manuscript table
TableCX_WTPSummmary %>%
  data.frame() %>%
  fwrite(
  sep = ",",
  here("Outputs", "Old", "Tables", "TableCX_WTPSummmary_Weighted.txt")
)


## Ensure that moments are correctly formatted
TableCX_WTPSummmary_Detailed <- Data %>%
  pivot_longer(cols = 1:9) %>%
  group_by(meadow_type, name) %>%
  summarise(
    "Median" = paste0("£",
                      median(value) %>% round(2) %>% sprintf("%.2f", .)),
    "Mean" = paste0("£", mean(value) %>% round(2) %>% sprintf("%.2f", .)),
    "SD" = paste0("£", sd(value) %>% round(2) %>% sprintf("%.2f", .)),
    "Lower" = paste0("£", quantile(value, c(0.025)) %>% round(2) %>% sprintf("%.2f", .)),
    "Upper" = paste0("£", quantile(value, c(0.975)) %>% round(2) %>% sprintf("%.2f", .)))


## Diagnostic only (median/mean/SD/95% CI per attribute/meadow type) - not a
## manuscript table; kept for cross-checking against Table C.2 (script 20)
TableCX_WTPSummmary_Detailed %>%
  data.frame() %>%
  fwrite(
    sep = ",",
    here("Outputs", "Old", "Tables", "TableCX_WTPSummmary_Detailed_Weighted.txt")
  )





## HERE WE HAVE MEAN (95% CI)
Data %>%
  pivot_longer(cols = 1:9) %>%
  group_by(meadow_type, name) %>%
  summarise(
    "WTP" = paste0("£", mean(value) %>% round(2) %>% sprintf("%.2f", .),
                   " (£",
                   quantile(value, c(0.025)) %>% round(2) %>% sprintf("%.2f", .),
                   " - £",
                   quantile(value, c(0.975)) %>% round(2) %>% sprintf("%.2f", .), ")")) %>% 
  pivot_wider(names_from = meadow_type, values_from = WTP)



# *************************************************************
#### Section 2: Plot  ####
# *************************************************************


Figure <- Data %>%
  
  pivot_longer(cols = 1:9) %>%
  
  ## Control first in the dodge so it sits at the edge of each group, matching the legend
  dplyr::mutate(name = factor(name, levels = Labels_Tidied),
                meadow_type = factor(meadow_type, levels = MeadowOrder)) %>%


  ggplot(aes(
    y = name  %>% rev(),
    x = value,
    fill = meadow_type
  )) +
  
  
  stat_boxplot(geom = "errorbar",
               width = 0.65,
               position = position_dodge(width = 0.75)) +
  
  
  geom_boxplot(outlier.shape = NA, notch = TRUE) +
  
  scale_x_continuous(name = "Marginal WTP (GBP) in local council tax, per household, per annum",
                     breaks = seq.int(from = -100, 
                                      to = 100, 
                                      by = 20)) +
  
  scale_y_discrete(name = "Attribute and level", labels = Labels %>% rev()) +
  
  scale_fill_manual(
    name = "Meadow type:",
    values = c(
      "Annual"    = "dodgerblue",
      "Control"   = "powderblue",
      "Perennial" = "navyblue"
    ),
    breaks = c("Control", "Annual", "Perennial"),
    labels = c(
      "Control\n (N = 157)",
      "Annual\n (N = 156)",
      "Perennial\n (N = 161)"
    ),
    guide = guide_legend(ncol = 3)
  ) +
  
  scale_point_size_continuous(guide = NULL) +
  scale_slab_colour_discrete(guide = NULL) +
  
  geom_vline(xintercept = 0, alpha = 0.5) +
  geom_hline(yintercept = 2.5, alpha = 0.25) +
  geom_hline(yintercept = 4.5, alpha = 0.25) +
  geom_hline(yintercept = 6.5, alpha = 0.25) +
    
  theme_bw() +
    
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    legend.key.size = unit(x = 1, units = "cm"),
    legend.box.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = TextSetup,
    axis.text.y = TextSetup,
    axis.title.y = TextSetup,
    axis.title.x = TextSetup,
    legend.text = TextSetup,
    legend.title = TextSetup
  ) +
  coord_cartesian(xlim = c(-40, 80))


# ***************************************************************************
#### Section 3: Export Plot ####
# ***************************************************************************


## Export and save in the right location
ggsave(
  Figure,
  device = "png",
  filename = here("Outputs", "Plots", "Meadows_Figure3_SimulatedWTP_Weighted.png"),
  width = 25,
  height = 20,  # Increased from 15 to 20 cm
  units = "cm",
  dpi = 500
)


# End Of Script -----------------------------------------------------------