#### Meadows: Figure C.2 - simulated WTP boxplot (grouped type) ###############
# AUTHOR: Peter King (p.king1@leeds.ac.uk)
# LAST CHANGE: 03/09/2025
# FUNCTION: Figure C.2 - box-and-whisker plot of simulated mean WTP per attribute,
#   Control vs pooled Annual+Perennial "grouped type", using WTP simulated in
#   script 08 (from the weighted grouped-type model, script 04).

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
  "SimulatedMeans_GroupedType.csv") %>% 
  fread() %>% 
  data.frame()


# *************************************************************
#### Section 2: Setup mapping  ####
# *************************************************************


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


TextSetup <- element_text(size = 14,
                          colour = "black",
                          family = "serif")

# custom_colors <- c(
#   RColorBrewer::brewer.pal(n = 3, name = "Blues"),
#   RColorBrewer::brewer.pal(n = 3, name = "Reds"),
#   RColorBrewer::brewer.pal(n = 3, name = "Greens"))


# custom_colors <- c(
#   RColorBrewer::brewer.pal(n = 9, name = "Blues")[c(9, 8, 2)])

custom_colors <- c(
  "darkblue",       # Annual or Perennial
  "powderblue",      # Control
  "dodgerblue"      # Annual
)

# *************************************************************
#### Section 2: Plot  ####
# *************************************************************


FigureC3 <- Data %>%
  
  pivot_longer(cols = 1:9) %>%
  
  dplyr::mutate(name = factor(name, levels = Labels_Tidied)) %>%
  
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
  
  geom_vline(xintercept = 0, alpha = 0.5) +
  geom_hline(yintercept = 2.5, alpha = 0.25) +
  geom_hline(yintercept = 4.5, alpha = 0.25) +
  geom_hline(yintercept = 6.5, alpha = 0.25) +
  
  theme_bw() +
  
  facet_grid( ~ Model) +
  
  scale_fill_manual(
    name = "Site:",
    values = c(
      "Treated" = "darkblue",
      "Control" = "powderblue"
    ),
    breaks = c("Control", "Treated"),
    labels = c(
      "Control\n (N = 157)\n",
      "Annual or Perennial\n (N = 317)\n"
    ),
    guide = guide_legend(ncol = 2)
  ) +
  
  scale_point_size_continuous(guide = NULL) +
  scale_slab_colour_discrete(guide = NULL) +
  
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = TextSetup,
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
  coord_cartesian(xlim = c(-45, 80)) ## Slightly expanding from 40->45



# ***************************************************************************
#### Section 3: Export Plot ####
# ***************************************************************************


## Export and save in the right location
ggsave(
  FigureC3,
  device = "png",
  filename = here("Outputs", "Plots", "Meadows_FigureC2_SimulatedWTP_GroupedType.png"),
  width = 25,
  height = 25,
  units = "cm",
  dpi = 500
)

# End Of Script -----------------------------------------------------------