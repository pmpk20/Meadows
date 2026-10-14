#### Meadows: Simulated WTP boxplot (unweighted, not in MS/SI) ###############
# AUTHOR: Peter King (p.king1@leeds.ac.uk)
# LAST CHANGE: 24/09/2026
# FUNCTION: Robustness-only boxplot of simulated WTP from the unweighted main
#   model (script 07). Not a manuscript/SI figure - kept for the analyst's own
#   comparison against Figure 3 (script 11); output archived under Outputs/Old/.

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
  "SimulatedMeans_AllTypes_Unweighted.csv") %>% 
  fread() %>% 
  data.frame()



# *************************************************************
#### Section 2: Summarise data  ####
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


## Plotting order of meadow types within each attribute level
MeadowOrder <- c("Control", "Annual", "Perennial")


TextSetup <- element_text(size = 14,
                          colour = "black",
                          family = "serif")

# custom_colors <- c(
#   RColorBrewer::brewer.pal(n = 9, name = "Blues")[c(9, 8, 2)])

custom_colors <- c(
  "darkblue",      # Distinctly medium-toned blue
  "dodgerblue",        # Lighter but clearly visible
  "powderblue"      # As requested
)

# *************************************************************
#### Section 3: Summarise data  ####
# *************************************************************

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



TableCX_WTPSummmary %>% 
  data.frame() %>% 
  fwrite(
    sep = ",",
    here("ModelOutputs", "TableCX_WTPSummmary_Unweighted.txt")
  )


# *************************************************************
#### Section 4: Plot  ####
# *************************************************************


Figure <- Data %>%
  pivot_longer(cols = 1:9) %>%
  ## Control first in the dodge so it sits at the edge of each group, matching the legend
  dplyr::mutate(name = factor(name, levels = Labels_Tidied),
                meadow_type = factor(meadow_type, levels = MeadowOrder)) %>%
  ggplot(aes(
    y = name %>% rev(),
    x = value,
    fill = meadow_type
  )) +
  stat_boxplot(geom = "errorbar",
               width = 0.65,
               position = position_dodge(width = 0.75)) +
  
  geom_boxplot(outlier.shape = NA, notch = TRUE) +
  
  scale_x_continuous(name = "Marginal WTP (GBP) in local council tax, per household, per annum",
                     breaks = seq(-100, 100, 20)) +
  
  scale_y_discrete(name = "Attribute and level", labels = Labels %>% rev()) +
  
  geom_vline(xintercept = 0, alpha = 0.5) +
  geom_hline(yintercept = 2.5, alpha = 0.25) +
  geom_hline(yintercept = 4.5, alpha = 0.25) +
  geom_hline(yintercept = 6.5, alpha = 0.25) +
  
  theme_bw() +
  
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
#### Section 5: Export Plot ####
# ***************************************************************************


## Export and save in the right location
## Outputs/Old/ is not tracked, so create it before saving this robustness figure
dir.create(here("Outputs", "Old", "Plots"), recursive = TRUE, showWarnings = FALSE)

ggsave(
  Figure,
  device = "png",
  filename = here("Outputs", "Old", "Plots", "Meadows_Figure2_WTPDistributions_Unweighted.png"),
  width = 25,
  height = 20,
  units = "cm",
  dpi = 500
)

# End Of Script -----------------------------------------------------------