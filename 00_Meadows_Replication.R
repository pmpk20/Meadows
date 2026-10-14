#### Meadows: Master replication script  ###############
# Function: Sources every script needed to reproduce the manuscript and SI
#   tables/figures, in the order the results appear in the paper.
# Author: Dr Peter King (p.king1@leeds.ac.uk)
# Last Edited: 19/02/2025
# Session info: see README.md ("Replication Information")


## Libraries here: -----------------------------------------------------------------
## Setting up all relevant libraries
library(data.table)
library(magrittr)
library(dplyr)
library(tidyverse)
library(here)
library(janitor)
library(apollo)
library(ggplot2)
library(rstatix)
library(stats)
library(mvtnorm)
library(MatchIt)
library(WeightIt)
library(cobalt)
library(sessioninfo)


# **********************************************************************************
#### Section One: Data Setup ####
## Survey data received pre-cleaned; no data-cleaning scripts exist.
## Estimates entropy balancing weights → outputs 'database_Meadows_Anonymised_Weighted.csv'
## Also produces balancing diagnostics used in Table B.4 and Table B.5.
# **********************************************************************************

here("OtherScripts/Balancing", "01_Meadows_GenerateEBWeights.R") %>% source()


# **********************************************************************************
#### Section Two: Modelling ####
## Estimates four correlated MXL models. Script 02 (weighted, main) is the only one
## strictly required for key results; 03-05 are robustness checks.
##   02 → main weighted model     → Tables 1, 2; Table C.1; Figure 3; Table 3
##   03 → unweighted model        → Tables 1, 2; Table C.1; Table C.3
##   04 → pooled type, weighted   → Tables C.4, C.5; Figure C.2
##   05 → pooled type, unweighted → Table C.4
# **********************************************************************************

## The estimated models are shipped in ModelOutputs/, so estimation is off by default.
## Set ESTIMATE_MODELS <- TRUE to re-estimate (hours; overwrites the shipped models).
ESTIMATE_MODELS <- FALSE
if (ESTIMATE_MODELS) {
here("ModelScripts", "02_Meadows_Pooled_Weighted_ParkFEs_V1.R") %>% source()
here("ModelScripts", "03_Meadows_Pooled_Unweighted_ParkFEs_V1.R") %>% source()

## Superseded model scripts kept for reference:
# here("ModelScripts", "XX_Meadows_AllTypes_Weighted_FewerInteractions_V2.R") %>% source()
# here("ModelScripts", "XX_Meadows_AllTypes_Unweighted_FewerInteractions_V2.R") %>% source()

here("ModelScripts", "04_Meadows_Pooled_Weighted_GroupedType_ParkFEs_V1.R") %>% source()
here("ModelScripts", "05_Meadows_Pooled_Unweighted_GroupedType_ParkFEs_V1.R") %>% source()
}

## Superseded model scripts kept for reference:
# here("ModelScripts", "04_Meadows_AllTypes_Weighted_FewerInteractions_GroupedType_V1.R") %>% source()
# here("ModelScripts", "05_Meadows_AllTypes_Unweighted_FewerInteractions_GroupedType_V1.R") %>% source()


# **********************************************************************************
#### Section Three: WTP Simulation ####
## Generates 10,000 WTP draws used by Poe test and figure scripts below.
## These produce intermediate objects only — not manuscript outputs directly.
##   06 → weighted draws (main model)
##   07 → unweighted draws
##   08 → pooled type draws
# **********************************************************************************

here("OtherScripts/Tables", "06_Meadows_SimulateWTPs_Weighted.R") %>% source()
here("OtherScripts/Tables", "07_Meadows_SimulateWTPs_Unweighted.R") %>% source()
here("OtherScripts/Tables", "08_Meadows_SimulateWTPs_GroupedType.R") %>% source()


# **********************************************************************************
#### Section Four: Main Manuscript Outputs ####
# **********************************************************************************

## Table 1: Model parameter summary — weighted and unweighted correlated MXL
here("OtherScripts/Tables", "09_Meadows_Table1_ModelSummaries.R") %>% source()

## Table 2: Interaction terms — meadow type × attribute level (weighted and unweighted)
here("OtherScripts/Tables", "10_Meadows_Table2_InteractionSummaries_Both.R") %>% source()

## Figure 3: Simulated WTP distributions by attribute and meadow type (weighted)
here("OtherScripts/Figures", "11_Meadows_Figure3_SimulatedWTP_Weighted.R") %>% source()

## In-text: share of respondents with negative WTP for pollinator quality, Phi(-mu/sigma)
here("OtherScripts/Tables", "12_Meadows_NegativeWTPShare.R") %>% source()

## Table 3: Poe tests — pairwise WTP differences by meadow type (weighted)
here("OtherScripts/Tables", "13_Meadows_Table3_PoeTests_Weighted.R") %>% source()


# **********************************************************************************
#### Section Five: Supplementary Material — Appendix B ####
# **********************************************************************************

## Table B1: Details per city
## Table B2: Details for CE attributbes
## Table B3: Codebook
## Table B4: Details per type
## Table B5: Balancing details
## Table B6: Details per park

## Table B.1: City demographics and park names (bottom rows)
here("OtherScripts/Tables", "14_Meadows_TableB1_BottomRows.R") %>% source()

## Table B.3: Variable codebook (written to Data/Meadows_Codebook.csv)
here("OtherScripts/Tables", "15_Meadows_TableB3_Codebook.R") %>% source()

## Table B.4: Sample counts by meadow type × sociodemographic with G-tests
here("OtherScripts/Tables", "16_Meadows_TableB4_Gtests.R") %>% source()

## Figure B.2: Balancing variable distributions by meadow type
here("OtherScripts/Figures", "17_Meadows_FigureB2_SampleDistributionByMeadow.R") %>% source()
# here("OtherScripts/Figures", "XX_Meadows_FigureC2_SampleDistributionByPark.R") %>% source()

## Table B.6: Respondent counts per meadow type × park × city
here("OtherScripts/Tables", "18_Meadows_TableB6_SampleByMeadowAndCity.R") %>% source()
# here("OtherScripts/Tables", "XX_Meadows_TableC1_Truncated_SampleByMeadowAndCity.R") %>% source()


# **********************************************************************************
#### Section Six: Supplementary Material — Appendix C ####
# **********************************************************************************

## Table C.1: Full model output — all parameters, weighted and unweighted correlated MXL
here("OtherScripts/Tables", "19_Meadows_TableC1_FullModelOutputs.R") %>% source()

## Table C.2: Simulated WTP summary statistics (median, mean, SD, 95% CI) by meadow type
here("OtherScripts/Tables", "20_Meadows_TableC2_SimulatedWTPMoments.R") %>% source()

## Table C.3: Poe tests — unweighted model (robustness check for Table 3)
here("OtherScripts/Tables", "21_Meadows_TableC3_PoeTests_Unweighted.R") %>% source()

## Table C.4: Full model output — pooled annual + perennial (weighted and unweighted)
here("OtherScripts/Tables", "22_Meadows_TableC4_FullModelOutputsGrouped.R") %>% source()

## Table C.5: Poe tests — pooled type model
here("OtherScripts/Tables", "23_Meadows_TableC5_PoeTests_GroupedType.R") %>% source()

## Figure C.1: Choice frequencies by alternative, meadow type, and city
here("OtherScripts/Figures", "24_Meadows_FigureC1_ChoiceAllByMeadowCity.R") %>% source()

## Figure C.2: Simulated WTP distributions for pooled type model
here("OtherScripts/Figures", "25_Meadows_FigureC2_SimulatedWTP_GroupedType.R") %>% source()


# End Of Script ##############################################################