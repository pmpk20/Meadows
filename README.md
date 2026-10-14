# Understanding Public Preferences for Urban Wildflower Meadows: supporting dataset

<!-- TODO: replace the Zenodo DOI placeholders once the first release is archived -->
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.XXXXXXX.svg)](https://doi.org/10.5281/zenodo.XXXXXXX)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![ORCID](https://img.shields.io/badge/ORCID-0000--0001--8550--466X-A6CE39?logo=orcid)](https://orcid.org/0000-0001-8550-466X)
[![GitHub release](https://img.shields.io/github/v/release/pmpk20/Meadows)](https://github.com/pmpk20/Meadows/releases)
------------------------------------------------------------------------


#### About:
- Repo Author: Dr Peter King (University of Leeds). Correspondence: p.king1\@leeds.ac.uk
- Co-authors: Tristan J. Pett (University of Kent), Mark A. Goddard (Northumbria University), Thomas H. Lundhede (University of Copenhagen), Gail E. Austen (University of Kent), Zoe G. Davies (University of Kent), Martin Dallimer (Imperial College London)
- Status: Accepted in ***Journal of Environmental Management*** (special issue: Landscape Perception) --- [https://doi.org/10.1016/j.jenvman.2026.131156](https://doi.org/10.1016/j.jenvman.2026.131156)
- Keywords: Aesthetic value, Choice experiments, Hypothetical bias, Nature-based solutions, Urban meadows, Willingness-to-pay.
- Last Change: 14/10/2026

#### Acknowledgements:
-   This research was partly funded by the European Research Council (ERC) under the European Union's Horizon 2020 Research and Innovation Programme (Consolidator Grant No. 726104) and by the Natural Environment Research Council (NE/V006916/1).
-   The meadow creation was supported by the UK Insect Pollinators Initiative, funded by BBSRC, Defra, NERC, the Scottish Government and the Wellcome Trust under the auspices of the Living with Environmental Change partnership (grant BB/I00047X/1). TJP was funded via a Swire Foundation PhD Scholarship.
-   Models were estimated on the High-Performance Computing systems provided by Information Services at the University of Kent and on the Aire HPC system at the University of Leeds, UK.
-   Using R and [Apollo](https://apollochoicemodelling.com/) (Hess and Palma, 2019).
-   The data were collected for Tristan Pett's PhD at the University of Kent ([Pett, 2018](https://kar.kent.ac.uk/74578/)), with ethical approval from the University of Kent.

#### Description:
-   `/Data/` contains the anonymised survey data from in situ interviews with 563 urban greenspace users at 17 sites in 11 parks in Leeds, Bristol and Edinburgh (July--August 2014). `database_Meadows_Anonymised.csv` is the input to the pipeline; `database_Meadows_Anonymised_Weighted.csv` is the 474-respondent estimation sample with entropy-balancing weights. `Meadows_Codebook.csv` explains every variable and its coding (SI Table B.3).
-   The data contain no names, contact details, home locations, free-text answers, ethnicity or interview dates. Respondent IDs were randomly re-assigned, and the interviewer, group-composition and travel-time variables were removed because no analysis uses them.
-   `/ModelScripts/`: the four mixed logit models (WTP-space, correlated random parameters, 2,000 Sobol draws). `/ModelOutputs/` holds the estimated models, so you do not need to re-estimate them.
-   `/OtherScripts/`: entropy balancing, WTP simulation, and every table and figure. `/OtherScripts/Extras/` holds alternative figures not reported in the paper.
-   `/Outputs/`: all tables and figures in the manuscript and supplementary material.
-   `Meadows_FullSurveyPrintUpdated.pdf` is the full questionnaire (block 1), reproduced from Pett (2018, Appendix 2).
-   Run `00_Meadows_Replication.R` to execute all replication scripts in order. Scripts are numbered 01--25 by execution sequence.
-   Data are publicly available in this repository. Zenodo DOI: [https://doi.org/10.5281/zenodo.XXXXXXX](https://doi.org/10.5281/zenodo.XXXXXXX)

#### Requirements and quick start:
- R version 4.4 or later. The shipped outputs were reproduced exactly with R 4.5.0 and: apollo (0.3.9), WeightIt (2.0.0), cobalt (5.0.0), MatchIt (4.7.2), data.table (1.18.6.1), tidyverse (2.0.0), ggplot2 (4.0.3), ggdist (3.3.3), mded (0.1.2), mvtnorm (1.4.2), here (1.0.2), janitor (2.2.1), rstatix (1.1.0).
- The models were estimated with Apollo 0.3.4 (models 02, 03, 05; R 4.3.3) and Apollo 0.3.9 (model 04; R 4.4.3).
- Open the project folder in R and run:

```r
install.packages(c("apollo", "WeightIt", "cobalt", "MatchIt", "data.table", "tidyverse",
                   "here", "janitor", "rstatix", "mvtnorm", "mded", "coin", "ggdist",
                   "ggridges", "AMR", "matrixStats", "reshape2", "gridExtra",
                   "distributional", "sessioninfo"))
source("00_Meadows_Replication.R")
```

- The full run takes about three minutes because the estimated models are included. To re-estimate them, set `ESTIMATE_MODELS <- TRUE` in `00_Meadows_Replication.R`. Each model takes about an hour on HPC; lower `nCores` (default 40) for a laptop and expect several hours per model.
- Run `sessioninfo::session_info()` afterwards to compare your setup.

------------------------------------------------------------------------
#### Directory structure (replication files only):

```
Meadows/
├── 00_Meadows_Replication.R                                      # runs everything below in order
├── Data/
│   ├── database_Meadows_Anonymised.csv                           # [input] 563 respondents × 6 choice tasks
│   ├── database_Meadows_Anonymised_Weighted.csv                  # [output of 01] 474 respondents with EB weights
│   └── Meadows_Codebook.csv                                      # [output of 15] Table B.3
├── ModelScripts/
│   ├── 02_Meadows_Pooled_Weighted_ParkFEs_V1.R                   # main weighted model → Tables 1, 2, C.1
│   ├── 03_Meadows_Pooled_Unweighted_ParkFEs_V1.R                 # unweighted model → Tables 1, 2, C.1
│   ├── 04_Meadows_Pooled_Weighted_GroupedType_ParkFEs_V1.R       # pooled sown meadows, weighted → Table C.4
│   └── 05_Meadows_Pooled_Unweighted_GroupedType_ParkFEs_V1.R     # pooled sown meadows, unweighted → Table C.4
├── ModelOutputs/
│   ├── Meadows_Pooled_*_ParkFEs_V1_model.rds                     # [output of 02-05] fitted Apollo models
│   ├── Meadows_Pooled_*_ParkFEs_V1_estimates.csv                 # [output of 02-05] parameter estimates
│   └── Meadows_Pooled_Weighted_GroupedType_ParkFEs_V1_output.txt # [output of 04] full model output
├── OtherScripts/
│   ├── Balancing/
│   │   └── 01_Meadows_GenerateEBWeights.R                        # EB weights → Table B.5
│   ├── Tables/
│   │   ├── 06_Meadows_SimulateWTPs_Weighted.R                    # 10,000 WTP draws, weighted model
│   │   ├── 07_Meadows_SimulateWTPs_Unweighted.R                  # 10,000 WTP draws, unweighted model
│   │   ├── 08_Meadows_SimulateWTPs_GroupedType.R                 # 10,000 WTP draws, pooled models
│   │   ├── 09_Meadows_Table1_ModelSummaries.R                    # Table 1
│   │   ├── 10_Meadows_Table2_InteractionSummaries_Both.R         # Table 2
│   │   ├── 12_Meadows_NegativeWTPShare.R                         # share with negative WTP (Discussion)
│   │   ├── 13_Meadows_Table3_PoeTests_Weighted.R                 # Table 3
│   │   ├── 14_Meadows_TableB1_BottomRows.R                       # Table B.1 (bottom rows)
│   │   ├── 15_Meadows_TableB3_Codebook.R                         # Table B.3
│   │   ├── 16_Meadows_TableB4_Gtests.R                           # Table B.4
│   │   ├── 18_Meadows_TableB6_SampleByMeadowAndCity.R            # Table B.6
│   │   ├── 19_Meadows_TableC1_FullModelOutputs.R                 # Table C.1
│   │   ├── 20_Meadows_TableC2_SimulatedWTPMoments.R              # Table C.2
│   │   ├── 21_Meadows_TableC3_PoeTests_Unweighted.R              # Table C.3
│   │   ├── 22_Meadows_TableC4_FullModelOutputsGrouped.R          # Table C.4
│   │   └── 23_Meadows_TableC5_PoeTests_GroupedType.R             # Table C.5
│   ├── Figures/
│   │   ├── 11_Meadows_Figure3_SimulatedWTP_Weighted.R            # Figure 3
│   │   ├── 17_Meadows_FigureB2_SampleDistributionByMeadow.R      # Figure B.2
│   │   ├── 24_Meadows_FigureC1_ChoiceAllByMeadowCity.R           # Figure C.1
│   │   └── 25_Meadows_FigureC2_SimulatedWTP_GroupedType.R        # Figure C.2
│   └── Extras/                                                   # alternative figures, not run by 00
├── Outputs/
│   ├── Tables/                                                   # [outputs of 01, 09-23] all tables
│   └── Plots/                                                    # [outputs of 11, 17, 24, 25] all figures
├── Figure1_LocationOfCities.png                                  # Figure 1 (static image, not script-generated)
├── Figure2_ExampleChoiceCard.png                                 # Figure 2 (block 1, task 4 of the fielded design)
├── Meadows_FullSurveyPrintUpdated.pdf                            # full questionnaire
├── CITATION.cff
└── LICENSE
```

#### Licence:
- Code (`*.R`): MIT, see `LICENSE`.
- Data (`Data/`, `ModelOutputs/`): CC BY 4.0, <https://creativecommons.org/licenses/by/4.0/>.
- Survey document and choice-card images: reproduced from Pett (2018) for documentation only; images remain with their original rights holders.

#### Contact:
- Dr Peter King, Institute for Transport Studies, University of Leeds: p.king1\@leeds.ac.uk
