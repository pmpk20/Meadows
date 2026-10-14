#### Meadows: variable codebook (Table B.3) ###############
# Function: One row per column in the main analysis dataset, giving its class,
#   observed values/range, and a plain-language description. Written to the Data
#   folder as a general-purpose data dictionary (also corresponds to the "Table B3:
#   Codebook" placeholder listed in the master script's Section Five comments).
#   Descriptions come from three passes: (a) data-driven inspection (crosstabs used to confirm
#   which columns are recodes/dummies of which other columns), (b) a manual visual
#   cross-check against the printed survey (Meadows_FullSurveyPrintUpdated.pdf - this
#   PDF is a scan with no text layer, so it cannot be parsed programmatically; it was
#   read page-by-page instead), and (c) the underlying PhD thesis methodology chapter
#   (Pett, 2018, "Biodiversity and human well-being", <https://kar.kent.ac.uk/74578/>,
#   Chapter 4), which has a normal text layer and describes the biodiversity-survey
#   and questionnaire design behind several otherwise-unexplained columns, plus
#   direct clarification from the study team where noted. Rows still marked CONFIRM
#   in the Status column are the ones no pass could resolve with confidence.
# Author: Dr Peter King (p.king1@leeds.ac.uk)
# Last Edited: 03/07/2026
# Session info: see README.md ("Replication Information")


# ****************************************************************
#### Section 1: Setup ####
# ****************************************************************

library(magrittr)
library(dplyr)
library(data.table)
library(here)


# ****************************************************************
#### Section 2: Import data ####
# ****************************************************************

Data <-
  here("Data", "database_Meadows_Anonymised_Weighted.csv") %>%
  fread() %>%
  data.frame()


# ****************************************************************
#### Section 3: Curated variable descriptions ####
## Hand-written, not derived from the data at runtime, so that a future change
## to the data (e.g. dropping a column) is caught as a mismatch below rather
## than silently producing a stale description.
# ****************************************************************

Descriptions <- tibble::tribble(
  ~Variable, ~Description, ~Status,

  # --- Row identifiers / design ------------------------------------------------
  "V1", "Row-index artifact from an earlier export/merge step; study team confirms no deeper meaning.", "",
  "X", "Second row-index artifact, likely from a later rbind/rownames-to-column step; study team confirms no deeper meaning.", "",
  "ID", "Respondent ID, randomly re-assigned (1-563) for the public release so it cannot be linked to the paper questionnaires; row order is unchanged. Used as the individual identifier (indivID) in the Apollo models; the weighted file has 474 unique values (the final analytic N).", "",
  "card", "Choice-set/card ID (1-12) from the full 12-choice-set design; not always equal to Task, since each respondent only sees 6 of the 12 cards (survey confirms a two-block design; this print copy is block one).", "",
  "Task", "Sequential choice task number (1-6) for this respondent.", "",
  "alt", "Which alternative (1=A, 2=B, 3=C, 4=D/status-quo) this row represents in the long-format choice data used for modelling.", "",
  "choice", "The alternative (1-4) actually chosen for this task.", "",

  # --- Choice-task attributes, alternatives A-D --------------------------------
  "choice1", "Wide-format indicator: was alternative A chosen (1) for this task? Retained from the original wide file; superseded by `choice`/`alt` in modelling.", "",
  "choice2", "As choice1, for alternative B.", "",
  "choice3", "As choice1, for alternative C.", "",
  "choice4", "As choice1, for alternative D (status quo).", "",
  "costA", "Cost/tax-increase level (GBP) shown for alternative A: 0, 5, 10, 20, 40, or 60.", "",
  "costB", "Cost level for alternative B (same level set as costA).", "",
  "costC", "Cost level for alternative C.", "",
  "costD", "Cost for alternative D (status quo); always 0.", "",
  "sq1", "Status-quo flag for alternative A; always 0 (A is never the opt-out).", "",
  "sq2", "Status-quo flag for alternative B; always 0.", "",
  "sq3", "Status-quo flag for alternative C; always 0.", "",
  "sq4", "Status-quo flag for alternative D; always 1 (D is the status-quo/opt-out option).", "",
  "av1", "Availability flag for alternative A; always 1 (always available).", "",
  "av2", "Availability flag for alternative B; always 1.", "",
  "av3", "Availability flag for alternative C; always 1.", "",
  "av4", "Availability flag for alternative D; always 1.", "",
  "asc1", "Alternative-specific-constant dummy for A; always 1.", "",
  "asc2", "Alternative-specific-constant dummy for B; always 1.", "",
  "asc3", "Alternative-specific-constant dummy for C; always 1.", "",
  "asc4", "Alternative-specific-constant dummy for D; always 0 (D/status-quo is the base level).", "",
  "Plants_5_A", "1 if alternative A shows the 'five plant species' level, else 0.", "",
  "Plants_10_A", "1 if alternative A shows the 'ten plant species' level, else 0.", "",
  "Native_Decrease_A", "1 if alternative A shows a decrease in native species, else 0.", "",
  "Native_Increase_A", "1 if alternative A shows an increase in native species, else 0.", "",
  "PollinatorQuality_Medium_A", "1 if alternative A shows medium pollinator-habitat quality, else 0.", "",
  "PollinatorQuality_High_A", "1 if alternative A shows high pollinator-habitat quality, else 0.", "",
  "Appearance_W_A", "1 if alternative A shows the white-only flower-colour level, else 0.", "",
  "Appearance_WYB_A", "1 if alternative A shows the white/yellow/blue flower-colour level, else 0.", "",
  "Appearance_WYBPR_A", "1 if alternative A shows the white/yellow/blue/pink/red flower-colour level, else 0.", "",
  "Plants_5_B", "As Plants_5_A, for alternative B.", "",
  "Plants_10_B", "As Plants_10_A, for alternative B.", "",
  "Native_Decrease_B", "As Native_Decrease_A, for alternative B.", "",
  "Native_Increase_B", "As Native_Increase_A, for alternative B.", "",
  "PollinatorQuality_Medium_B", "As PollinatorQuality_Medium_A, for alternative B.", "",
  "PollinatorQuality_High_B", "As PollinatorQuality_High_A, for alternative B.", "",
  "Appearance_W_B", "As Appearance_W_A, for alternative B.", "",
  "Appearance_WYB_B", "As Appearance_WYB_A, for alternative B.", "",
  "Appearance_WYBPR_B", "As Appearance_WYBPR_A, for alternative B.", "",
  "Plants_5_C", "As Plants_5_A, for alternative C.", "",
  "Plants_10_C", "As Plants_10_A, for alternative C.", "",
  "Native_Decrease_C", "As Native_Decrease_A, for alternative C.", "",
  "Native_Increase_C", "As Native_Increase_A, for alternative C.", "",
  "PollinatorQuality_Medium_C", "As PollinatorQuality_Medium_A, for alternative C.", "",
  "PollinatorQuality_High_C", "As PollinatorQuality_High_A, for alternative C.", "",
  "Appearance_W_C", "As Appearance_W_A, for alternative C.", "",
  "Appearance_WYB_C", "As Appearance_WYB_A, for alternative C.", "",
  "Appearance_WYBPR_C", "As Appearance_WYBPR_A, for alternative C.", "",
  "Plants_5_D", "Attribute dummy for alternative D; always 0 (status quo has none of these attributes).", "",
  "Plants_10_D", "As above; always 0.", "",
  "Native_Decrease_D", "As above; always 0.", "",
  "Native_Increase_D", "As above; always 0.", "",
  "PollinatorQuality_Medium_D", "As above; always 0.", "",
  "PollinatorQuality_High_D", "As above; always 0.", "",
  "Appearance_W_D", "As above; always 0.", "",
  "Appearance_WYB_D", "As above; always 0.", "",
  "Appearance_WYBPR_D", "As above; always 0.", "",

  # --- Park-visit context -------------------------------------------------------
  "freqv", "Survey Q1, 'How frequently do you come to this park?' - coded in questionnaire order: 1 = 6 or 7 days a week, 2 = 2 to 5 days a week, 3 = 1 day a week, 4 = 1 to 3 days a month, 5 = less than one day a month (so higher = less frequent).", "",
  "FrequencyOfVisit", "Duplicate of freqv (identical crosstab), used interchangeably downstream.", "",
  "timev", "Survey Q4, 'How long have you been in this park today?' (minutes).", "",
  "visittime", "Exact duplicate of timev.", "",

  # --- Perception / knowledge battery -------------------------------------------
  "PlantGuess", "Survey Q9, 'About how many different species of flowering plants would you say are here?' - 5-point banded guess (values are band floors: <5, 6-10, 11-20, 21-30, >30).", "",
  "plrich", "Actual number of flowering-plant species recorded at the site via the source study's 1m x 1m quadrat transect surveys (Pett 2018 thesis, Sec 4.3.3); range 1-31 matches the thesis's reported site range exactly. Not the respondent's guess (see PlantGuess).", "",
  "NativeGuess", "Survey Q10, 'About how many of the species of plants in this area do you think are native to the UK?' - 5-point banded guess (0/25/50/75/100 = none/quarter/half/three-quarters/all).", "",
  "natper", "Actual percentage of native flowering species at the site, classified via the Online Atlas of the British & Irish Flora from the same quadrat surveys as plrich (Pett 2018 thesis, Sec 4.3.3). Not the respondent's guess (see NativeGuess).", "",
  "ColourGuess", "Survey Q11, 'About how colourful would you say this area is?' - 5-point scale, 1 = very few colours to 5 = very many colours.", "",
  "PollinGuess", "Survey Q12, 'Do you think this area provides useful resources for pollinating insects?' - 5-point scale, 1 = poor/not useful to 5 = excellent/very useful.", "",

  # --- Demographics --------------------------------------------------------------
  "city", "Survey city: 1 = Leeds, 2 = Bristol, 3 = Edinburgh.", "",
  "Income", "Survey question 'What is your total household income (before tax)?', coded to each band's approximate midpoint (GBP). Bands: up to £5,199; £5,200 to £10,399; £10,400 to £15,599; £15,600 to £20,799; £20,800 to £25,999; £26,000 to £31,199; £31,200 to £36,399; £36,400 to £51,999; £52,000 and above.", "",
  "IncomeDummy", "Median-split binary version of Income (0 for the four lowest bands, 1 for the five highest); not used by the current EB-weighting formula, which uses continuous Income instead. The model scripts (02-05) recompute it before estimation as 1 = income of £26,000 or more, 0 = below £26,000 (474-respondent sample: 272 / 202).", "",
  "type", "Duplicate of Type, retained from the source file (identical crosstab).", "",
  "Type", "Meadow type at the park where the respondent was surveyed: 1 = Control, 2 = Perennial, 3 = Annual.", "",
  "Control", "1 if Type == Control, else 0.", "",
  "Perennial", "1 if Type == Perennial, else 0.", "",
  "Annual", "1 if Type == Annual, else 0.", "",
  "TypeAnnual", "Duplicate of Annual.", "",
  "TypePerennial", "Duplicate of Perennial.", "",
  "Gender", "Survey question 'What is your gender?' with tickboxes Male / Female. Resolved: filtering to Task == 1 and cross-tabulating Gender x city reproduces the same cell counts as the thesis's sample-composition table (Pett 2018, Table S5.7.2), confirming 0 = Female, 1 = Male (Leeds 121 F / 95 M; Bristol 77 / 75; Edinburgh 107 / 88).", "",
  "age", "Ordinal age-band code (1-6); AgeLevel gives the corresponding band floor in years.", "",
  "AgeLevel", "Age-band floor in years; six bands 18-24, 25-34, 35-44, 45-54, 55-64, 65+ (Pett 2018 thesis, p.88), one-to-one with age.", "",
  "AgeDummy", "Median-split binary version of age/AgeLevel (0 for the three youngest bands, i.e. under 45; 1 for the three oldest, 45 and over); used as an EB-weighting covariate and, recomputed identically, in the models (474-respondent sample: 249 / 225).", "",
  "Education", "Highest qualification, 6 categories from 'no qualifications' to 'degree/higher degree/NVQ4-5/professional qualifications'.", "",
  "Employment", "Employment status, 5 categories: employed / unemployed and looking / not working (e.g. full-time parent) / retired / in full-time education.", "",
  "CNS_ScaleMean", "Mean score on the (state-version) Connectedness to Nature Scale (Mayer et al. 2009), adapted by the source study: 7-point scale, 1 = strongly disagree to 7 = strongly agree, averaged across scale items (Pett 2018 thesis, Ch.4). -999 is the missing/not-completed flag.", "",
  "CNS_ScaleMean_MeanUpdated", "Same as CNS_ScaleMean, but without the -999 flag. Not constructed by any script in this repo (present already in the raw Anonymised.csv - data received pre-cleaned, see README). Study team clarifies the pipeline's general convention for -999 is listwise deletion, not value replacement (e.g. script 01 drops rows with missing AgeLevel/non-positive Income/Education rather than imputing them) - so despite the '_MeanUpdated' name this is unlikely to be a mean-imputed version of CNS_ScaleMean; more likely a corrected re-scoring done upstream by the original depositor.", "",
  "Charity", "Survey Q15, 'In the last five years have you been a member of any wildlife conservation or natural heritage organisation (e.g. RSPB, Wildlife Trust, National Trust)?' - Yes/No.", "",

  # --- Park / city identifiers -----------------------------------------------------
  "park...34", "Composite park+meadow-type label (e.g. 'Burley_Control'); column name has a make.names() '...34' suffix because the source data had both a park *name* variable and a park *number* variable (1-16) carrying the same information, which collided on import. Note: 'Perrenial' misspelling is preserved verbatim from the source data.", "",
  "Clean_Park_ID", "Standardised park identifier (11 parks), derived from park...34 with the meadow-type suffix stripped.", "",
  "EC_Park_Burley", "Effects-coded (deviation) dummy for Burley park; Stanhope_Rec is the omitted reference park (-1 across all EC_Park_* dummies).", "",
  "EC_Park_Gores_Marsh", "Effects-coded dummy for Gores Marsh park.", "",
  "EC_Park_Horfield_Common", "Effects-coded dummy for Horfield Common park.", "",
  "EC_Park_Inch_Park", "Effects-coded dummy for Inch Park.", "",
  "EC_Park_King_Lane", "Effects-coded dummy for King Lane park.", "",
  "EC_Park_Middleton", "Effects-coded dummy for Middleton park.", "",
  "EC_Park_Pilrig", "Effects-coded dummy for Pilrig park.", "",
  "EC_Park_Rodley", "Effects-coded dummy for Rodley park.", "",
  "EC_Park_St_Andrews", "Effects-coded dummy for St Andrews park.", "",
  "EC_Park_St_Marks", "Effects-coded dummy for St Marks park.", "",
  "City_1_Effect", "Effects-coded dummy: 1 = Leeds, -1 = Edinburgh (omitted reference), 0 = Bristol.", "",
  "City_2_Effect", "Effects-coded dummy: 1 = Bristol, -1 = Edinburgh (omitted reference), 0 = Leeds.", "",
  "City_1_Leeds", "Simple dummy: 1 if city == Leeds, else 0.", "",
  "City_2_Bristol", "Simple dummy: 1 if city == Bristol, else 0.", "",

  # --- Entropy-balancing outputs (see OtherScripts/Balancing/01_Meadows_GenerateEBWeights.R) --
  "Treatment", "Ternary treatment code used for EB weighting: 0 = Control, 1 = Annual, 2 = Perennial. NOTE: this numeric order differs from Type (1 = Control, 2 = Perennial, 3 = Annual) - same three groups, different code order.", "",
  "Treatment_Binary", "Binary version of Treatment: 0 = Control, 1 = Other (Annual+Perennial pooled), used for the Control-vs-Other robustness EB weights.", "",
  "always_sq", "1 if the respondent chose the status-quo alternative (alt == 4) in every one of their 6 tasks, else 0. Not currently used downstream.", "",
  "Weights", "Entropy-balancing weight from the main ternary (Control/Annual/Perennial) fit.", "",
  "Weights_Binary", "Entropy-balancing weight from the binary (Control-vs-Other) robustness fit.", ""
)


# ****************************************************************
#### Section 4: Compute observed class/values per column ####
# ****************************************************************

summarise_column <- function(x) {
  n_unique <- length(unique(x))
  if (is.numeric(x) && n_unique <= 12) {
    values <- paste(sort(unique(x)), collapse = ", ")
  } else if (is.numeric(x)) {
    values <- paste0(round(min(x, na.rm = TRUE), 2), " to ", round(max(x, na.rm = TRUE), 2))
  } else {
    values <- paste(head(sort(unique(x)), 12), collapse = ", ")
  }
  tibble::tibble(Class = class(x)[1], N_Unique = n_unique, Values_Or_Range = values)
}

Observed <- purrr::map_dfr(Data, summarise_column) %>%
  mutate(Variable = names(Data), .before = 1)


# ****************************************************************
#### Section 5: Join and flag any schema drift ####
## If a column exists in the data but not in Descriptions (or vice versa), that
## means the dataset has changed since this codebook was written - flag it rather
## than silently dropping/inventing a row.
# ****************************************************************

Codebook <- Observed %>%
  full_join(Descriptions, by = "Variable") %>%
  mutate(
    Description = ifelse(is.na(Description), "NOT IN CODEBOOK - new/renamed column", Description),
    Status = ifelse(is.na(Class), "CONFIRM - column in codebook but not found in current data", Status),
    Status = ifelse(is.na(Description) | Description == "NOT IN CODEBOOK - new/renamed column",
                     "CONFIRM - undocumented column", Status)
  ) %>%
  select(Variable, Class, N_Unique, Values_Or_Range, Description, Status)

n_confirm <- sum(Codebook$Status != "" & !is.na(Codebook$Status))
message("Codebook: ", nrow(Codebook), " variables, ", n_confirm, " flagged CONFIRM.")


# ****************************************************************
#### Section 6: Export ####
# ****************************************************************

## quote = "auto" (fwrite's default) since Description text contains commas
Codebook %>%
  fwrite(paste0(here(
    "Data",
    "Meadows_Codebook.csv"
  )))


#### End of script
# ****************************************************************
