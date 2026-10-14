#### Meadows: In-text - share of respondents with negative WTP ###############
# FUNCTION: Discussion text - "approximately a quarter of respondents had negative WTP" for
#   pollinator habitat quality. Under normally distributed WTP the share below zero is
#   Phi(-mu/sigma), using the mean and SD of each random parameter. In this model the
#   means are WTP at control sites (meadow-type interactions shift the mean at annual and
#   perennial meadows).


library(here)
library(magrittr)
library(data.table)


Models <- list(
  Weighted   = here("ModelOutputs", "Meadows_Pooled_Weighted_ParkFEs_V1_model.rds") %>% readRDS(),
  Unweighted = here("ModelOutputs", "Meadows_Pooled_Unweighted_ParkFEs_V1_model.rds") %>% readRDS()
)

Levels <- c("PollinatorQualityMedium", "PollinatorQualityHigh")

NegativeShare <- rbindlist(lapply(names(Models), function(m) {
  est <- Models[[m]]$estimate
  rbindlist(lapply(Levels, function(l) {
    mu    <- est[[paste0("mu_", l)]]
    sigma <- abs(est[[paste0("sig_", l)]])
    data.table(Model = m, Level = l, Mean = round(mu, 3), SD = round(sigma, 3),
               ShareNegative = round(pnorm(-mu / sigma), 3))
  }))
}))

print(NegativeShare)

NegativeShare %>%
  fwrite(here("Outputs", "Tables", "Meadows_InText_NegativeWTPShare.txt"))
