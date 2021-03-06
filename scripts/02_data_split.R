# Environment ------------------------------------------------------------------
library(tidyverse)
library(here)
library(tidymodels)


# Data IO  ---------------------------------------------------------------------
preprocessed_df <-  here("data", "processed", 
                         "yes_baseline_outcome-aggression_n-2186_p-289.rds") %>% 
    read_rds()


# Split for Exploration and Holdout Datasets -----------------------------------
set.seed(1234)
splits <- initial_split(preprocessed_df, prop = 3/4, stata = c(bpaq_tot, g_psy))
discovery_df <- training(splits)
holdout_df <- testing(splits)

write_rds(discovery_df, here("data", "processed", "discovery_dataset.rds"))
write_rds(holdout_df , here("data", "processed", "holdout_dataset.rds"))



