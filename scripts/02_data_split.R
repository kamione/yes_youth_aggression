# Environment ------------------------------------------------------------------
library(tidyverse)
library(here)
library(tidymodels)


# Data IO  ---------------------------------------------------------------------
psychopathology_list <- c("PHQ9_tot", "YMRS_tot", "HCL32_tot", "GAD7_tot",
                          "LSAS_Tot", "YBOCS_Sym", "Cape_distress_tot",
                          "PQB_tot")
preprocessed_df <-  here("data", "processed", 
                         "yes_baseline_outcome-aggression_n-2184_p-244.rds") %>% 
    read_rds()


# Split for Exploration and Holdout Datasets -----------------------------------
set.seed(1234)
splits <- initial_split(preprocessed_df, prop = 3/4)
discovery_df <- training(splits)
holdout_df <- testing(splits)

# get the first principal component as General Psychopathology
pca_res <- discovery_df %>% 
    select(psychopathology_list) %>% 
    princomp(cor = TRUE, scores = TRUE)
summary(pca_res)
discovery_df$g_psy <- pca_res %>% 
    pluck("scores") %>% 
    as_tibble() %>% 
    pull(Comp.1)
holdout_df$g_psy <- predict(pca_res, holdout_df) %>% 
    as_tibble() %>% 
    pull(Comp.1)

# scale BPAQ total
discovery_df$bpaq_scaled <- scale(discovery_df$bpaq_tot)
holdout_df$bpaq_scaled <- scale(
    holdout_df$bpaq_tot, 
    center = mean(discovery_df$bpaq_tot),
    scale = sd(discovery_df$bpaq_tot)
)

# make sure numeric
discovery_df$bpaq_scaled <- as.numeric(discovery_df$bpaq_scaled)
holdout_df$bpaq_scaled <- as.numeric(holdout_df$bpaq_scaled)

write_rds(discovery_df, here("data", "processed", "discovery_dataset.rds"))
write_rds(holdout_df, here("data", "processed", "holdout_dataset.rds"))

