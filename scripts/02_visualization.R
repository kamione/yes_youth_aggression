# Environment ------------------------------------------------------------------
library(tidyverse)
library(haven)
library(here)
library(ggpubr)
library(glue)
library(ggthemes)

# Data I/O ---------------------------------------------------------------------
data <- here("data", "processed", "yes_baseline_outcome-aggression_n-2215_p-293.csv") %>% 
    read_csv(col_types = cols()) %>% 
    mutate(BPAQ_tot = rowSums(across(BPAQ_1:BPAQ_12)), .before = BPAQ_1) %>% 
    mutate(BPAQ_phy = rowSums(across(BPAQ_1:BPAQ_3)), .before = BPAQ_1) %>% 
    mutate(BPAQ_verb = rowSums(across(BPAQ_4:BPAQ_6)), .before = BPAQ_1) %>% 
    mutate(BPAQ_anger = rowSums(across(BPAQ_7:BPAQ_9)), .before = BPAQ_1) %>% 
    mutate(BPAQ_host = rowSums(across(BPAQ_10:BPAQ_12)), .before = BPAQ_1)

# Basic Visualization ----------------------------------------------------------
hist_fig <- data %>% 
    select(matches("BPAQ_[tpvah]")) %>% 
    pivot_longer(everything(), names_to = "type", values_to = "value") %>% 
    mutate(type = factor(
        type, 
        levels = c("BPAQ_tot", "BPAQ_phy", "BPAQ_verb", "BPAQ_anger", "BPAQ_host"),
        labels = c("Total", "Physical Aggression", "Verbal Aggression", "Anger", "Hostility")
    )) %>% 
    gghistogram(x = "value",
                fill = "lightgray",
                binwidth = 2,
                rug = TRUE,
                xlab = "BPAQ Score") +
    theme_pander() +
    facet_wrap(~ type, nrow = 1, scales = "free")
ggexport(hist_fig,
         filename = here("outputs", "figs", "BPAQ_histogram.pdf"),
         width = 16,
         height = 4)

log_hist_fig <- data %>% 
    select(matches("BPAQ_[tpvah]")) %>% 
    pivot_longer(everything(), names_to = "type", values_to = "value") %>% 
    mutate(value = log(value)) %>% 
    mutate(type = factor(
        type, 
        levels = c("BPAQ_tot", "BPAQ_phy", "BPAQ_verb", "BPAQ_anger", "BPAQ_host"),
        labels = c("Total", "Physical Aggression", "Verbal Aggression", "Anger", "Hostility")
    )) %>% 
    gghistogram(x = "value",
                fill = "lightgray",
                binwidth = 0.3,
                rug = TRUE,
                xlab = "BPAQ Score (Log)") +
    theme_pander() +
    facet_wrap(~ type, nrow = 1, scales = c("free")) 
ggexport(log_hist_fig,
         filename = here("outputs", "figs", "BPAQ_histogram_log_transform.pdf"),
         width = 16,
         height = 4)
