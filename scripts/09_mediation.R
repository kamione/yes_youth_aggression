# Environment ------------------------------------------------------------------
library(tidyverse)
library(here)
library(haven)
library(bootnet)
library(lavaan)


# Data IO  ---------------------------------------------------------------------
psychopathology_list <- c("PHQ9_tot", "YMRS_tot", "HCL32_tot", "GAD7_tot",
                          "LSAS_Tot", "YBOCS_Sym", "Cape_distress_tot",
                          "PQB_tot")


holdout_df <- read_rds(here("data", "processed", "holdout_dataset.rds")) %>% 
    drop_na(BIS_2, BIS_6, BIS_9, BFI_37) %>% 
    mutate(x_factor = BIS_2 + BIS_6 + BIS_9 + BFI_37) %>% 
    select(bpaq_tot:bpaq_host, all_of(psychopathology_list), g_psy, x_factor) 
    

cor_res <- holdout_df %>% 
    rename(
        "Impulsivity" = "x_factor",
        "Aggression" = "bpaq_tot",
        "Physical Aggression" = "bpaq_phy",
        "Verbal Aggression" = "bpaq_verb",
        "Hostility" = "bpaq_host",
        "Anger" = "bpaq_anger",
        "Depression (PHQ-9)" = "PHQ9_tot",
        "GAD (GAD-7)" = "GAD7_tot",
        "Mania (YMRS)" = "YMRS_tot",
        "Hypomania (HCL-32)" = "HCL32_tot",
        "GAD (GAD-7)" = "GAD7_tot",
        "Social Anxiety (LSAS)" = "LSAS_Tot",
        "OCD (Y-BOCS)" = "YBOCS_Sym",
        "PLE (CAPE-P-15)" = "Cape_distress_tot",
        "Prodrome (PQB)" = "PQB_tot",
        "Psychopathology (g)" = "g_psy"
    ) %>% 
    psych::corr.test(method = "spearman")

pdf(here("outputs", "figs", "data-holdout_desc-spearman_corr_aggression.pdf"), height = 8, width = 8)
corrplot::corrplot(
    cor_res$r,
    col = colorRampPalette(c("#0C6291", "#FBFEF9", "#A63446"))(256),
    method = "square",
    mar = rep(0, 4),
    p.mat = cor_res$p,
    tl.col = "#1C1C1C",
    tl.srt = 45,
    type = c("upper"),
    diag = FALSE,
    rect.col = "lightgrey",
    number.digits = 2
)
dev.off()


set.seed(1234)
model <- ' # direct effect
             g_psy ~ c*x_factor
           # mediator
             bpaq_tot ~ a*x_factor
             g_psy ~ b*bpaq_tot
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '
fit <- sem(
    model,
    data = holdout_df,
    meanstructure = TRUE,
    se = "bootstrap",
    bootstrap = 10000
)
summary(fit, fit.measure = TRUE, standardized = TRUE, ci = TRUE)
standardizedSolution(fit)
parameterEstimates(fit, boot.ci.type = "bca.simple", standardized = TRUE)

# using aggression as a mediator
psych::mediate(g_psy ~ x_factor + (bpaq_tot),
               data = holdout_df,
               std = TRUE,
               n.iter = 10000) %>%
    print(short = FALSE)

# using psychopathology as a mediator
psych::mediate(bpaq_tot ~ x_factor + (g_psy),
               data = holdout_df,
               std = TRUE,
               n.iter = 10000) %>%
    print(short = FALSE)




