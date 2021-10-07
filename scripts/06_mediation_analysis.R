# Environment ------------------------------------------------------------------
library(tidyverse)
library(here)
library(haven)
library(bootnet)
library(lavaan)
library(semPlot)
library(mice)

# Data IO  ---------------------------------------------------------------------
data_psychopathology <- here("data", "raw", "27 Jun 21 - FINAL ALL.sav") %>% 
    read_spss(user_na = TRUE) %>% 
    filter(str_detect(Parti_ID, "YES")) %>% 
    select(Parti_ID, PHQ9_tot, YMRS_tot, HCL32_tot, GAD7_tot, LSAS_Tot, YBOCS_Sym,
           TSQ_tot, Cape_distress_tot, PQB_tot) %>% 
    zap_labels()

mice_res <- mice(data_psychopathology %>% select(-Parti_ID),
     m =  10, seed = 1234, use.matcher = TRUE, printFlag = FALSE)

imp <- complete(mice_res) %>% 
    mutate(Parti_ID = data_psychopathology$Parti_ID)

data <- here("data", "processed", "yes_baseline_outcome-aggression_n-2215_p-294.csv") %>% 
    read_csv(col_types = cols()) %>% 
    mutate(aggression = rowSums(across(BPAQ_1:BPAQ_12))) %>% 
    mutate(irritability = rowSums(across(c(DASS_S_18, BIS_17, BFI_12)))) %>% 
    select(Parti_ID, aggression, irritability, Age, Sex, Edu_TotYr) %>% 
    left_join(imp, by = "Parti_ID") %>% 
    rename(
        "Aggression" = "aggression",
        "Irritability" = "irritability",
        "Depression (PHQ-9)" = "PHQ9_tot",
        "GAD (GAD-7)" = "GAD7_tot",
        "Mania (YMRS)" = "YMRS_tot",
        "Hypomania (HCL-32)" = "HCL32_tot",
        "PLE (CAPE-P-15)" = "Cape_distress_tot",
        "Prodrome (PQB)" = "PQB_tot",
        "OCD (Y-BOCS)" = "YBOCS_Sym",
        "PTSD (TSQ)" = "TSQ_tot",
        "Social Phobia (LSAS)" = "LSAS_Tot"
    )


pca_res <- princomp(data %>% select(`Depression (PHQ-9)`:`Social Phobia (LSAS)`), cor = TRUE, scores = TRUE)

data_g <- data %>% 
    mutate(`Psychopathology (g)` = pca_res$scores[, 1])



cor_data <- data_g %>% 
    select(-c(Parti_ID, Age, Sex, Edu_TotYr)) %>% 
    psych::corr.test(method = "spearman")

pdf(here("outputs", "figs", "spearman_corr_aggression_irritability_psy.pdf"), height = 8, width = 8)
corrplot::corrplot(
    cor_data$r,
    col = colorRampPalette(c("#0C6291", "#FBFEF9", "#A63446"))(256),
    method = "square",
    mar = rep(0, 4),
    p.mat = matrix(p.adjust(cor_data$p, method = "bonferroni"),  nrow = 12),
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
             Aggression ~ c*Irritability
           # mediator
             g_psy ~ a*Irritability
             Aggression ~ b*g_psy
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '
fit <- sem(model,
           data = data_g %>% rename(g_psy = "Psychopathology (g)"),
           meanstructure = TRUE,
           se = "bootstrap",
           bootstrap = 10000)
summary(fit, fit.measure = TRUE, standardized = TRUE, ci = TRUE)
standardizedSolution(fit)
parameterEstimates(fit, boot.ci.type = "bca.simple", standardized = TRUE)

# double check with the mediate function from psych
psych::mediate(Aggression ~ Irritability + (g_psy),
               data = data_g %>% rename(g_psy = "Psychopathology (g)"),
               std = TRUE,
               n.iter = 10000) %>%
    print(short = FALSE)


