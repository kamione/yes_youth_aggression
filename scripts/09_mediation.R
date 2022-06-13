# Environment ------------------------------------------------------------------
library(tidyverse)
library(here)
library(haven)
library(bootnet)
library(lavaan)
library(easystats)


# Data IO  ---------------------------------------------------------------------
psychopathology_list <- c("PHQ9_tot", "YMRS_tot", "HCL32_tot", "GAD7_tot",
                          "LSAS_Tot", "YBOCS_Sym", "Cape_distress_tot",
                          "PQB_tot")


holdout_df <- read_rds(here("data", "processed", "holdout_dataset.rds")) %>% 
    drop_na(BIS_2, BIS_6, BFI_37) %>% 
    mutate(x_factor = BIS_2 + BIS_6 + BFI_37) %>% 
    select(cidi_e34, Age, Sex, bpaq_tot:bpaq_host, all_of(psychopathology_list), g_psy, x_factor) %>% 
    mutate(cidi_e34 = ordered(cidi_e34))

holdout_df %>% 
    glm(
        formula = cidi_e34 ~ Age + Sex + bpaq_tot + g_psy + x_factor, 
        family = binomial(link = "logit")
    ) %>% 
    model_parameters(standardize = "refit") %>% 
    plot() +
        scale_y_discrete(
            labels = c("Impulsivity", "General Psychopathology", "Aggression", "Sex [Females]", "Age")
        )
        theme_pander() +
        theme(legend.position = "none")


cor_res <- holdout_df %>% 
    select(bpaq_tot:x_factor) %>% 
    rename(
        "Antagonistic Impulsivity" = "x_factor",
        "Triat Aggression" = "bpaq_tot",
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
        "General Psychopathology" = "g_psy"
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
model1 <- ' # direct effect
             g_psy ~ c*x_factor
           # mediator
             bpaq_tot ~ a*x_factor
             g_psy ~ b*bpaq_tot
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '
fit1 <- sem(
    model1,
    data = holdout_df,
    meanstructure = TRUE,
    se = "bootstrap",
    bootstrap = 10000
)
summary(fit1, fit.measure = TRUE, standardized = TRUE, ci = TRUE)
standardizedSolution(fit1)

# using aggression as a mediator
psych::mediate(g_psy ~ x_factor + (bpaq_tot),
               data = holdout_df,
               std = TRUE,
               n.iter = 10000) %>%
    print(short = FALSE)


set.seed(1234)
model2 <- ' # direct effect
             bpaq_tot ~ c*x_factor
           # mediator
             g_psy ~ a*x_factor
             bpaq_tot ~ b*g_psy
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '
fit2 <- sem(
    model2,
    data = holdout_df,
    meanstructure = TRUE,
    se = "bootstrap",
    bootstrap = 10000
)
summary(fit2, fit.measure = TRUE, standardized = TRUE, ci = TRUE)
standardizedSolution(fit2)

# using psychopathology as a mediator
psych::mediate(bpaq_tot ~ x_factor + (g_psy),
               data = holdout_df,
               std = TRUE,
               n.iter = 10000) %>%
    print(short = FALSE)


multipleMediation <- '
cidi_e34 ~ b1 * g_psy + b2 * bpaq_tot + c * x_factor
g_psy ~ a1 * x_factor
bpaq_tot ~ a2 * x_factor
indirect1 := a1 * b1
indirect2 := a2 * b2
total := c + (a1 * b1) + (a2 * b2)
g_psy ~~ bpaq_tot
'
fit3 <- sem(
    multipleMediation,
    data = holdout_df,
    se = "bootstrap",
    bootstrap = 10000,
    estimator = "DWLS"
)

summary(fit3, fit.measure = TRUE, standardized = TRUE, ci = TRUE)
standardizedSolution(fit3)
inspect(fit3, "r2")
