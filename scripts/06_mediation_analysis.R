# Environment ------------------------------------------------------------------
library(tidyverse)
library(here)
library(bootnet)
library(qgraph)
library(bnlearn)
library(Cairo)
library(snow)
library(lavaan)
library(semPlot)

# Data IO  ---------------------------------------------------------------------
data_psychopathology <- here("data", "raw", "27 Jun 21 - FINAL ALL.sav") %>% 
    read_spss(user_na = TRUE) %>% 
    filter(str_detect(Parti_ID, "YES")) %>% 
    select(Parti_ID, PHQ9_tot, GAD7_tot, YMRS_tot, HCL32_tot, Cape_distress_tot,
           PQB_tot, YBOCS_Sym, TSQ_tot, LSAS_Tot)


data <- here("data", "processed", "yes_baseline_outcome-aggression_n-2215_p-294.csv") %>% 
    read_csv(col_types = cols()) %>% 
    mutate(aggression = rowSums(across(BPAQ_1:BPAQ_12))) %>% 
    mutate(irritability = rowSums(across(c(DASS_S_18, BIS_17, BFI_12)))) %>% 
    select(Parti_ID, aggression, irritability) %>% 
    left_join(data_psychopathology, by = "Parti_ID")


cor_data <- data %>% 
    select(-Parti_ID) %>% 
    psych::corr.test()

corrplot::corrplot(
    cor_data$r,
    col = colorRampPalette(c("#0C6291", "#FBFEF9", "#A63446"))(256),
    method = "square",
    mar = rep(1, 4),
    p.mat = cor_data$p,
    tl.col = "#1C1C1C",
    tl.srt = 45,
    type = c("upper"),
    diag = FALSE,
    rect.col = "lightgrey",
    number.digits = 2
)

set.seed(1234)
model <- ' # direct effect
             aggression ~ c*irritability
           # mediator
             LSAS_Tot ~ a*irritability
             aggression ~ b*LSAS_Tot
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '
fit <- sem(model, data = data, se = "bootstrap", bootstrap = 1000)
summary(fit, standardized = TRUE)
semPaths(fit,
         whatLabels = "std",
         layout = "tree", 
         sizeMan = 15,
         sizeInt = 15,
         sizeLat = 15,
         rotation = 2,
         edge.label.cex=1.5)

set.seed(1234)
model <- ' # direct effect
             aggression ~ c*irritability
           # mediator
             GAD7_tot ~ a*irritability
             aggression ~ b*GAD7_tot
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '

fit <- sem(model, data = data)
summary(fit, standardized = TRUE)

semPaths(fit,
         whatLabels = "std",
         style = "lisrel",
         layout = "tree", 
         sizeMan = 15,
         sizeInt = 15,
         sizeLat = 15,
         rotation = 2,
         edge.label.cex=1.5,
         loop = 0)


