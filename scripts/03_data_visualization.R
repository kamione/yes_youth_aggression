# Environment ------------------------------------------------------------------
library(tidyverse)
library(here)
library(gtsummary)
library(flextable)
library(officer)

sect_properties_wide <- prop_section(
    page_size = page_size(orient = "landscape",
                          width = 8.3, height = 11.7),
    type = "continuous",
    page_margins = page_mar()
)


# Data IO  ---------------------------------------------------------------------
psychopathology_list <- c("PHQ9_tot", "YMRS_tot", "HCL32_tot", "GAD7_tot",
                          "LSAS_Tot", "YBOCS_Sym", "Cape_distress_tot",
                          "PQB_tot")

discovery_df <- read_rds(here("data", "processed", "discovery_dataset.rds"))
validation_df <- read_rds(here("data", "processed", "holdout_dataset.rds"))

full_df <- discovery_df %>% 
    mutate(split = "Discovery", .before = Age) %>% 
    bind_rows(validation_df %>% mutate(split = "Validation", .before = Age))


# Distribution Visualization --------------------------------------------------
full_compaison_table <- full_df %>% 
    select(split, Age, Sex, Edu_TotYr, bpaq_host, g_psy) %>%
    tbl_summary(
        by = split,
        statistic = list(all_continuous() ~ "{mean} ({sd})"),
        digits = all_continuous() ~ c(2, 2),
        label = list(
            Edu_TotYr = "Years of Education",
            bpaq_host = "BPAQ hostility",
            g_psy = "Psychopathology (g)"
        )
    ) %>% 
    add_p() %>% 
    add_q() %>% 
    bold_p(q = TRUE)

full_compaison_table %>% 
    as_gt() %>% 
    gt::gtsave(here("outputs", "tables", "full_compaison.html"))

full_compaison_table %>% 
    as_flex_table() %>% 
    bold(part = "header") %>% 
    save_as_docx(
        path = here("outputs", "tables", "full_compaison.docx"),
        pr_section = sect_properties_wide
    )



# Table Benign vs. High Psychopathology ---------------------------------------
discovery_comparison_table <- discovery_df %>% 
    mutate(psy_rank = ntile(g_psy, 4)) %>% 
    filter(psy_rank %in% c(1, 4)) %>% 
    mutate(psy_rank = factor(
            psy_rank, levels = c(1, 4), 
            labels = c("Benign", "High")
        )
    ) %>% 
    select(psy_rank, Age, Sex, Edu_TotYr, bpaq_tot:bpaq_host, 
           all_of(psychopathology_list)) %>% 
    tbl_summary(
        by = psy_rank,
        statistic = list(all_continuous() ~ "{mean} ({sd})"),
        digits = all_continuous() ~ c(2, 2),
        label = list(
            Edu_TotYr = "Years of Education",
            PHQ9_tot = "Depression (PHQ-9)",
            YMRS_tot = "Mania (YMRS)",
            HCL32_tot = "Hypomania (HCL−32)",
            GAD7_tot = "GAD (GAD−7)",
            LSAS_Tot = "Social Anxiety (LSAS)",
            YBOCS_Sym = "OCD (Y−BOCS)",
            Cape_distress_tot = "PLE (CAPE−P−15)",
            PQB_tot = "Prodrome (PQB)",
            bpaq_tot = "BPAQ total (log)",
            bpaq_phy = "BPAQ physical aggression",
            bpaq_verb = "BPAQ verbal aggression",
            bpaq_anger = "BPAQ anger",
            bpaq_host = "BPAQ hostility"
        )
    ) %>% 
    add_p() %>% 
    add_q() %>% 
    bold_p(q = TRUE)

validation_comparison_table <- validation_df %>% 
    mutate(psy_rank = ntile(g_psy, 4)) %>% 
    filter(psy_rank %in% c(1, 4)) %>% 
    mutate(psy_rank = factor(
        psy_rank, levels = c(1, 4), 
        labels = c("Benign", "High")
    )
    ) %>% 
    select(psy_rank, Age, Sex, Edu_TotYr, bpaq_tot:bpaq_host, 
           all_of(psychopathology_list)) %>% 
    tbl_summary(
        by = psy_rank,
        type = YMRS_tot ~ "continuous",
        statistic = list(all_continuous() ~ "{mean} ({sd})"),
        digits = all_continuous() ~ c(2, 2),
        label = list(
            Edu_TotYr = "Years of Education",
            PHQ9_tot = "Depression (PHQ-9)",
            YMRS_tot = "Mania (YMRS)",
            HCL32_tot = "Hypomania (HCL−32)",
            GAD7_tot = "GAD (GAD−7)",
            LSAS_Tot = "Social Anxiety (LSAS)",
            YBOCS_Sym = "OCD (Y−BOCS)",
            Cape_distress_tot = "PLE (CAPE−P−15)",
            PQB_tot = "Prodrome (PQB)",
            bpaq_tot = "BPAQ total (log)",
            bpaq_phy = "BPAQ physical aggression",
            bpaq_verb = "BPAQ verbal aggression",
            bpaq_anger = "BPAQ anger",
            bpaq_host = "BPAQ hostility"
        )
    ) %>% 
    add_p() %>% 
    add_q() %>% 
    bold_p(q = TRUE)

merged_comparison_table <- tbl_merge(
    tbls = list(discovery_comparison_table, validation_comparison_table),
    tab_spanner = c("**Discovery Dataset**", "**Validation Dataset**"))

merged_comparison_table %>% 
    as_gt() %>% 
    gt::gtsave(here("outputs", "tables", "comparison_begnin_high.html"))

merged_comparison_table %>% 
    as_flex_table() %>% 
    bold(part = "header") %>% 
    save_as_docx(
        path = here("outputs", "tables", "comparison_begnin_high.docx"),
        pr_section = sect_properties_wide
    )










# 
corr_res <- full_df %>% 
    select(bpaq_tot:bpaq_host, all_of(psychopathology_list)) %>% 
    psych::corr.test()


network_data <- list()
network_data$data <- preprocessed_df %>% select(bpaq_phy:bpaq_host, all_of(psychopathology_list)) 
network_data$node_col <- c(rep("#B5CAA0", 4), rep("#DAC9A6", 8))
network_data$lable <- c("Phy", "Verb", "Anger", "Host", psychopathology_label)
network_data$group <- c("1. Outcomes", rep("2. Features", 8))


g <- estimateNetwork(
    network_data$data,
    default = "EBICglasso",
    threshold = TRUE
)

g$graph %>% qgraph(layout = "spring", labels = network_data$lable, color = network_data$node_col)

