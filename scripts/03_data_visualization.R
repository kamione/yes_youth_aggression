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
    bind_rows(validation_df %>% mutate(split = "Holdout", .before = Age))


# Distribution Visualization --------------------------------------------------
full_compaison_table <- full_df %>% 
    select(split, Age, Sex, Edu_TotYr, cidi_e34, bpaq_tot, g_psy) %>%
    tbl_summary(
        by = split,
        statistic = list(all_continuous() ~ "{mean} ({sd})"),
        digits = all_continuous() ~ c(2, 2),
        label = list(
            Edu_TotYr = "Years of Education",
            bpaq_tot = "Trait Aggression",
            g_psy = "General Psychopathology",
            cidi_e34 = "Aggressive Behavior (Lifetime)"
        )
    ) %>% 
    add_overall() %>% 
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
    mutate(cidi_e34 = factor(cidi_e34, levels = c(0, 1), labels = c("No", "Yes"))) %>% 
    #filter(psy_rank %in% c(1, 4)) %>% 
    #mutate(psy_rank = factor(
    #        psy_rank, levels = c(1, 4), 
    #        labels = c("Low", "High")
    #   )
    #) %>% 
    select(Age, Sex, Edu_TotYr, cidi_e34, bpaq_tot:bpaq_host, g_psy,
           all_of(psychopathology_list)) %>% 
    tbl_summary(
        by = cidi_e34,
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
            bpaq_host = "BPAQ hostility",
            cidi_e34 = "Aggressive Behavior (Lifetime)",
            g_psy = "General Psychopathology"
        )
    ) %>% 
    add_p() %>% 
    add_q() %>% 
    bold_p(q = TRUE)

holdout_comparison_table <- validation_df %>% 
    mutate(cidi_e34 = factor(cidi_e34, levels = c(0, 1), labels = c("No", "Yes"))) %>% 
    select(Age, Sex, Edu_TotYr, cidi_e34, bpaq_tot:bpaq_host, g_psy,
           all_of(psychopathology_list)) %>% 
    tbl_summary(
        by = cidi_e34,
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
            bpaq_host = "BPAQ hostility",
            cidi_e34 = "Aggressive Behavior (Lifetime)",
            g_psy = "General Psychopathology"
        )
    ) %>% 
    add_p() %>% 
    add_q() %>% 
    bold_p(q = TRUE)

merged_comparison_table <- tbl_merge(
    tbls = list(discovery_comparison_table, holdout_comparison_table),
    tab_spanner = c("**Discovery Dataset**", "**Holdout Dataset**"))
merged_comparison_table

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

m1_discovery <- discovery_df %>% 
    glm(formula = cidi_e34 ~ Age + Sex + Edu_TotYr + bpaq_tot + g_psy, family = binomial(link="logit"))

m1_holdout <- validation_df %>% 
    glm(formula = cidi_e34 ~ Age + Sex + Edu_TotYr + bpaq_tot + g_psy, family = binomial(link="logit"))

m1_discovery_table <- tbl_merge(
    list(
        tbl_regression(
            m1_discovery, 
            tidy_fun = tidy_standardize,
            label = list(
                Edu_TotYr = "Years of Education",
                bpaq_tot = "Trait Aggression",
                g_psy = "General Psychopathology"
            )
        ) ,
        tbl_regression(
            m1_discovery,
            label = list(
                Edu_TotYr = "Years of Education",
                bpaq_tot = "Trait Aggression",
                g_psy = "General Psychopathology"
            )
        ) %>%
            modify_column_hide(c(estimate, ci)) %>% 
            bold_p
    )
) %>% 
    modify_spanning_header(c(estimate_1, ci_1, p.value_2) ~ NA)

m1_holdout_table <- tbl_merge(
    list(
        tbl_regression(
            m1_holdout, 
            tidy_fun = tidy_standardize,
            label = list(
                Edu_TotYr = "Years of Education",
                bpaq_tot = "Trait Aggression",
                g_psy = "General Psychopathology"
            )
        ),
        tbl_regression(
            m1_holdout,
            label = list(
                Edu_TotYr = "Years of Education",
                bpaq_tot = "Trait Aggression",
                g_psy = "General Psychopathology"
            )
        ) %>%
            modify_column_hide(c(estimate, ci)) %>% 
            bold_p
    )
) %>% 
    modify_spanning_header(c(estimate_1, ci_1, p.value_2) ~ NA)

merged_reg_table <- tbl_merge(
    tbls = list(m1_discovery_table, m1_holdout_table),
    tab_spanner = c("**Discovery Dataset**", "**Holdout Dataset**"))
merged_reg_table


merged_reg_table %>% 
    as_gt() %>% 
    gt::gtsave(here("outputs", "tables", "reg_aggresivebehavior.html"))

merged_reg_table %>% 
    as_flex_table() %>% 
    bold(part = "header") %>% 
    save_as_docx(
        path = here("outputs", "tables", "reg_aggresivebehavior.docx"),
        pr_section = sect_properties_wide
    )


# Visualization Partial Correlation --------------------------------------------
psychopathology_label <- c("DEP", "Mania", "HM", "GAD", "LSAS", "OCD", "CAPE",
                           "PQB")

network_data <- list()
network_data$data <- full_df %>% select(cidi_e34, bpaq_phy:bpaq_host, all_of(psychopathology_list)) 
network_data$node_col <- c(rep("#B5CAA0", 5), rep("#DAC9A6", 8))
network_data$lable <- c("AggrBeh", "Phy", "Verb", "Anger", "Host", psychopathology_label)
network_data$group <- c("1. Outcomes", rep("2. Features", 8))

library(bootnet)
library(qgraph)
g <- estimateNetwork(
    network_data$data,
    default = "EBICglasso",
    threshold = TRUE
)

qgraph(g$graph, layout = "spring", labels = network_data$lable)

