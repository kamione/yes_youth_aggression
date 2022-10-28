# Environment ------------------------------------------------------------------
library(tidyverse)
library(haven)
library(here)
library(ggpubr)
library(glue)


# Data I/O ---------------------------------------------------------------------
psychopathology_list <- c("PHQ9_tot", "YMRS_tot", "HCL32_tot", "GAD7_tot",
                          "LSAS_Tot", "YBOCS_Sym", "Cape_distress_tot",
                          "PQB_tot")

df <- here("data", "raw", "27 Jun 21 - FINAL ALL.sav") %>% 
    read_spss(user_na = TRUE) %>%
    zap_labels() %>%
    zap_label() %>% 
    zap_formats() %>%
    zap_widths() %>%
    zap_missing() %>% 
    # remove multiple abnormal values (checked by skimr::skim()) 
    na_if(-999) %>% 
    na_if(998) %>% 
    na_if(44198) %>% 
    na_if(45692) %>% 
    na_if(15813) %>% 
    filter(str_detect(Parti_ID, "YES")) %>% 
    drop_na(Age, Sex, Edu_TotYr, BPAQ_1:BPAQ_12, all_of(psychopathology_list))


# Preporcessing ----------------------------------------------------------------
preprocessed_df <- df %>% 
    select(-ends_with(c("tot", "TEXT", "group", "mean", "Sym", "total", "Anx",
                        "Stress", "Depress"))) %>%
    select(-matches("BFI_[ocean]")) %>% # remove sum scores
    select(-matches("MOCA_[sAMSE]")) %>% # remove sum scores
    select(-matches("BSS_[EBTD]")) %>%
    select(-matches("SF12_[PM]")) %>%
    select(-matches("sympreact")) %>% 
    select(Parti_ID, Age, Sex, BMI, Edu_TotYr, # demographics
           starts_with(c(
               "BPAQ", # outcome
               "DS", "VF", "Inf", "TMT", "Stroop", "STROOP", "PG_", # cognition
               "ISI", "PSQI", "ESS", "IPAQ", "AUDIT_12m", # lifestyle
               "SF12", "SOFAS_6m", "WHO5", # general functioning
               "BFI", "SES", "Procras", "CDRISC", "BIS", "GCQ", "PPAAUS",
               "FOI", "YMS", "UCLA", "BSS" # psychological constructs
           ))
    ) %>% 
    # add total psychopathology scores
    bind_cols(select(df, all_of(psychopathology_list))) %>% 
    # remove variables with strings 
    select(Parti_ID, which(sapply(., class) != "character")) %>%  
    # remove missing > 25% item
    select_if(~sum(is.na(.))/length(.) < 0.25) %>%  
    # remove individuals with NA items more than 5%
    filter(rowSums(is.na(.)) / dim(.)[2] < 0.05) %>% 
    mutate(
        Sex = factor(Sex, levels = c(1, 2), labels = c("Males", "Females"))
    ) %>% 
    mutate(bpaq_tot = rowSums(across(BPAQ_1:BPAQ_12)), .before = BPAQ_1) %>% 
    mutate(bpaq_phy = rowSums(across(BPAQ_1:BPAQ_3)), .before = BPAQ_1) %>% 
    mutate(bpaq_verb = rowSums(across(BPAQ_4:BPAQ_6)), .before = BPAQ_1) %>% 
    mutate(bpaq_anger = rowSums(across(BPAQ_7:BPAQ_9)), .before = BPAQ_1) %>% 
    mutate(bpaq_host = rowSums(across(BPAQ_10:BPAQ_12)), .before = BPAQ_1) %>% 
    select(-c(BPAQ_1:BPAQ_12))


# Save -------------------------------------------------------------------------
# write .rds for other steps
df_size <- dim(preprocessed_df)
write_rds(
    preprocessed_df, 
    here("data", "processed",
        glue(
            "yes_baseline_outcome-aggression_n-{df_size[1]}_p-{df_size[2]}.rds"
        )
    )
)


# Missing Pattern --------------------------------------------------------------
missing_values <- preprocessed_df %>%
    gather(key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%
    group_by(key) %>%
    mutate(total = n()) %>%
    group_by(key, total, isna) %>%
    summarise(num.isna = n(), .groups = "drop") %>%
    mutate(pct = num.isna / total) %>% 
    mutate(key = factor(key, levels = str_sort(unique(key), decreasing = TRUE)))

levels <- (missing_values %>% 
               filter(isna == TRUE) %>% 
               arrange(desc(pct)))$key %>% 
    droplevels()

proportion_plot <- missing_values %>%
    ggplot() +
        geom_bar(
            aes(x = reorder(key, desc(pct)), y = pct, fill = isna), 
            stat = 'identity', 
            alpha = 0.8
        ) +
        scale_x_discrete(limits = levels) +
        scale_fill_manual(
            name = "", 
            values = c('grey20', 'tomato2'),
            labels = c("Present", "Missing")
        ) +
        coord_flip() +
        theme(axis.text.y = element_text(size = 8)) +
        theme(legend.position = "none") +
        labs(x = "Item", y = "Proportion of Missing")

# missing patterns
row_plot <- preprocessed_df %>%
    mutate(id = row_number()) %>%
    gather(key = "key", value = "val", -id) %>%
    mutate(isna = is.na(val)) %>%
    ggplot(aes(key, id, fill = isna)) +
        geom_raster(alpha = 0.8) +
        scale_x_discrete(limits = levels) +
        scale_fill_manual(name   = "",
                          values = c('grey20', 'tomato2'),
                          labels = c("Present", "Missing")) +
        labs(y = "Missing in Individuals") +
        theme(axis.title.y = element_blank(),
              axis.text.y  = element_text(size = 8)) +
        theme(legend.position = "none") +
        coord_flip()

# save plot
ggarrange(proportion_plot, row_plot, ncol = 2, nrow = 1) %>% 
    ggexport(filename = here("outputs", "figs", "missing_itemwise_plot.pdf"),
             width    = 20,
             height   = 15)
