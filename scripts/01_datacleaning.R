# Environment ------------------------------------------------------------------
library(tidyverse)
library(haven)
library(here)
library(ggpubr)
library(glue)

# Data I/O ---------------------------------------------------------------------
df <- here("data", "raw", "27 Jun 21 - FINAL ALL.sav") %>% 
    read_spss(user_na = TRUE)

# Data Cleaning ----------------------------------------------------------------
filtered_df <- df %>% 
    filter(str_detect(Parti_ID, "YES")) %>%
    select(Parti_ID, Age, Sex, BMI, Edu_TotYr,
           starts_with(c("BPAQ", # outcome
                         "Birth",
                         "DS", "VF", "Inf", "TMT", "Stroop", "STROOP", "PG_", # Cognitive
                         "ISI", "PSQI", "ESS", "IPAQ", "FRS", "BSS", "AUDIT_12m", # lifestyle
                         "SF12", "SOFAS_6m", # general functioning
                         "K10", "SDS", # distress and disability
                         "BFI", # personality
                         "SES", "BHS", "PS", "CDRISC", "BIS", "GCQ", "PPAAUS",
                         "FOI", "YMS", "UCLA", # psychological constructs
                         "WHO5", "DASS" # health
                         # "PHQ9", "GAD7", "YMRS", "HCL32", "Cape", "PQB",
                         # "YBOCS", "TSQ", "LSAS", "EDEQ", # psychopathology
                         # "SPQB", "SAPAS" # personality disorder
                         ))) %>%
    select(-ends_with(c("tot", "TEXT", "group", "mean", "Sym", "total"))) %>%
    select(-matches("BFI_[ocean]")) %>% # remove sum scores
    select(-matches("MOCA_[sAMSE]")) %>% # remove sum scores
    select(-matches("BSS_[EBTD]")) %>%
    select(-matches("SF12_[PM]")) %>%
    select(-matches("sympreact")) %>%  # remove sum scores
    select(Parti_ID, which(sapply(., class) != "character")) %>%  # remove variables with strings 
    select_if(~sum(is.na(.))/length(.) < 0.25) %>%  # remove missing > 25% item
    zap_labels() %>%
    zap_formats() %>%
    zap_widths() %>%
    na_if(-999) %>% 
    mutate(n_na_prop = rowSums(is.na(.)) / dim(.)[2]) %>% 
    filter(n_na_prop < 0.05) %>% # remove individuals with NA items more than 5%
    select(-n_na_prop) %>% 
    filter(is.na(rowSums(across(BPAQ_1:BPAQ_12))) == 0)

size <- dim(filtered_df)
write_csv(filtered_df, here("data", "processed", glue("yes_baseline_outcome-aggression_n-{size[1]}_p-{size[2]}.csv")))


# Missing Pattern --------------------------------------------------------------
missing_values <- filtered_df %>%
    gather(key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%
    group_by(key) %>%
    mutate(total = n()) %>%
    group_by(key, total, isna) %>%
    summarise(num.isna = n(), .groups = "drop") %>%
    mutate(pct = num.isna / total) %>% 
    mutate(key = factor(key, levels = str_sort(unique(key), decreasing = TRUE)))

levels <- (missing_values %>% filter(isna == TRUE) %>% arrange(desc(pct)))$key

proportion_plot <- missing_values %>%
    ggplot() +
        geom_bar(aes(x = reorder(key, desc(pct)), y = pct, fill = isna), stat = 'identity', alpha = 0.8) +
        scale_x_discrete(limits = levels) +
        scale_fill_manual(name = "", values = c('grey20', 'tomato2'), labels = c("Present", "Missing")) +
        coord_flip() +
        theme(axis.text.y = element_text(size = 8)) +
        theme(legend.position = "none") +
        labs(x = "Item", y = "Proportion of Missing")

# missing patterns
row_plot <- filtered_df %>%
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
             width    = 8,
             height   = 15)