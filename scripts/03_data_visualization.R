# Environment ------------------------------------------------------------------
library(tidyverse)
library(here)
library(gtsummary)
library(ggside)
library(ggpubr)
library(flextable)
library(officer)
library(ggthemes)
library(patchwork)
library(bootnet)
library(qgraph)

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
psychopathology_label <- c("Depression", "Mania", "Hypomania", "General Anxiety", 
                           "Social Anxiety", "Obsessive-Compulsive", 
                           "Psychotic-like Experiences", "Prodromal Psychosis")
aggressiveness_list <- c("bpaq_tot", "bpaq_phy", "bpaq_verb", "bpaq_anger", 
                         "bpaq_host")
aggressiveness_label <- c("BPAQ total", "BPAQ physical aggression",
                          "BPAQ verbal aggression", "BPAQ anger",
                          "BPAQ hostility")


discovery_df <- read_rds(here("data", "processed", "discovery_dataset.rds"))
holdout_df <- read_rds(here("data", "processed", "holdout_dataset.rds"))

full_df <- discovery_df %>% 
    mutate(split = "Discovery", .before = Age) %>% 
    bind_rows(holdout_df %>% mutate(split = "Holdout", .before = Age))


# Distribution Visualization --------------------------------------------------

full_df %>% 
    group_by(Sex) %>% 
    summarize(
        mean_age = mean(Age),
        sd_age = sd(Age)
    )

full_compaison_table <- full_df %>% 
    select(split, Age, Sex, Edu_TotYr, bpaq_tot, g_psy) %>%
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


# Correlation between Trait Aggression and General Psychopathology -------------
# show the correlations
full_df %>%
    select(split, g_psy, bpaq_tot) %>% 
    group_by(split) %>% 
    correlation::correlation(method = "pearson", p_adjust = "fdr")


hexgonplot_corr <- full_df %>% 
    ggplot(aes(x = g_psy, y = bpaq_tot)) +
        geom_hex() +
        scale_fill_gradient(low = "grey70", high = "grey5") +
        geom_smooth(method = "lm", color = "tomato3", fill = "tomato3") +
        stat_cor(method = "pearson", label.x = 6, label.y = 13) +
        labs(
            x = "General Psychopathology (a.u.)", 
            y = "BPAQ Total",
            fill = "Bin Size"
        ) +
        geom_xsidedensity(
            aes(y = stat(density)),
            color = NA, 
            fill = "lightgrey",
            alpha    = 0.7
        ) +
        geom_ysidedensity(
            aes(x = stat(density)), 
            color = NA, 
            fill = "lightgrey",
            alpha    = 0.7
        ) +
        facet_wrap(vars(split)) +
        guides(fill = guide_colourbar(barwidth = 10, barheight = 1)) +
        theme_minimal() +
        theme(
            legend.position = "bottom",
            plot.margin = margin(2, 2, 2, 2, "mm"),
            ggside.panel.scale.x = 0.2,
            ggside.panel.scale.y = 0.2,
            text = element_text(size = 24)
        ) +
        theme_ggside_void()
hexgonplot_corr
        
ggsave(
    plot = hexgonplot_corr, 
    filename = here("outputs", "figs", "hexgonplot_aggressiveness_psychopathology.pdf"),
    width = 14,
    height = 8
)

corr_heatmap_discovery <- discovery_df %>% 
    correlation::correlation(
        select = c("g_psy", psychopathology_list),
        select2 = aggressiveness_list,
        method = "pearson", p_adjust = "fdr"
    ) %>% 
    as_tibble() %>% 
    mutate(r = if_else(p < 0.05, r, 0)) %>% 
    filter(!str_detect(Parameter1, "bpaq")) %>% 
    filter(str_detect(Parameter2, "bpaq")) %>% 
    mutate(Parameter1 = factor(
        Parameter1, 
        levels = c("g_psy", psychopathology_list),
        labels = c("General Psychopathology", psychopathology_label)
    )
    ) %>% 
    mutate(Parameter2 = factor(
        Parameter2, 
        levels = rev(aggressiveness_list),
        labels = rev(aggressiveness_label)
    )
    ) %>% 
    ggplot(aes(x = Parameter1, y = Parameter2, fill = r)) +
        geom_tile(color = "grey10") +
        scale_fill_viridis_c(begin = 0.2, limits = c(0.05, 0.65)) +
        labs(x = "", y = "", title = "Discovery") +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

corr_heatmap_holdout <- holdout_df %>% 
    correlation::correlation(
        select = c("g_psy", psychopathology_list),
        select2 = aggressiveness_list,
        method = "pearson", p_adjust = "fdr"
    ) %>% 
    as_tibble() %>% 
    mutate(r = if_else(p < 0.05, r, 0)) %>% 
    filter(!str_detect(Parameter1, "bpaq")) %>% 
    filter(str_detect(Parameter2, "bpaq")) %>% 
    mutate(Parameter1 = factor(
            Parameter1, 
            levels = c("g_psy", psychopathology_list),
            labels = c("General Psychopathology", psychopathology_label)
        )
    ) %>% 
    mutate(Parameter2 = factor(
            Parameter2, 
            levels = rev(aggressiveness_list),
            labels = rev(aggressiveness_label)
        )
    ) %>% 
    ggplot(aes(x = Parameter1, y = Parameter2, fill = r)) +
        geom_tile(color = "grey10") +
        scale_fill_viridis_c(begin = 0.2, limits = c(0.05, 0.65)) +
        labs(x = "", y = "", title = "Holdout") +
        theme(
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
        )

corr_heatmap <- (corr_heatmap_discovery + corr_heatmap_holdout) +
    plot_layout(guides = "collect") &
        theme_pander() &
        theme(plot.margin = margin(2, 2, 2, 2, "mm"))

ggsave(
    plot = corr_heatmap, 
    filename = here("outputs", "figs", "corr_heatmap_aggressiveness_psychopathology.pdf"),
    width = 8,
    height = 3.5
)
