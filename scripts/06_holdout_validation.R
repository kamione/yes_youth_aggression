# Environment ------------------------------------------------------------------
library(tidyverse)
library(here)
library(tidymodels)
library(progress)
library(ggpubr)
library(ggthemes)
library(glue)


# Data IO  ---------------------------------------------------------------------
psychopathology_list <- c("PHQ9_tot", "YMRS_tot", "HCL32_tot", "GAD7_tot",
                          "LSAS_Tot", "YBOCS_Sym", "Cape_distress_tot",
                          "PQB_tot")

discovery_df <- here("data", "processed", "discovery_dataset.rds") %>% 
    read_rds() %>% 
    select(-c(Parti_ID, bpaq_phy:bpaq_host, all_of(psychopathology_list)))
holdout_df <- here("data", "processed", "holdout_dataset.rds") %>% 
    read_rds() %>% 
    select(-c(Parti_ID, bpaq_phy:bpaq_host, all_of(psychopathology_list)))

params_psy_df <- here("outputs", "cache", "model-psy_desc-model_parameters.rds") %>% 
    read_rds()
params_aggr_df <- here("outputs", "cache", "model-aggr_desc-model_parameters.rds") %>% 
    read_rds()
vi_psy_df <- here("outputs", "cache", "model-psy_desc-model_importance.rds") %>% 
    read_rds()
vi_aggr_df <- here("outputs", "cache", "model-aggr_desc-model_importance.rds") %>% 
    read_rds()

psy_intercept <- params_psy_df %>% 
    slice(1) %>% 
    mutate(mean = mean(c_across(starts_with("iter")), na.rm = TRUE), .after = variable) %>% 
    pull(mean)
aggr_intercept <- params_aggr_df %>% 
    slice(1) %>% 
    mutate(mean = mean(c_across(starts_with("iter")), na.rm = TRUE), .after = variable) %>% 
    pull(mean)


# Performance Psy:Psy ----------------------------------------------------------
params_psy_df_mean <- params_psy_df %>% 
    rowwise() %>%
    mutate(
        b_mean = mean(c_across(starts_with("iter")), na.rm = TRUE), 
        b_sd = sd(c_across(starts_with("iter")), na.rm = TRUE), 
        .after = variable) %>% 
    ungroup() %>% 
    select(-starts_with("iter"))

psy_sorted_df <- vi_psy_df %>% 
    rowwise() %>%
    mutate(
        vi_mean = mean(c_across(starts_with("iter")), na.rm = TRUE), 
        vi_sd = sd(c_across(starts_with("iter")), na.rm = TRUE), 
        .after = variable) %>% 
    ungroup() %>% 
    arrange(desc(abs(vi_mean))) %>% 
    select(-starts_with("iter")) %>% 
    left_join(params_psy_df_mean, by = "variable")

psy_sorted_feature_param <- pull(psy_sorted_df, b_mean)
psy_sorted_feature_names <- pull(psy_sorted_df, variable)
write_rds(psy_sorted_feature_names, here("outputs", "cache", "psy_sorted_feature_names.rds"))

preped_psy_recipe <- discovery_df %>%
    select(-bpaq_tot) %>% 
    recipe(g_psy ~ .) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_numeric_predictors()) %>% 
    step_center(all_numeric_predictors()) %>% 
    step_scale(all_numeric_predictors()) %>% 
    step_impute_knn(all_numeric_predictors(), neighbors = 20) %>% 
    prep()

baked_psy_holdout_df <- holdout_df %>% 
    select(-bpaq_tot) %>%
    bake(preped_psy_recipe, new_data = .)

psy_feature_holdout_df <- select(baked_psy_holdout_df, all_of(psy_sorted_feature_names))

psy_res_df <- (psy_intercept + as.matrix(psy_feature_holdout_df) %*% psy_sorted_feature_param) %>% 
    as_tibble(.name_repair = ~".pred") %>% 
    bind_cols(baked_psy_holdout_df %>% select(g_psy))

psy_r <- psy_res_df %>%
    correlation::correlation() %>% 
    pluck("r")
psy_p <- psy_res_df %>%
    correlation::correlation() %>% 
    pluck("p")
psy_rmse <- psy_res_df %>% 
    rmse(truth = g_psy, estimate = .pred) %>%
    pull(.estimate) %>% 
    format(digits = 3)
psy_mae <- psy_res_df %>% 
    mae(truth = g_psy, estimate = .pred) %>% 
    pull(.estimate) %>% 
    format(digits = 3)
psy_rsq <- psy_res_df %>% 
    rsq(truth = g_psy, estimate = .pred) %>% 
    pull(.estimate) %>% 
    format(digits = 3)


first_annotation <- glue("italic(r)=={format(psy_r, digits = 3)}*','~italic(p)<0.001")
second_annotation <- glue("R^2=={psy_rsq}*','~MAE=={psy_mae}*','~RMSE=={psy_rmse}")

scatter_psy_figure <- psy_res_df %>% 
    ggplot(aes(x = g_psy, y = .pred)) +
    geom_jitter(width = 0.1, size = 3, color = "grey30", alpha = 0.9) +
    geom_smooth(method = "lm", color = "tomato3", fill = "grey80") +
    geom_abline(slope = 1, lty = 2, color = "grey60") +
    labs(x = "Empirical Score", y = "Predicted Score", 
         title = "General Psychopathology  (All Features)") +
    scale_x_continuous(limits = c(-2.5, 5)) + 
    scale_y_continuous(limits = c(-2.5, 5)) +
    annotate(geom = "text",
             x = -2.5,
             y = 4.8,
             label = first_annotation,
             color = "gray20",
             hjust = 0,
             parse = TRUE) +
    annotate(geom = "text",
             x = -2.5,
             y = 4.4,
             label = second_annotation,
             color = "gray20",
             hjust = 0,
             parse = TRUE) +
    theme_pander() +
    theme(plot.margin = margin(2, 2, 2, 2, "mm"))

ggsave(filename = here("outputs", "figs", "model-psy_outcome-psy_desc-scatter.pdf"), 
       scatter_psy_figure,
       width = 6,
       height = 5)


rmse_list <- NULL
for (ith_param in 1:273) {
    params <- psy_sorted_feature_param[1:ith_param]
    features <- psy_sorted_feature_names[1:ith_param]
    
    tmp_df <- select(baked_psy_holdout_df, all_of(features))
    
    rmse_list[ith_param] <- (psy_intercept + as.matrix(tmp_df) %*% params) %>% 
        as_tibble(.name_repair = ~".pred") %>% 
        bind_cols(baked_psy_holdout_df %>% select(g_psy)) %>% 
        rmse(truth = g_psy, estimate = .pred) %>%
        pull(.estimate)
}

psy_feature_performance_figure <- as_tibble(list(x = 1:273, y = rmse_list)) %>% 
    ggplot(aes(x = x, y = y)) +
    geom_point(size = 3, color = "grey30", alpha = 0.8) +
    scale_x_continuous(breaks = seq(0, 300, 50)) +
    labs(x = "Number of Features (Sorted by Importance)", y = "RMSE",
         title = "General Psychopathology") +
    theme_pander() +
    theme(plot.margin = margin(2, 2, 2, 2, "mm"))
ggsave(filename = here("outputs", "figs", "model-psy_outcome-psy_desc-feature_performance.pdf"), 
       psy_feature_performance_figure,
       width = 6,
       height = 5)


# Performance Aggr:Aggr --------------------------------------------------------
params_aggr_df_mean <- params_aggr_df %>% 
    rowwise() %>%
    mutate(
        b_mean = mean(c_across(starts_with("iter")), na.rm = TRUE), 
        b_sd = sd(c_across(starts_with("iter")), na.rm = TRUE), 
        .after = variable) %>% 
    ungroup() %>% 
    select(-starts_with("iter"))

aggr_sorted_df <- vi_aggr_df %>% 
    rowwise() %>%
    mutate(
        vi_mean = mean(c_across(starts_with("iter")), na.rm = TRUE), 
        vi_sd = sd(c_across(starts_with("iter")), na.rm = TRUE), 
        .after = variable) %>% 
    ungroup() %>% 
    arrange(desc(abs(vi_mean))) %>% 
    select(-starts_with("iter")) %>% 
    left_join(params_aggr_df_mean, by = "variable")


aggr_sorted_feature_param <- pull(aggr_sorted_df, b_mean)
aggr_sorted_feature_names <- pull(aggr_sorted_df, variable)
write_rds(aggr_sorted_feature_names, here("outputs", "cache", "aggr_sorted_feature_names.rds"))


preped_aggr_recipe <- discovery_df %>%
    select(-g_psy) %>% 
    recipe(bpaq_tot ~ .) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_numeric_predictors()) %>% 
    step_center(all_numeric_predictors()) %>% 
    step_scale(all_numeric_predictors()) %>% 
    step_impute_knn(all_numeric_predictors(), neighbors = 20) %>% 
    prep()

baked_aggr_holdout_df <- holdout_df %>% 
    select(-g_psy) %>%
    bake(preped_aggr_recipe, new_data = .)

feature_aggr_holdout_df <- select(baked_aggr_holdout_df, all_of(aggr_sorted_feature_names))

agg_res_df <- (aggr_intercept + as.matrix(feature_aggr_holdout_df) %*% aggr_sorted_feature_param) %>% 
    as_tibble(.name_repair = ~".pred") %>% 
    bind_cols(baked_aggr_holdout_df %>% select(bpaq_tot))

agg_r <- agg_res_df %>%
    correlation::correlation() %>% 
    pluck("r")
agg_p <- agg_res_df %>%
    correlation::correlation() %>% 
    pluck("p")
agg_rmse <- agg_res_df %>% 
    rmse(truth = bpaq_tot, estimate = .pred) %>%
    pull(.estimate) %>% 
    format(digits = 3)
agg_mae <- agg_res_df %>% 
    mae(truth = bpaq_tot, estimate = .pred) %>% 
    pull(.estimate) %>% 
    format(digits = 3)
agg_rsq <- agg_res_df %>% 
    rsq(truth = bpaq_tot, estimate = .pred) %>% 
    pull(.estimate) %>% 
    format(digits = 3)


first_annotation <- glue("italic(r)=={format(agg_r, digits = 3)}*','~italic(p)<0.001")
second_annotation <- glue("R^2=={agg_rsq}*','~MAE=={agg_mae}*','~RMSE=={agg_rmse}")

scatter_aggr_figure <- agg_res_df %>% 
    ggplot(aes(x = bpaq_tot, y = .pred)) +
    geom_jitter(width = 0.1, size = 3, color = "grey30", alpha = 0.9) +
    geom_smooth(method = "lm", color = "tomato3", fill = "grey80") +
    geom_abline(slope = 1, lty = 2, color = "grey60") +
    labs(x = "Empirical Score", y = "Predicted Score", title = "Aggression (All Features)") +
    scale_x_continuous(limits = c(2.3, 4)) + 
    scale_y_continuous(limits = c(2.3, 4)) +
    annotate(geom = "text",
             x = 2.35,
             y = 3.95,
             label = first_annotation,
             color = "gray20",
             hjust = 0,
             parse = TRUE) +
    annotate(geom = "text",
             x = 2.35,
             y = 3.85,
             label = second_annotation,
             color = "gray20",
             hjust = 0,
             parse = TRUE) +
    theme_pander() +
    theme(plot.margin = margin(2, 2, 2, 2, "mm"))

ggsave(filename = here("outputs", "figs", "model-aggr_outcome-aggr_desc-scatter.pdf"), 
       scatter_aggr_figure,
       width = 6,
       height = 5)



rmse_list <- NULL
for (ith_param in 1:273) {
    params <- aggr_sorted_feature_param[1:ith_param]
    features <- aggr_sorted_feature_names[1:ith_param]
    
    tmp_df <- select(baked_aggr_holdout_df, all_of(features))
    
    rmse_list[ith_param] <- (aggr_intercept + as.matrix(tmp_df) %*% params) %>% 
        as_tibble(.name_repair = ~".pred") %>% 
        bind_cols(baked_aggr_holdout_df %>% select(bpaq_tot)) %>% 
        rmse(truth = bpaq_tot, estimate = .pred) %>%
        pull(.estimate)
}

aggr_feature_performance_figure <- as_tibble(list(x = 1:273, y = rmse_list)) %>% 
    ggplot(aes(x = x, y = y)) +
    geom_point(size = 3, color = "grey30", alpha = 0.8) +
    scale_x_continuous(breaks = seq(0, 300, 50)) +
    labs(x = "Number of Features (Sorted by Importance)", y = "RMSE",
         title = "Aggression") +
    theme_pander() +
    theme(plot.margin = margin(2, 2, 2, 2, "mm"))
ggsave(filename = here("outputs", "figs", "model-aggr_outcome-aggr_desc-feature_performance.pdf"), 
       aggr_feature_performance_figure,
       width = 6,
       height = 5)



# Performance Overlapped Features ----------------------------------------------
overlapped_features <- intersect(
    psy_sorted_feature_names[1:75], 
    aggr_sorted_feature_names[1:75]
)

psy_selected_feature_param <- psy_sorted_df %>% 
    filter(variable %in% overlapped_features) %>% 
    pull(b_mean)

selected_feature_psy_holdout_df <- select(baked_psy_holdout_df, all_of(overlapped_features))

psy_overlapped_featureres_df <- 
    (psy_intercept + as.matrix(selected_feature_psy_holdout_df) %*% psy_selected_feature_param) %>% 
    as_tibble(.name_repair = ~".pred") %>% 
    bind_cols(baked_psy_holdout_df %>% select(g_psy))

psy_r <- psy_overlapped_featureres_df %>%
    correlation::correlation() %>% 
    pluck("r")
psy_p <- psy_overlapped_featureres_df %>%
    correlation::correlation() %>% 
    pluck("p")
psy_rmse <- psy_overlapped_featureres_df %>% 
    rmse(truth = g_psy, estimate = .pred) %>%
    pull(.estimate) %>% 
    format(digits = 3)
psy_mae <- psy_overlapped_featureres_df %>% 
    mae(truth = g_psyt, estimate = .pred) %>% 
    pull(.estimate) %>% 
    format(digits = 3)
psy_rsq <- psy_overlapped_featureres_df %>% 
    rsq(truth = g_psy, estimate = .pred) %>% 
    pull(.estimate) %>% 
    format(digits = 3)

first_annotation <- glue("italic(r)=={format(psy_r, digits = 3)}*','~italic(p)<0.001")
second_annotation <- glue("R^2=={psy_rsq}*','~MAE=={psy_mae}*','~RMSE=={psy_rmse}")

scatter_psy_overlapped_features_figure <- psy_overlapped_featureres_df %>% 
    ggplot(aes(x = g_psy, y = .pred)) +
    geom_jitter(width = 0.1, size = 3, color = "grey30", alpha = 0.9) +
    geom_smooth(method = "lm", color = "tomato3", fill = "grey80") +
    #geom_abline(slope = 1, lty = 2, color = "grey60") +
    labs(x = "Empirical Score", y = "Predicted Score", 
         title = "General Psychopathology (Only Overlapped Features)") +
    scale_x_continuous(limits = c(-2.5, 5)) + 
    scale_y_continuous(limits = c(-1, 2)) +
    annotate(geom = "text",
             x = -2.5,
             y = 1.9,
             label = first_annotation,
             color = "gray20",
             hjust = 0,
             parse = TRUE) +
    annotate(geom = "text",
             x = -2.5,
             y = 1.75,
             label = second_annotation,
             color = "gray20",
             hjust = 0,
             parse = TRUE) +
    theme_pander() +
    theme(plot.margin = margin(2, 2, 2, 2, "mm"))
ggsave(filename = here("outputs", "figs", "model-psy_outcome-psy_desc-overlapped_feature_scatter.pdf"), 
       scatter_psy_overlapped_features_figure,
       width = 6,
       height = 5)


aggr_selected_feature_param <- aggr_sorted_df %>% 
    filter(variable %in% overlapped_features) %>% 
    pull(b_mean)

selected_feature_aggr_holdout_df <- select(baked_aggr_holdout_df, all_of(overlapped_features))

agg_overlapped_featureres_df <- 
    (aggr_intercept + as.matrix(selected_feature_aggr_holdout_df) %*% aggr_selected_feature_param) %>% 
    as_tibble(.name_repair = ~".pred") %>% 
    bind_cols(agg_overlapped_featureres_df %>% select(bpaq_tot))

agg_r <- agg_overlapped_featureres_df %>%
    correlation::correlation() %>% 
    pluck("r")
agg_p <- agg_overlapped_featureres_df %>%
    correlation::correlation() %>% 
    pluck("p")
agg_rmse <- agg_overlapped_featureres_df %>% 
    rmse(truth = bpaq_tot, estimate = .pred) %>%
    pull(.estimate) %>% 
    format(digits = 3)
agg_mae <- agg_overlapped_featureres_df %>% 
    mae(truth = bpaq_tot, estimate = .pred) %>% 
    pull(.estimate) %>% 
    format(digits = 3)
agg_rsq <- agg_overlapped_featureres_df %>% 
    rsq(truth = bpaq_tot, estimate = .pred) %>% 
    pull(.estimate) %>% 
    format(digits = 3)

first_annotation <- glue("italic(r)=={format(agg_r, digits = 3)}*','~italic(p)<0.001")
second_annotation <- glue("R^2=={agg_rsq}*','~MAE=={agg_mae}*','~RMSE=={agg_rmse}")

scatter_aggr_overlapped_features_figure <- agg_overlapped_featureres_df %>% 
    ggplot(aes(x = bpaq_tot, y = .pred)) +
    geom_jitter(width = 0.1, size = 3, color = "grey30", alpha = 0.9) +
    geom_smooth(method = "lm", color = "tomato3", fill = "grey80") +
    #geom_abline(slope = 1, lty = 2, color = "grey60") +
    labs(x = "Empirical Score", y = "Predicted Score", title = "Aggression (Only Overlapped Features)") +
    scale_x_continuous(limits = c(2.3, 4)) + 
    scale_y_continuous(limits = c(2.7, 3.4)) +
    annotate(geom = "text",
             x = 2.35,
             y = 3.375,
             label = first_annotation,
             color = "gray20",
             hjust = 0,
             parse = TRUE) +
    annotate(geom = "text",
             x = 2.35,
             y = 3.34,
             label = second_annotation,
             color = "gray20",
             hjust = 0,
             parse = TRUE) +
    theme_pander() +
    theme(plot.margin = margin(2, 2, 2, 2, "mm"))
ggsave(filename = here("outputs", "figs", "model-aggr_outcome-aggr_desc-overlapped_feature_scatter.pdf"), 
       scatter_aggr_overlapped_features_figure,
       width = 6,
       height = 5)
 