# Environment ------------------------------------------------------------------
library(tidyverse)
library(here)
library(tidymodels)
library(progress)
library(ggpubr)
library(ggthemes)
library(gghighlight)
library(glue)

source(here("src", "R", "utilities.R"))

# Data IO  ---------------------------------------------------------------------
psychopathology_list <- c("PHQ9_tot", "YMRS_tot", "HCL32_tot", "GAD7_tot",
                          "LSAS_Tot", "YBOCS_Sym", "Cape_distress_tot",
                          "PQB_tot")

discovery_df <- here("data", "processed", "discovery_dataset.rds") %>% 
    read_rds() %>% 
    select(-c(Parti_ID, bpaq_tot:bpaq_host, all_of(psychopathology_list)))
holdout_df <- here("data", "processed", "holdout_dataset.rds") %>% 
    read_rds() %>% 
    select(-c(Parti_ID, bpaq_tot:bpaq_host, all_of(psychopathology_list)))

params_psy_df <- 
    here("outputs", "cache", "model-psy_desc-model_parameters.rds") %>% 
    read_rds()
params_aggr_df <- 
    here("outputs", "cache", "model-aggr_desc-model_parameters.rds") %>% 
    read_rds()
vi_psy_df <- 
    here("outputs", "cache", "model-psy_desc-model_importance.rds") %>% 
    read_rds()
vi_aggr_df <- 
    here("outputs", "cache", "model-aggr_desc-model_importance.rds") %>% 
    read_rds()

n_features <- nrow(params_psy_df) - 1

psy_intercept <- params_psy_df %>% 
    slice(1) %>% 
    mutate(mean = mean(c_across(starts_with("iter")), na.rm = TRUE),
           .after = variable) %>% 
    pull(mean)

aggr_intercept <- params_aggr_df %>% 
    slice(1) %>% 
    mutate(mean = mean(c_across(starts_with("iter")), na.rm = TRUE),
           .after = variable) %>% 
    pull(mean)


# Performance: Psy -> Psy ------------------------------------------------------
params_psy_df_mean <- params_psy_df %>% 
    rowwise() %>%
    mutate(
        b_mean = mean(c_across(starts_with("iter")), na.rm = TRUE), 
        b_sd = sd(c_across(starts_with("iter")), na.rm = TRUE), 
        .after = variable
    ) %>% 
    mutate(
        
    ) %>% 
    ungroup() %>% 
    select(-starts_with("iter"))
params_psy_df_mean$lower_ci <- NA
params_psy_df_mean$upper_ci <- NA
params_psy_df_mean$exclude <- NA

for (ith_feat in 1:n_features + 1) {
    ci <- params_psy_df %>% 
        slice(ith_feat) %>% 
        select(-variable) %>% 
        as.numeric() %>% 
        confidence_interval(interval = 0.95)
    params_psy_df_mean[ith_feat, "lower_ci"] <- ci[1]
    params_psy_df_mean[ith_feat, "upper_ci"] <- ci[2]
    params_psy_df_mean[ith_feat, "exclude"] <- if_else(
        ci[1] * ci[2] > 0, 0, 1
    )
}

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

psy_sorted_feature_param <- psy_sorted_df %>% 
    filter(exclude == 0) %>% 
    pull(b_mean)
psy_sorted_feature_names <- psy_sorted_df %>% 
    filter(exclude == 0) %>% 
    pull(variable)
write_rds(psy_sorted_feature_names, 
          here("outputs", "cache", "psy_sorted_feature_names.rds"))

preped_psy_recipe <- discovery_df %>%
    select(-bpaq_scaled) %>% 
    recipe(g_psy ~ .) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_numeric_predictors()) %>% 
    step_center(all_numeric_predictors()) %>% 
    step_scale(all_numeric_predictors()) %>% 
    step_impute_knn(all_numeric_predictors(), neighbors = 20) %>% 
    prep()

baked_psy_holdout_df <- holdout_df %>% 
    select(-bpaq_scaled) %>%
    bake(preped_psy_recipe, new_data = .)

feature_psy_holdout_df <- select(baked_psy_holdout_df, all_of(psy_sorted_feature_names))

psy_res_df <- 
    (psy_intercept + as.matrix(feature_psy_holdout_df) %*% psy_sorted_feature_param) %>% 
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
    labs(
        x = "Empirical Score", 
        y = "Predicted Score", 
        title = "General Psychopathology  (All Features)"
    ) +
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
scatter_psy_figure

ggsave(filename = here("outputs", "figs", "model-psy_outcome-psy_desc-scatter.pdf"), 
       scatter_psy_figure,
       width = 6,
       height = 5)


# Performance: Aggr -> Aggr ----------------------------------------------------
params_aggr_df_mean <- params_aggr_df %>% 
    rowwise() %>%
    mutate(
        b_mean = mean(c_across(starts_with("iter")), na.rm = TRUE), 
        b_sd = sd(c_across(starts_with("iter")), na.rm = TRUE), 
        .after = variable) %>% 
    ungroup() %>% 
    select(-starts_with("iter"))
params_aggr_df_mean$lower_ci <- NA
params_aggr_df_mean$upper_ci <- NA
params_aggr_df_mean$exclude <- NA

for (ith_feat in 1:n_features + 1) {
    ci <- params_aggr_df %>% 
        slice(ith_feat) %>% 
        select(-variable) %>% 
        as.numeric() %>% 
        confidence_interval(interval = 0.95)
    params_aggr_df_mean[ith_feat, "lower_ci"] <- ci[1]
    params_aggr_df_mean[ith_feat, "upper_ci"] <- ci[2]
    params_aggr_df_mean[ith_feat, "exclude"] <- if_else(
        ci[1] * ci[2] > 0, 0, 1
    )
}

aggr_sorted_df <- vi_aggr_df %>% 
    rowwise() %>%
    mutate(
        vi_mean = mean(c_across(starts_with("iter")), na.rm = TRUE), 
        vi_sd = sd(c_across(starts_with("iter")), na.rm = TRUE), 
        .after = variable
    ) %>% 
    ungroup() %>% 
    arrange(desc(abs(vi_mean))) %>% 
    select(-starts_with("iter")) %>% 
    left_join(params_aggr_df_mean, by = "variable")

aggr_sorted_feature_param <- aggr_sorted_df %>% 
    filter(exclude == 0) %>% 
    pull(b_mean)
aggr_sorted_feature_names <- aggr_sorted_df %>% 
    filter(exclude == 0) %>% 
    pull(variable)
write_rds(aggr_sorted_feature_names, here("outputs", "cache", "aggr_sorted_feature_names.rds"))

preped_aggr_recipe <- discovery_df %>%
    select(-g_psy) %>% 
    recipe(bpaq_scaled ~ .) %>% 
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
    bind_cols(baked_aggr_holdout_df %>% select(bpaq_scaled))

agg_r <- agg_res_df %>%
    correlation::correlation() %>% 
    pluck("r")
agg_p <- agg_res_df %>%
    correlation::correlation() %>% 
    pluck("p")
agg_rmse <- agg_res_df %>% 
    rmse(truth = bpaq_scaled, estimate = .pred) %>%
    pull(.estimate) %>% 
    format(digits = 3)
agg_mae <- agg_res_df %>% 
    mae(truth = bpaq_scaled, estimate = .pred) %>% 
    pull(.estimate) %>% 
    format(digits = 3)
agg_rsq <- agg_res_df %>% 
    rsq(truth = bpaq_scaled, estimate = .pred) %>% 
    pull(.estimate) %>% 
    format(digits = 3)

first_annotation <- glue("italic(r)=={format(agg_r, digits = 3)}*','~italic(p)<0.001")
second_annotation <- glue("R^2=={agg_rsq}*','~MAE=={agg_mae}*','~RMSE=={agg_rmse}")

scatter_aggr_figure <- agg_res_df %>% 
    ggplot(aes(x = bpaq_scaled, y = .pred)) +
    geom_jitter(width = 0.1, size = 3, color = "grey30", alpha = 0.9) +
    geom_smooth(method = "lm", color = "tomato3", fill = "grey80") +
    geom_abline(slope = 1, lty = 2, color = "grey60") +
    labs(
        x = "Empirical Score", 
        y = "Predicted Score", 
        title = "Proneness to Aggression (All Features)"
    ) +
    scale_x_continuous(limits = c(-1.5, 4.3)) + 
    scale_y_continuous(limits = c(-1.5, 4.3)) +
    annotate(geom = "text",
             x = -1.4,
             y = 3.9,
             label = first_annotation,
             color = "gray20",
             hjust = 0,
             parse = TRUE) +
    annotate(geom = "text",
             x = -1.4,
             y = 3.6,
             label = second_annotation,
             color = "gray20",
             hjust = 0,
             parse = TRUE) +
    theme_pander() +
    theme(plot.margin = margin(2, 2, 2, 2, "mm"))
scatter_aggr_figure

ggsave(filename = here("outputs", "figs", "model-aggr_outcome-aggr_desc-scatter.pdf"), 
       scatter_aggr_figure,
       width = 6,
       height = 5)


# Overlapping Non-Zero Feature Prediction Performance --------------------------
overlapping_features <- intersect(
    psy_sorted_feature_names, aggr_sorted_feature_names
)
cat(glue("A total of {length(overlapping_features)} features is selected"))
write_rds(overlapping_features, here("data", "processed", "overlapping_features.rds"))

psy_feat_idx <- which(psy_sorted_feature_names %in% overlapping_features)
aggr_feat_idx <- which(aggr_sorted_feature_names %in% overlapping_features)

# psychopathology -> psychopathology
psy_using_psy_res_df <- 
    (psy_intercept + 
         as.matrix(feature_psy_holdout_df %>% 
                       select(psy_sorted_feature_names[psy_feat_idx])) %*% 
         psy_sorted_feature_param[psy_feat_idx]
    ) %>% 
    as_tibble(.name_repair = ~".pred") %>% 
    bind_cols(baked_psy_holdout_df %>% select(g_psy))

psy_using_psy_res_df %>% correlation::correlation() %>% pluck("r")
psy_using_psy_res_df %>% correlation::correlation() %>% pluck("p")
psy_using_psy_res_df %>% 
    rsq(truth = g_psy, estimate = .pred) %>%
    pull(.estimate) %>% 
    format(digits = 3)
psy_using_psy_res_df %>% 
    mae(truth = g_psy, estimate = .pred) %>%
    pull(.estimate) %>% 
    format(digits = 3)
psy_using_psy_res_df %>% 
    rmse(truth = g_psy, estimate = .pred) %>%
    pull(.estimate) %>% 
    format(digits = 3)

# Proneness to aggression -> Proneness to aggression
aggr_using_aggr_res_df <- 
    (aggr_intercept + 
         as.matrix(feature_aggr_holdout_df 
                   %>% select(aggr_sorted_feature_names[aggr_feat_idx])) %*% 
         aggr_sorted_feature_param[aggr_feat_idx]
    ) %>% 
    as_tibble(.name_repair = ~".pred") %>% 
    bind_cols(baked_aggr_holdout_df %>% select(bpaq_scaled))

aggr_using_aggr_res_df %>% correlation::correlation() %>% pluck("r")
aggr_using_aggr_res_df %>% correlation::correlation() %>% pluck("p")
aggr_using_aggr_res_df %>% 
    rsq(truth = bpaq_scaled, estimate = .pred) %>%
    pull(.estimate) %>% 
    format(digits = 3)
aggr_using_aggr_res_df %>% 
    mae(truth = bpaq_scaled, estimate = .pred) %>%
    pull(.estimate) %>% 
    format(digits = 3)
aggr_using_aggr_res_df %>% 
    rmse(truth = bpaq_scaled, estimate = .pred) %>%
    pull(.estimate) %>% 
    format(digits = 3)


# proneness to aggression -> psychopathology
psy_using_aggr_res_df <- 
    (psy_intercept + 
         as.matrix(feature_psy_holdout_df %>% 
                       select(aggr_sorted_feature_names[aggr_feat_idx])) %*% 
         aggr_sorted_feature_param[aggr_feat_idx]
    ) %>% 
    as_tibble(.name_repair = ~".pred") %>% 
    bind_cols(baked_psy_holdout_df %>% select(g_psy))

psy_using_aggr_res_df %>% correlation::correlation() %>% pluck("r")
psy_using_aggr_res_df %>% correlation::correlation() %>% pluck("p")
psy_using_aggr_res_df %>% 
    rsq(truth = g_psy, estimate = .pred) %>%
    pull(.estimate) %>% 
    format(digits = 3)
psy_using_aggr_res_df %>% 
    mae(truth = g_psy, estimate = .pred) %>%
    pull(.estimate) %>% 
    format(digits = 3)
psy_using_aggr_res_df %>% 
    rmse(truth = g_psy, estimate = .pred) %>%
    pull(.estimate) %>% 
    format(digits = 3)

# psychopathology -> Proneness to aggression
aggr_using_psy_res_df <- 
    (aggr_intercept + 
         as.matrix(feature_aggr_holdout_df 
                   %>% select(psy_sorted_feature_names[psy_feat_idx])) %*% 
         psy_sorted_feature_param[psy_feat_idx]
     ) %>% 
    as_tibble(.name_repair = ~".pred") %>% 
    bind_cols(baked_aggr_holdout_df %>% select(bpaq_scaled))

aggr_using_psy_res_df %>% correlation::correlation() %>% pluck("r")
aggr_using_psy_res_df %>% correlation::correlation() %>% pluck("p")
aggr_using_psy_res_df %>% 
    rsq(truth = bpaq_scaled, estimate = .pred) %>%
    pull(.estimate) %>% 
    format(digits = 3)
aggr_using_psy_res_df %>% 
    mae(truth = bpaq_scaled, estimate = .pred) %>%
    pull(.estimate) %>% 
    format(digits = 3)
aggr_using_psy_res_df %>% 
    rmse(truth = bpaq_scaled, estimate = .pred) %>%
    pull(.estimate) %>% 
    format(digits = 3)
