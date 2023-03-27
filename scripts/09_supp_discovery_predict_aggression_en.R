# Environment ------------------------------------------------------------------
library(tidyverse)
library(here)
library(tidymodels)
library(doParallel)
library(vip)
library(glue)
library(progress)

# fix the conflict
select <- dplyr::select

source(here("src", "R", "utilities.R"))


# Data IO  ---------------------------------------------------------------------
psychopathology_list <- c("PHQ9_tot", "YMRS_tot", "HCL32_tot", "GAD7_tot",
                          "LSAS_Tot", "YBOCS_Sym", "Cape_distress_tot",
                          "PQB_tot")

discovery_df <- here("data", "processed", "discovery_dataset.rds") %>% 
    read_rds() %>% 
    select(-c(Parti_ID, bpaq_tot:bpaq_host, g_psy, all_of(psychopathology_list)))
holdout_df <- here("data", "processed", "holdout_dataset.rds") %>% 
    read_rds() %>% 
    select(-c(Parti_ID, bpaq_tot:bpaq_host, g_psy, all_of(psychopathology_list)))


# Elastic Net ------------------------------------------------------------------
# create empty data frame for results
repeat_var <- paste0("iter_", str_pad(1:100, 4, pad = "0"))
metrics_df <- matrix(nrow = 100, ncol = 6) %>%
    as_tibble(.name_repair = ~c("lambda", "mixture", "rmse", "mae", "r", "rsq")) %>% 
    mutate(iteration = repeat_var, .before = lambda)

n_feature <- discovery_df %>% 
    select(-bpaq_scaled) %>% 
    colnames() %>% 
    length()

vi_df <- matrix(nrow = n_feature, ncol = 100) %>%
    as_tibble(.name_repair = ~repeat_var) %>%
    mutate(variable = NA, .before = iter_0001)

param_df <- vi_df %>% add_row() # for intercept

# set up text progress bar
pb <- progress_bar$new(
    format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
    total = 100,
    complete = "=",
    incomplete = "-",
    current = ">", 
    clear = FALSE,
    width = 100
)

for (iter in 1:100) {
    # update progress bar
    pb$tick()
    
    set.seed(iter)
    splits <- initial_split(discovery_df, prop = 3/4, stata = bpaq_scaled)
    data_tr <- training(splits)
    data_te <- testing(splits)
    
    set.seed(iter)
    data_tr_cv <- vfold_cv(data_tr, v = 10, strata = bpaq_scaled)
    
    master_recipe <- recipe(bpaq_scaled ~ ., data = data_tr) %>% 
        step_dummy(all_nominal_predictors()) %>% # pls make sure dummy variable
        step_zv(all_numeric_predictors()) %>% 
        step_center(all_numeric_predictors()) %>% 
        step_scale(all_numeric_predictors()) %>% 
        step_impute_knn(all_numeric_predictors(), neighbors = 20)
    
    master_grid <- grid_regular(
        penalty(), mixture(),
        levels = list(penalty = 100, mixture = 10)
    )
    
    # model development
    en_mod_spec <- linear_reg(penalty = tune(), mixture = tune()) %>% 
        set_engine("glmnet") %>% 
        set_mode("regression")
    
    en_workflow <- workflow() %>% 
        add_model(en_mod_spec) %>% 
        add_recipe(master_recipe)
    
    # hyperparameter tuning
    mycluster <- parallel::makeCluster(8)
    registerDoParallel(mycluster)
    en_fit_tr <- en_workflow %>% 
        tune_grid(grid = master_grid,
                  resamples = data_tr_cv,
                  control = control_grid(save_pred = TRUE),
                  metrics = metric_set(rmse))
    stopCluster(mycluster)
    unregister()
    
    en_best_hyperparameter <- select_best(en_fit_tr, "rmse")
    
    metrics_df[iter, "lambda"] <- pull(en_best_hyperparameter, penalty)
    metrics_df[iter, "mixture"] <- pull(en_best_hyperparameter, mixture)
    
    # model testing
    en_fit_te <- en_workflow %>%
        finalize_workflow(en_best_hyperparameter) %>% 
        last_fit(splits)
    en_y_te_pred <- collect_predictions(en_fit_te)
    en_test_res <- en_y_te_pred %>% 
        select(.pred) %>% 
        bind_cols(select(data_te, bpaq_scaled))
    
    # write metrics
    metrics_df[iter, "rmse"] <- en_test_res %>% 
        rmse(truth = bpaq_scaled, estimate = .pred) %>% 
        pull(.estimate)
    metrics_df[iter, "mae"] <- en_test_res %>% 
        mae(truth = bpaq_scaled, estimate = .pred) %>%
        pull(.estimate)
    metrics_df[iter, "r"] <- en_test_res %>% 
        correlation::correlation() %>% 
        pluck("r")
    metrics_df[iter, "rsq"] <- en_test_res %>% 
        rsq(truth = bpaq_scaled, estimate = .pred) %>% 
        pull(.estimate)
    
    # get model importance based on test data
    vi_df[, c(1, iter + 1)] <- en_fit_te %>% 
        pluck(".workflow", 1) %>%   
        extract_fit_parsnip() %>% 
        vi_model() %>% 
        mutate(Sign = if_else(Sign == "POS", 1, -1)) %>% 
        summarize(
            varaible = Variable,
            importance = Importance * Sign
        )
    
    # get model parameters from training data for further use
    param_df[, c(1, iter + 1)] <- en_workflow %>% 
        finalize_workflow(en_best_hyperparameter) %>%
        fit(data_tr) %>%
        extract_fit_parsnip() %>%
        tidy() %>% 
        select(term, estimate)
}

write_rds(metrics_df, here("outputs", "cache", "model-aggr_algo-en_desc-metrics.rds"))
#metrics_df <- read_rds(here("outputs", "cache", "model-aggr_algo-en_desc-metrics.rds"))
write_rds(vi_df, here("outputs", "cache", "model-aggr_algo-en_desc-model_importance.rds"))
#vi_df <- read_rds(here("outputs", "cache", "model-aggr_algo-en_desc-model_importance.rds"))
write_rds(param_df, here("outputs", "cache", "model-aggr_algo-en_desc-model_parameters.rds"))
#param_df <- read_rds(here("outputs", "cache", "model-aggr_algo-en_desc-model_parameters.rds"))

