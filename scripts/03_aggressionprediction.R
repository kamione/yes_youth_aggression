# Environment ------------------------------------------------------------------
library(tidyverse)
library(caret)
library(here)
library(glmnet)
library(kernlab)
library(ROCR)
library(randomForest)
library(tidymodels)
library(doParallel)
library(probably)
library(vip)
library(ranger)
library(klaR)
library(glue)
library(ggpubr)
library(keras)
library(ggthemes)


# fix the conflict
select <-  dplyr::select

# Data IO  ---------------------------------------------------------------------
data <- here("data", "processed", "yes_baseline_outcome-aggression_n-2215_p-293.csv") %>% 
    read_csv(col_types = cols()) %>% 
    mutate(BPAQ_tot = log(rowSums(across(BPAQ_1:BPAQ_12))), .before = BPAQ_1) %>% 
    #mutate(BPAQ_phy = rowSums(across(BPAQ_1:BPAQ_3)), .before = BPAQ_1) %>% 
    #mutate(BPAQ_verb = rowSums(across(BPAQ_4:BPAQ_6)), .before = BPAQ_1) %>% 
    #mutate(BPAQ_anger = rowSums(across(BPAQ_7:BPAQ_9)), .before = BPAQ_1) %>% 
    #mutate(BPAQ_host = rowSums(across(BPAQ_10:BPAQ_12)), .before = BPAQ_1) %>% 
    select(-c(BPAQ_1:BPAQ_12))



# remove all registered parallel workers
unregister <- function() {
    env <- foreach:::.foreachGlobals
    rm(list = ls(name=env), pos = env)
}


# Data Split -------------------------------------------------------------------
set.seed(1234)
splits <- initial_split(data, prop = 3/4, stata = BPAQ_tot)
data_tr <- training(splits)
data_te <- testing(splits)

set.seed(1234)
data_tr_cv <- vfold_cv(data_tr, v = 5, repeats = 10, strata = BPAQ_tot)

master_recipe <- recipe(BPAQ_tot ~ ., data = data_tr) %>% 
    step_zv(all_predictors()) %>% 
    step_center(all_predictors()) %>% 
    step_scale(all_predictors()) %>% 
    step_impute_knn(all_predictors(), neighbors = 20)

master_lambda_grid <- grid_regular(penalty(), levels = 100)


# model development
lasso_mod_spec <- 
    linear_reg(penalty = tune(), mixture = 1) %>% 
    set_engine("glmnet") %>% 
    set_mode("regression")

lasso_workflow <- 
    workflow() %>% 
    add_model(lasso_mod_spec) %>% 
    add_recipe(master_recipe)

# hyperparameter tuning
cluster <- parallel::makeCluster(8)
registerDoParallel(cluster)
lasso_fit_tr <- lasso_workflow %>% 
    tune_grid(grid = master_lambda_grid,
              resamples = data_tr_cv,
              control = control_grid(save_pred = TRUE),
              metrics = metric_set(rmse, rsq, mae))
stopCluster(cluster)
unregister()

# plot lasso performance grid


lasso_performance_cv_fig <- lasso_fit_tr %>%
    collect_metrics() %>%
    ggplot(aes(penalty, mean, color = .metric)) +
        geom_errorbar(aes(
            ymin = mean - std_err,
            ymax = mean + std_err
        ), alpha = 0.5) +
        geom_line(size = 1.5) +
        facet_wrap(~.metric, scales = "free", nrow = 3) +
        scale_x_log10() +
        theme_pander() +
        theme(legend.position = "none")
ggexport(lasso_performance_cv_fig,
         filename = here("outputs", "figs", "lasso_performance_cv.pdf"),
         width = 5,
         height = 6)


lasso_best_lambda <- lasso_fit_tr %>% select_best("rmse")

# model testing
lasso_workflow_sel_param <- lasso_workflow %>%
    finalize_workflow(lasso_best_lambda)


lasso_fit_te <- lasso_workflow_sel_param %>%
    last_fit(splits)

lasso_y_te_pred <- lasso_fit_te %>% 
    collect_predictions()

lasso_test_res <- bind_cols(lasso_y_te_pred %>% select(.pred), data_te %>% select(BPAQ_tot))

lasso_rmse <- rmse(lasso_test_res, truth = BPAQ_tot, estimate = .pred)
lasso_rsq <- rsq(lasso_test_res, truth = BPAQ_tot, estimate = .pred)
lasso_mae <- mae(lasso_test_res, truth = BPAQ_tot, estimate = .pred)

lasso_pred_cor <- cor.test(lasso_test_res$BPAQ_tot, lasso_test_res$.pred)
first_annotation <- glue("italic(r)=={format(lasso_pred_cor$estimate, digits = 3)}*','~italic(p)<0.001")
second_annotation <- glue("R^2=={format(lasso_rsq$.estimate, digits = 3)}*','~MAE=={format(lasso_mae$.estimate, digits = 4)}")


lasso_fig <- ggplot(lasso_test_res, aes(x = BPAQ_tot, y = .pred)) + 
    geom_abline(lty = 3) + 
    geom_point(size = 2.5, position = position_jitter(w = 0.1, h = 0), alpha = 0.5) + 
    geom_smooth(method = "lm", color = "tomato3") +
    xlim(2.35, 3.85) +
    ylim(2.35, 3.85) +
    annotate(geom = "text",
             x = 2.35,
             y = 3.75,
             label = first_annotation,
             color = "gray20",
             hjust = 0,
             parse = TRUE) +
    annotate(geom = "text",
             x = 2.35,
             y = 3.65,
             label = second_annotation,
             color = "gray20",
             hjust = 0,
             parse = TRUE) +
    labs(x = "Empirical Score", y = "Predicted Score", title = "LASSO") +
    theme_pander() +
    theme(plot.margin = unit(rep(2, 4), "mm"))
ggsave(lasso_fig, filename = here("outputs", "figs", "lasso_bpaq_tot_pred_perf.pdf"),
       width = 5, height = 4.5)

# Feature Importance -----------------------------------------------------------
top30 <- lasso_fit_te %>% 
    pluck(".workflow", 1) %>%   
    extract_fit_parsnip() %>% 
    vip(num_features = 30, geom = "point") +
    theme_pander() +
    theme(plot.margin = unit(rep(4, 4), "mm"))
ggsave(top30,
       filename = here("outputs", "figs", "lasso_bpaq_tot_pred_vip_top30.pdf"),
       width = 4.5, height = 6)

top30$data %>%
    select(Variable) %>% 
    write_csv(here("data", "processed", "lasso_top30_variables.csv"))

top20 <- lasso_fit_te %>% 
    pluck(".workflow", 1) %>%   
    extract_fit_parsnip() %>% 
    vip(num_features = 20, geom = "point") +
    theme_pander() +
    theme(plot.margin = unit(rep(5, 4), "mm"))
ggsave(top20,
       filename = here("outputs", "figs", "lasso_bpaq_tot_pred_vip_top20.pdf"),
       width = 4, height = 6)

top20$data %>%
    select(Variable) %>% 
    write_csv(here("data", "processed", "lasso_top20_variables.csv"))

