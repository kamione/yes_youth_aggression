# Environment ------------------------------------------------------------------
library(tidyverse)
library(here)
library(mgm)
library(qgraph)
library(ggpubr)
library(ggthemes)
library(bootnet)
library(pheatmap)
library(RColorBrewer)
library(gtsummary)
library(patchwork)

source(here("src", "R", "utilities.R"))

# Data IO  ---------------------------------------------------------------------
overlapping_features <- read_rds(
    here("data", "processed", "overlapping_features.rds")
)

fa_feature_details <- 
    here("data", "processed", "fa_features_details.csv") %>% 
    read_csv(show_col_types = FALSE)

n_features <- length(fa_feature_details$label)
print(glue::glue("A total of {n_features} is selected"))

discovery_df <- read_rds(here("data", "processed", "discovery_dataset.rds"))
holdout_df <- read_rds(here("data", "processed", "holdout_dataset.rds"))

full_df <- discovery_df %>% 
    mutate(split = "Discovery", .before = Age) %>% 
    bind_rows(holdout_df %>% mutate(split = "Holdout", .before = Age)) %>% 
    select(bpaq_tot, g_psy, all_of(overlapping_features)) %>% 
    drop_na() %>% 
    mutate(type = "No Missing")

# compare drop out and no drop out samples
comparison_df <- discovery_df %>% 
    mutate(split = "Discovery", .before = Age) %>% 
    bind_rows(holdout_df %>% mutate(split = "Holdout", .before = Age)) %>% 
    mutate(type = "Original") %>% 
    bind_rows(
        discovery_df %>% 
            mutate(split = "Discovery", .before = Age) %>% 
            bind_rows(holdout_df %>% mutate(split = "Holdout", .before = Age)) %>% 
            drop_na() %>% 
            mutate(type = "No Missing")
    )

comparison_df %>% 
    select(type, Age, Sex, Edu_TotYr, bpaq_tot, g_psy) %>%
    mutate(type = factor(type, levels = c("Original", "No Missing"))) %>% 
    tbl_summary(
        by = type,
        statistic = list(all_continuous() ~ "{mean} ({sd})"),
        digits = all_continuous() ~ c(2, 2),
        label = list(
            Edu_TotYr = "Years of Education",
            bpaq_tot = "Proneness to Aggression",
            g_psy = "General Psychopathology",
            cidi_e34 = "Aggressive Behavior (Lifetime)"
        )
    ) %>% 
    add_p() %>% 
    add_q() %>% 
    bold_p(q = TRUE) %>% 
    as_gt() %>% 
    gt::gtsave(here("outputs", "tables", "compaison-full_vs_dropout.html"))


# Dimension Reduction ----------------------------------------------------------
set.seed(1234)
parallel <- psych::fa.parallel(
    x = full_df %>% select(overlapping_features), 
    fm = "minres", 
    fa = "fa",
    n.iter = 100,
    plot = FALSE
)
parallel_fig <- plot_faparallel(parallel)
parallel_fig
ggsave(
    plot = parallel_fig, 
    filename = here("outputs", "figs", "fa_parallel_res.pdf"),
    height = 6,
    width = 12
)

fa_res <- psych::fa(
    full_df %>% select(overlapping_features), 
    nfactors = 18, 
    rotate = "oblimin", 
    fm = "minres"
)
pdf(file = here("outputs", "figs", "fa_corrplot.pdf"), width = 12, height = 12)
corrplot::corrplot(
    fa_res$scores %>% 
        as_tibble(rownames = NA) %>% 
        rename_at(vars(starts_with("MR")), ~str_replace(., "MR", "F")) %>% 
        select(paste0("F", 1:18)) %>% 
        cor(),
    col = colorRampPalette(c("navy", "white", "tomato3"))(40),
    method = "color",
    mar = rep(0, 4),
    tl.col = "#1C1C1C",
    tl.srt = 0,
    type = c("upper"),
    diag = FALSE,
    rect.col = "lightgrey",
    addCoef.col = "grey40",
    number.cex = 0.8,
    number.digits = 2
)
dev.off()

auto_transform <- function(x) {
    bn_obj <- bestNormalize::bestNormalize(x)
    transformed_x <- predict(bn_obj, x)
    return(transformed_x)
}

fa_score_df <- full_df %>% 
    select(g_psy, bpaq_tot) %>%
    bind_cols(fa_res$scores) %>% 
    rename_at(vars(starts_with("MR")), ~str_replace(., "MR", "F")) %>% 
    mutate_all(.funs = auto_transform) %>% 
    select(g_psy, bpaq_tot, paste0("F", 1:18))

plot_hist <- function(df, x) {
    hist_fig <- df %>% 
        ggplot(aes(x = !!sym(x))) + 
            geom_histogram(binwidth = 0.5, color = "gray30", fill = "gray30") +
            labs(y = "") +
            theme_pander()
    return(hist_fig)
}

hist_figs <- list()

for (variable in colnames(fa_score_df)) {
    hist_figs[[variable]] <- plot_hist(fa_score_df, variable)
}
hist_wrap_plot <- wrap_plots(hist_figs, ncol = 5, nrow = 4)
ggsave(
    plot = hist_wrap_plot, 
    filename = here("outputs", "figs", "hist_wrap_plot_transformed.pdf"), 
    width = 8,
    height = 6
)


# plot FA loading
pdf(file = here("outputs", "figs", "fa_heatmap.pdf"), width = 12, height = 14)
pheatmap(
    fa_res$loadings %>% 
        unclass() %>% 
        as_tibble(rownames = NA) %>% 
        rename_at(vars(starts_with("MR")), ~str_replace(., "MR", "F")) %>% 
        rownames_to_column() %>% 
        select(rowname, fa_feature_details$label) %>% 
        column_to_rownames("rowname"),
    color = colorRampPalette(c("navy", "white", "tomato3"))(40),
    breaks = seq(-1, 1, by = 0.05),
    display_numbers = TRUE,
    cluster_rows = TRUE,
    treeheight_row = 0,
    cluster_cols = FALSE,
    angle_col = 0,
    width = 10, 
    height = 12
)
dev.off()


# Symptom Network --------------------------------------------------------------
network_data       <- list()
network_data$data  <- fa_score_df %>% select(g_psy, bpaq_tot, fa_feature_details$label)
network_data$node  <- c("GPsy", "Aggr", fa_feature_details$label)
network_data$node_detail <- c("General Psychopathology",
                              "Proneness to Aggression",
                              fa_feature_details$details)
network_data$node_col <- c("#B5CAA0", "#B5CAA0", rep("#DAC9A6", n_features))
network_data$group <- c("1. Outcomes", "1. Outcomes", rep("2. Features", n_features))

set.seed(1234)
g <- estimateNetwork(
    network_data$data, 
    default = "ggmModSelect",
    stepwise = TRUE,
    corMethod = "cor_auto"
)

centrality_plot <- g %>%
    centralityPlot(
        print   = FALSE,
        labels  = network_data$node,
        include = "ExpectedInfluence",
        orderBy = "ExpectedInfluence"
    ) +
    scale_x_continuous(limits = c(-0.3, 1.1)) +
    labs(x = "Standardized Centrality Indices") +
    theme(axis.title = element_text(size = 8)) +
    theme_pander()
centrality_plot
ggsave(here("outputs", "figs", "data-discovery_desc-network_centrality.pdf"),
       centrality_plot, height = 6, width = 4)

# plot the graph
network_graph <- qgraph(
    g$graph,
    # layout
    layout = "spring",
    repulsion = 1.1,
    color = network_data$node_col,
    labels = network_data$node,
    groups = network_data$group,
    nodeNames = network_data$node_detail,
    # node
    vsize = 4,
    vTrans = 250,
    label.fill.vertical = 0.2,
    borders = FALSE,
    # edge
    esize = 10,
    fade = TRUE,
    # edge curvature
    curve = 0.5,
    curveAll = TRUE,
    # legend
    legend = TRUE, 
    legend.cex = 0.7,
    GLratio = 1.5,
    cut = 0,
    minimum = 0,
    theme = "TeamFortress",
    filetype = "pdf",
    filename = here("outputs", "figs", "data-discovery_desc-outcomes_features_network")
)


flow_graph <- qgraph(
    g$graph,
    # layout
    layout = "spring",
    repulsion = 1.1,
    color = network_data$node_col,
    labels = network_data$node,
    groups = network_data$group,
    #nodeNames = network_data$node_detail,
    # node
    vsize = 4,
    vTrans = 250,
    label.fill.vertical = 0.2,
    borders = FALSE,
    # edge
    esize = 10,
    fade = TRUE,
    # edge curvature
    curve = 0.5,
    curveAll = TRUE,
    # legend
    legend = FALSE, 
    cut = 0,
    minimum = 0,
    theme = "TeamFortress",
    mar = c(10, 10, 10, 10)
)
flow(
    flow_graph, 
    from = 1,
    filetype = "pdf",
    filename = here("outputs", "figs", "data-discovery_desc-gpsy_flow_network")
)
flow(
    flow_graph, 
    from = 2,
    filetype = "pdf",
    filename = here("outputs", "figs", "data-discovery_desc-aggr_flow_network")
)

# Stability --------------------------------------------------------------------

# check stability of centrality
g_bs_case <- bootnet(
    g,
    statistics = c("ExpectedInfluence", "Edge"),
    nBoots = 1000,
    nCores = 8,
    type = "case"
)
write_rds(g_bs_case, here("outputs", "cache", "g_bs_case.rds"))

stability_drop_plot <- plot(g_bs_case, statistics = "ExpectedInfluence") + 
    theme_pander() +
    theme(
        legend.position = "none",
        plot.margin = margin(2, 2, 2, 2, "mm")
    )
stability_drop_plot
ggsave(
    here("outputs", "figs", "data-discovery_desc-bs_stability_drop.pdf"),
    stability_drop_plot, 
    height = 4, 
    width = 8
)

# check stability of edge
g_bs_np <- bootnet(
    g,
    statistics = c("ExpectedInfluence", "Edge"),
    nBoots = 1000,
    nCores = 8,
    type = "nonparametric"
)
write_rds(g_bs_np, here("outputs", "cache", "g_bs_np.rds"))

stability_edge_plot <- plot(g_bs_np,  statistics = "Edge", order = "sample") +
    theme_bw() +
    theme(
        legend.position = "none",
        plot.margin = margin(2, 2, 2, 2, "mm")
    )
stability_edge_plot
ggsave(
    here("outputs", "figs", "data-discovery_desc-bs_stability_edge.pdf"),
    stability_edge_plot, 
    height = 18,
    width = 8
)
plot(g_bs_np, plot = "interval", split0 = TRUE, statistics = "Edge", order = "sample")


