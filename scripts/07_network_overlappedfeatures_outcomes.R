# Environment ------------------------------------------------------------------
library(tidyverse)
library(mgm)
library(qgraph)
library(ggplot2)
library(ggthemes)
library(bootnet)
library(here)


# Data IO  ---------------------------------------------------------------------
aggr_sorted_feature_names <- read_rds(here("outputs", "cache", "aggr_sorted_feature_names.rds"))
psy_sorted_feature_names <- read_rds(here("outputs", "cache", "psy_sorted_feature_names.rds"))
overlapped_features <- intersect(psy_sorted_feature_names[1:75], aggr_sorted_feature_names[1:75])

overlapped_features_details <- 
    here("data", "processed", "lasso_overlapped_features_details.csv") %>% 
    read_csv(show_col_types = FALSE)

preprocessed_df <- here("data", "processed", "yes_baseline_outcome-aggression_n-2186_p-288.rds") %>% 
    read_rds() %>% 
    select(c(bpaq_tot, g_psy, all_of(overlapped_features))) %>% 
    drop_na()


n_features <- length(overlapped_features)
print(glue::glue("A total of {n_features} is selected"))

# Symptom Network --------------------------------------------------------------
network_data       <- list()
network_data$data  <- preprocessed_df
network_data$node  <- c("Aggr", "Psy", overlapped_features_details$Label)
network_data$node_detail <- c("Sum of Buss-Perry Aggression Questionnaire (Log)",
                              "General Psychoapthology",
                              overlapped_features_details$Details)
network_data$node_col <- c("#B5CAA0", "#B5CAA0", rep("#DAC9A6", n_features))
network_data$group <- c("1. Outcomes", "1. Outcomes", rep("2. Features", n_features))

g <- estimateNetwork(
    preprocessed_df,
    default = "EBICglasso",
    threshold = TRUE
)

centrality_plot <- g %>%
    centralityPlot(
        print   = FALSE,
        #labels  = c("Aggression", top20$Label),
        include = "ExpectedInfluence",
        orderBy = "ExpectedInfluence"
    ) +
    scale_x_continuous(limits = c(-0.3, 1.5)) +
    labs(x = "Standardized Centrality Indices") +
    theme(axis.title = element_text(size = 8)) +
    theme_pander()
ggsave(here("outputs", "figs", "data-full_desc-network_centrality.pdf"),
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
    legend.cex = 0.55,
    GLratio = 0.65,
    cut = 0,
    theme = "TeamFortress",
    filetype = "pdf",
    filename = here("outputs", "figs", "data-discovery_desc-outcomes_features_network")
)

  # save graph layout
write_rds(
    network_graph$layout, 
    file = here("outputs", "cache", "data-full_desc-aggression_graph_layout.rds"))

# check stability of graph
g_bs <- bootnet(g,
                statistics = "ExpectedInfluence",
                nBoots = 1000,
                nCores = 8,
                type = "case")

stability_plot <- plot(g_bs, statistics = "ExpectedInfluence") + 
    theme_pander() +
    theme(
        legend.position = "none",
        plot.margin = margin(2, 2, 2, 2, "mm")
    )
ggsave(here("outputs", "figs", "data-full_desc-bs_stability.pdf"),
       stability_plot, height = 4, width = 4)





