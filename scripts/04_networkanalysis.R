# Environment ------------------------------------------------------------------
library(tidyverse)
library(mgm)
library(qgraph)
library(ggplot2)
library(bootnet)
library(here)
library(skimr)
library(ggthemes)

options(skimr_strip_metadata = FALSE)

# Data IO  ---------------------------------------------------------------------
top20 <- here("data", "processed", "lasso_top20_variables_details.csv") %>% 
    read_csv(col_types = cols()) %>% 
    arrange(Variable)


data <- here("data", "processed", "yes_baseline_outcome-aggression_n-2215_p-293.csv") %>% 
    read_csv(col_types = cols()) %>% 
    mutate(BPAQ_tot = rowSums(across(BPAQ_1:BPAQ_12)), .before = BPAQ_1) %>% 
    select(BPAQ_tot, all_of(top20$Variable))

data %>% skim()

# Symptom Network --------------------------------------------------------------
network_data       <- list()
network_data$data  <- data

network_data$node  <- c("Aggression", top20$Label)


# impute 10 data sets and run the estimate networks
# only edges survived 9 out of 10 will be keep.
impute_estimator <- function(x, nImpute = 10) {
    library("mice")
    library("bootnet")
    library("abind")
    
    # Impute missing data:
    miceres <- mice(x, m = nImpute, seed = 1234, use.matcher = TRUE,
                    printFlag = FALSE)
    
    # return a list of graphs with imputed data
    graphs <- lapply(seq_len(nImpute), function(i){
        imp <- complete(miceres, i)
        # Estimate the network:
        imputed_graphs <- estimateNetwork(
            imp,
            default = "EBICglasso",
            threshold = TRUE
        )
        return(imputed_graphs$graph)                
    })
    
    # Make array:
    array <- do.call(abind, c(graphs, list(along = 3)))
    
    # Average and retain only edges included in all imputations:
    final <- apply(array, 1:2, function(x) {
            (mean(x != 0) >= 0.9) * mean(x)
        }
    )
    return(final)
}

# graph that using sum score of aggression
g <- estimateNetwork(network_data$data, fun = impute_estimator)

centrality_plot <- g %>%
    centralityPlot(
        print   = FALSE,
        labels  = network_data$node,
        include = "ExpectedInfluence",
        orderBy = "ExpectedInfluence"
    ) +
    labs(x = "Standardized Centrality Indices") +
    theme_pander() +
    theme(axis.title = element_text(size = 12),
          plot.margin = unit(rep(2, 4), "mm"))
ggsave(file.path("outputs", "figs", "network_centrality.pdf"),
       centrality_plot, height = 6, width = 4)


network_graph <- qgraph(g$graph,
       layout = "spring",
       groups = c("1. Outcomes", rep("2. Features", 20)), 
       color = c("#B5CAA0", rep("#DAC9A6", 20)), 
       repulsion = 1,
       tuning = 0.25,
       labels = c("Aggression", top20$Label),
       nodeNames = c("Buss-Perry Aggression Scores (Total)", top20$Details),
       # node
       palette = "pastel",
       vsize = 4.5,
       vTrans = 250,
       label.fill.vertical = 0.2,
       borders = FALSE,
       # edge
       esize = 15,
       fade = TRUE,
       # legend
       legend = TRUE,
       legend.mode = "style1",
       legend.cex = 0.55,
       # layout
       GLratio = 0.6,
       # curve
       curve = 0.3,
       curveAll = TRUE,
       cut = 0,
       filetype = "pdf",
       filename = here("outputs", "figs", "aggression_network"),
       theme = "TeamFortress")

write_rds(network_graph$layout, path = here("outputs", "graph_layouts", "aggression_graph_layout.rds"))

g_boots <- bootnet(g,
                   statistics = "ExpectedInfluence", 
                   nBoots = 1000,
                   nCores = 8,
                   type = "case")

g_stability_fig <- plot(g_boots, statistics = "ExpectedInfluence") + 
    theme_pander() +
    theme(legend.position = "none",
          plot.margin = unit(rep(2, 4), "mm"))

ggsave(filename = here("outputs", "figs", "stability_bootstrapped_n-1000.pdf"),
       plot = g_stability_fig,
       height = 4,
       width = 5)

























# run the graphs using sub-scales
data_sub <- here("data", "processed", "yes_baseline_outcome-aggression_n-2215_p-293.csv") %>% 
    read_csv(col_types = cols()) %>% 
    mutate(BPAQ_phy = rowSums(across(BPAQ_1:BPAQ_3)), .before = BPAQ_1) %>% 
    mutate(BPAQ_verb = rowSums(across(BPAQ_4:BPAQ_6)), .before = BPAQ_1) %>% 
    mutate(BPAQ_anger = rowSums(across(BPAQ_7:BPAQ_9)), .before = BPAQ_1) %>% 
    mutate(BPAQ_host = rowSums(across(BPAQ_10:BPAQ_12)), .before = BPAQ_1) %>%
    select(BPAQ_phy:BPAQ_host, all_of(top20))

# Sub Symptom Network ----------------------------------------------------------
network_data       <- list()
network_data$data  <- data_sub

#networkSymptomDat$node  <- c(itemLabel[c(1:17, 21:25)])
#networkSymptomDat$group <- c(rep("Attention Deficits", 9),
#                            rep("Conduct Problems", 8),
#                            rep("Oppositional Defiant Behaviors", 5))

g_sub <- estimateNetwork(network_data$data, fun = impute_estimator)
pdf(here("outputs", "figs", "aggression_subscores_network.pdf"), width = 5, height = 4.5)
qgraph(g_sub$graph,
       layout = "spring",
       esize = 25,
       palette = "pastel",
       esize = 25,
       legend.cex = 0.35, 
       legend = FALSE, 
       vsize = 8,
       vTrans = 254)
dev.off()









