# Environment ------------------------------------------------------------------
library(tidyverse)
library(here)
library(bootnet)
library(qgraph)
library(bnlearn)
library(parallel)
library(pbmcapply)

# Data IO  ---------------------------------------------------------------------
overlapped_features <- read_rds(here("outputs", "cache", "overlapped_features.rds"))
graph_layout <- read_rds(here("outputs", "cache", "data-discovery_desc-aggression_graph_layout.rds"))

overlapped_features_details <- 
    here("data", "processed", "lasso_overlapped_features_details.csv") %>% 
    read_csv(show_col_types = FALSE)

discovery_df <- here("data", "processed", "discovery_dataset.rds") %>% 
    read_rds() %>% 
    select(bpaq_tot, g_psy, all_of(overlapped_features)) %>% 
    drop_na() %>% 
    mutate(
        bpaq_tot = ntile(bpaq_tot, 10),
        g_psy = ntile(g_psy, 10)
    ) %>% 
    mutate_all(as.ordered) %>% 
    as.data.frame()

blacklist <- rbind(
    # Nothing cause traits:
    data.frame(
        from = keep(overlapped_features, overlapped_features != "UCLA_14"), 
        to = "UCLA_14"
    ),
    data.frame(
        from = keep(overlapped_features, 
                    !(overlapped_features %>% str_detect("BIS"))),
        to = "BIS_2"
    ),
    data.frame(
        from = keep(overlapped_features, 
                    !(overlapped_features %>% str_detect("BIS"))), 
        to = "BIS_6"
    ),
    data.frame(
        from = keep(overlapped_features, 
                    !(overlapped_features %>% str_detect("BIS"))), 
        to = "BIS_29"
    ),
    data.frame(
        from = keep(overlapped_features, overlapped_features != "BFI_37"), 
        to = "BFI_37"
    ),
    data.frame(
        from = keep(overlapped_features, overlapped_features != "Age"),
        to = "Age"
    )
)


# DAG Estimation ---------------------------------------------------------------
set.seed(1234)
res <- pc.stable(discovery_df, blacklist = blacklist)

qgraph(
    res,
    layout = graph_layout,
    labels = c("Aggr", "Psy", overlapped_features_details$Label),
    color = c("#B5CAA0", "#B5CAA0", rep("#DAC9A6", 22)),
    legend = FALSE,
    # node
    vsize = 8,
    vTrans = 250,
    label.fill.vertical = 0.2,
    borders = FALSE,
    # edge
    esize = 3.5,
    fade = TRUE,
    # edge curvature
    curve = 0.5,
    curveAll = TRUE,
    cut = 0,
    filetype = "pdf",
    filename = here("outputs", "figs", "data-discovery_desc-dag")
)


boots <- pbmclapply(1:1000, function(x) {
        n_subj <- nrow(discovery_df)
        bs_index <- sample(x = seq_len(n_subj), size = n_subj, replace = TRUE)
        # bootstrapping the participants
        data_bs <- discovery_df[bs_index, ] %>% droplevels()
        res_bs <- pc.stable(data_bs, blacklist = blacklist)
        
        # extract edges
        graph <- qgraph(res_bs, DoNotPlot = TRUE)
        edges <- as.data.frame(graph$Edgelist[c("from", "to", "directed")])
        
        return(edges)
    },
    mc.style = "txt",
    mc.cores = getOption("mc.cores", 4L)
)
write_rds(boots, here("outputs", "cache", "boots_cache.rds"))
# boots <- read_rds(here("outputs", "cache", "boots_cache.rds"))

boots_aggregated <- do.call(rbind, boots) %>%
    select(from, to, directed) %>% 
    group_by(from, to, directed) %>% 
    tally() %>% 
    mutate(prop = n / length(boots))

boot_graph <- qgraph(
    boots_aggregated[, c("from", "to", "prop")],
    directed = boots_aggregated$directed,
    # layout
    layout = graph_layout,
    labels = c("Aggr", "Psy", overlapped_features_details$Label),
    color = c("#B5CAA0", "#B5CAA0", rep("#DAC9A6", 22)),
    legend = FALSE,
    maximum = 1,
    minimum = 0.4, 
    cut = 0,
    # node
    vsize = 8,
    vTrans = 250,
    label.fill.vertical = 0.2,
    borders = FALSE,
    # edge
    esize = 4,
    fade = FALSE,
    edge.labels = TRUE,
    edge.label.cex = 0.65,
    edge.color = "gray50",
    # edge curvature
    curve = 0.5,
    curveAll = TRUE,
    filetype = "pdf",
    filename = here("outputs", "figs", "data-discovery_desc-dag_bs_1000")
)
    