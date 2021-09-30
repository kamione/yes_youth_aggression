# Environment ------------------------------------------------------------------
library(tidyverse)
library(here)
library(bootnet)
library(qgraph)
library(bnlearn)
library(Cairo)
library(snow)

# Data IO  ---------------------------------------------------------------------
# Read data:
top20 <- here("data", "processed", "lasso_top20_variables_details.csv") %>% 
    read_csv(col_types = cols()) %>% 
    arrange(Variable)

data <- here("data", "processed", "yes_baseline_outcome-aggression_n-2215_p-293.csv") %>% 
    read_csv(col_types = cols()) %>% 
    mutate(BPAQ_tot = rowSums(across(BPAQ_1:BPAQ_12)), .before = BPAQ_1) %>%
    select(BPAQ_tot, all_of(top20$Variable)) %>% 
    # for dag, ordered variables are required
    mutate_all(as.ordered)
    

graph_layout <- here("outputs", "graph_layouts", "aggression_graph_layout.rds") %>% 
    read_rds()


blacklist <-  rbind(
    # Nothing cause traits:
    data.frame(
        from = top20$Variable[top20$Variable!="BFI_12"], to = "BFI_12"
    ),
    data.frame(
        from = top20$Variable[top20$Variable!="BFI_17"], to = "BFI_17"
    ),
    data.frame(
        from = top20$Variable[top20$Variable!="BFI_33"], to = "BFI_33"
    ),
    data.frame(
        from = top20$Variable[top20$Variable!="BIS_15"], to = "BIS_15"
    ),
    data.frame(
        from = top20$Variable[top20$Variable!="BIS_17"], to = "BIS_17"
    ),
    data.frame(
        from = top20$Variable[top20$Variable!="UCLA_1"], to = "UCLA_1"
    ),
    data.frame(
        from = top20$Variable[top20$Variable!="UCLA_19"], to = "UCLA_19"
    )
)


# DAG Estimation ---------------------------------------------------------------
# bnlearn estimator function
bn_estimator <- function(x, blacklist = NULL, n_imp = 10) {
    
    library(tidyverse)
    library(bnlearn)
    library(mice)
    library(qgraph)
    
    # impute missing data using mice
    miceres <- mice(data, n_imp, seed = 1234)
    
    # estimate graphs of each imputation
    graphs <- lapply(seq_len(n_imp), function(i)
        {
            imp <- complete(miceres, i)
            # Estimate the network:
            res <- pc.stable(imp, blacklist = blacklist)
     
            # Obtain edges:
            graph <- qgraph(res, DoNotPlot = TRUE)
            edges <- as.data.frame(graph$Edgelist[c("from", "to", "directed")])
            return(edges)
        }
    )
    
    # aggregate the graphs
    graphsDF <- do.call(rbind, graphs)
    summarized <- graphsDF %>%
        group_by(from, to, directed) %>%
        tally() %>%
        filter(n >= 0.9 * n_imp)
    
    # Return:
    return(summarized)
}

res <- bn_estimator(data, blacklist = blacklist, n_imp = 10)

# visualize the DAG estimation
graph <- qgraph(
    res[, c("from","to")],
    directed = res$directed,
    layout = graph_layout,
    labels = c("Aggression", top20$Label),
    groups = c("1. Outcomes", rep("2. Features", 20)), 
    color = c("#B5CAA0", rep("#DAC9A6", 20)),
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
    title = "Estimated DAG",
    filename = here("outputs", "figs", "aggression_dag_network")
)



# Bootstrapping for Stability --------------------------------------------------

cl <- makeSOCKcluster(8)

boots <- parLapply(cl, 1:100, function(x)
    {
        data_bs <- data[sample(seq_len(nrow(data)), nrow(data), TRUE), ]
        bn_estimator(data_bs, blacklist, 1)
    }
)

stopCluster(cl)




