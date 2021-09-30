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
top20 <- here("data", "processed", "lasso_top20_variables.csv") %>% 
    read_csv(col_types = cols()) %>% 
    arrange(Variable) %>% 
    pull(Variable)

data <- here("data", "processed", "yes_baseline_outcome-aggression_n-2212_p-268.csv") %>% 
    read_csv(col_types = cols()) %>% 
    mutate(BPAQ_tot = rowSums(across(BPAQ_1:BPAQ_12)), .before = BPAQ_1) %>%
    select(BPAQ_tot, all_of(top20)) %>% 
    mutate(SOFAS_6m = SOFAS_6m %>% cut(c(0, 41, 61, 81, 100), labels = c("worse", "bad", "fair", "good")) %>% as.ordered()) %>% 
    mutate_if(is.numeric, as.ordered)
    

items <- data %>% colnames()


blacklist <- dataframe



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

res <- bn_estimator(data, n_imp = 10)

# visualize the DAG estimation
pdf(here("outputs", "figs", "dag_estimation.pdf"), width = 5, height = 4.5)
graph <- qgraph(res[, c("from","to")],
                directed = res$directed,
                repulsion = 0.9,
                title = "Estimated DAG",
                labels = items)
dev.off()



data_sub <- here("data", "processed", "yes_baseline_outcome-aggression_n-2215_p-266.csv") %>% 
    read_csv(col_types = cols()) %>% 
    mutate(BPAQ_phy = rowSums(across(BPAQ_1:BPAQ_3)), .before = BPAQ_1) %>% 
    mutate(BPAQ_verb = rowSums(across(BPAQ_4:BPAQ_6)), .before = BPAQ_1) %>% 
    mutate(BPAQ_anger = rowSums(across(BPAQ_7:BPAQ_9)), .before = BPAQ_1) %>% 
    mutate(BPAQ_host = rowSums(across(BPAQ_10:BPAQ_12)), .before = BPAQ_1) %>%
    select(BPAQ_phy:BPAQ_host, all_of(top20)) %>% 
    mutate(SOFAS_6m = SOFAS_6m %>% cut(c(0, 41, 61, 81, 100), labels = c("worse", "bad", "fair", "good")) %>% as.ordered()) %>% 
    mutate_if(is.numeric, as.ordered)


res_sub <- bn_estimator(data_sub, n_imp = 10)
items_sub <- data_sub %>% colnames()
# visualize the DAG estimation
pdf(here("outputs", "figs", "dag_estimation.pdf"), width = 5, height = 4.5)
graph <- qgraph(res_sub[, c("from","to")],
                directed = res_sub$directed,
                repulsion = 0.9,
                title = "Estimated DAG",
                labels = items_sub)
dev.off()




# Bootstrapping for Stability --------------------------------------------------

cl <- makeSOCKcluster(8)

boots <- parLapply(cl, 1:100, function(x)
    {
        data_bs <- data[sample(seq_len(nrow(data)), nrow(data), TRUE), ]
        bn_estimator(data_bs, blacklist, 1)
    }
)

stopCluster(cl)




