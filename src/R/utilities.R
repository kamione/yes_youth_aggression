library(ggpubr)
library(ggthemes)

# remove all registered parallel workers
unregister <- function() {
    env <- foreach:::.foreachGlobals
    rm(list = ls(name=env), pos = env)
}

confidence_interval <- function(vector, interval) {
    # Standard deviation of sample
    vec_sd <- sd(vector)
    # Sample size
    n <- length(vector)
    # Mean of sample
    vec_mean <- mean(vector)
    # Error according to t distribution
    error <- qt((interval + 1)/2, df = n - 1) * vec_sd / sqrt(n)
    # Confidence interval as a vector
    result <- c("lower" = vec_mean - error, "upper" = vec_mean + error)
    return(result)
}

plot_faparallel <- function(parallel) {
    obs = data.frame(parallel$fa.values)
    obs$type = c('Observed Data')
    obs$num = c(row.names(obs))
    obs$num = as.numeric(obs$num)
    colnames(obs) = c('eigenvalue', 'type', 'num')
    
    percentile = apply(parallel$values, 2, function(x) quantile(x, .95))
    min = as.numeric(nrow(obs))
    min = (4 * min) - (min - 1)
    max = as.numeric(nrow(obs))
    max = 4*max
    percentile1 = percentile[min:max]
    
    sim = data.frame(percentile1)
    sim$type = c("Simulated Data (95th %ile)")
    sim$num = c(row.names(obs))
    sim$num = as.numeric(sim$num)
    colnames(sim) = c("eigenvalue", "type", "num")
    
    eigendat = rbind(obs, sim)
    
    p <- ggplot(eigendat, aes(x = num, y = eigenvalue, shape = type)) +
        geom_line() +
        geom_point(size = 4) +
        scale_y_continuous(name = "Eigenvalue") +
        scale_x_continuous(
            name = "Factor Number",
            breaks = min(eigendat$num):max(eigendat$num)
        ) +
        scale_shape_manual(values = c(16, 1)) +
        geom_vline(xintercept = parallel$nfact, linetype = "dashed") +
        theme_bw()+
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            legend.title = element_blank(),
            legend.position = c(.7,.8),
            axis.line.x = element_line(color = "black"),
            axis.line.y = element_line(color = "black"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        )
        
    
    return(p)
}
