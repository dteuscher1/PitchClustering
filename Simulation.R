# Load packages
library(mvtnorm)
library(tidyverse)
library(mclust)

# Function for first simulation
simulation_1 <- function(a){
    # Create mean vectors
    clust1_mean <- matrix(c(4, 3, 2), ncol = 1)
    clust2_mean <- matrix(c(1, 4, -1), ncol = 1)
    clust3_mean <- matrix(c(-1, 3, 2), ncol = 1)
    # Combine into a list
    means <- list(clust1_mean, clust2_mean, clust3_mean)
    # Create variance-covariance matrix
    var_cov <- matrix(c(1, .5, .5,
                        .5, 1, .5,
                        .5, .5, 1), nrow = 3, byrow = TRUE)
    # Simulate the data from 3 clusters with 100 observations from each
    sim_data <- data.frame()
    for(i in 1:3){
        sim_data_clust <- data.frame(rmvnorm(100, mean = means[[i]], sigma = a *var_cov)) %>%
            mutate(cluster = i)
        sim_data <- sim_data %>% bind_rows(sim_data_clust)
    }
    # Scale the data
    X <- sim_data %>% dplyr::select(1:3) %>% scale()
    
    # Run k-means and Gaussian mixed models and determine accuracy 
    km <- kmeans(X, 3)
    km_accuracy <- sum(apply(table(km$cluster, sim_data$cluster), 1, max))/300
    mm <- Mclust(X, G = 3, verbose = FALSE)
    mm_accuracy <- sum(apply(table(mm$classification, sim_data$cluster), 1, max))/300
    return(c(km_accuracy, mm_accuracy))
}
# Run simulation for a = 1, 3, 5
results1 <- replicate(1000, simulation_1(1))
results2 <- replicate(1000, simulation_1(3))
results3 <- replicate(1000, simulation_1(5))

# Get mean and Monte Carlo errors
apply(results1, 1, mean)
apply(results2, 1, mean)
apply(results3, 1, mean)
apply(results1, 1, quantile, c(.025, .975))
apply(results2, 1, quantile, c(.025, .975))
apply(results3, 1, quantile, c(.025, .975))

# Function for second simulation
simulation_2 <- function(a) {
    # Create cluster means
    clust1_mean <- matrix(c(4, 3, 2), ncol = 1)
    clust2_mean <- matrix(c(1, 4, -1), ncol = 1)
    clust3_mean <- matrix(c(-1, 3, 2), ncol = 1)
    # Create list of means
    means <- list(clust1_mean, clust2_mean, clust3_mean)
    # Create variance-covariance matrices
    var_cov_1 <- matrix(c(1, .5, .5,
                         .5, 1, .5,
                         .5, .5, 1), nrow = 3, byrow = TRUE)
    var_cov_2 <- matrix(c(1, -.5, -.5,
                          -.5, 1, -.5,
                          -.5, -.5, 1), nrow = 3, byrow = TRUE)
    var_cov_3 <- matrix(c(1, .5, -.5,
                          .5, 1, .5,
                          -.5, .5, 1), nrow = 3, byrow = TRUE)
    # Create a list of matrices
    var_covs <- list(var_cov_1, var_cov_2, var_cov_3)
    # Simulate the data from the 3 clusters
    sim_data <- data.frame()
    for(i in 1:3){
        sim_data_clust <- data.frame(rmvnorm(100, mean = means[[i]], sigma = a * var_covs[[i]])) %>%
            mutate(cluster = i)
        sim_data <- sim_data %>% bind_rows(sim_data_clust)
    }
    # Scale the data
    X <- sim_data %>% dplyr::select(1:3) %>% scale()
    # Cluster with k-means and Gaussian mixed models and calculate accuracy
    km <- kmeans(X, 3)
    km_accuracy <- sum(apply(table(km$cluster, sim_data$cluster), 1, max))/300
    mm <- Mclust(X, G = 3, verbose = FALSE)
    mm_accuracy <- sum(apply(table(mm$classification, sim_data$cluster), 1, max))/300
    return(c(km_accuracy, mm_accuracy))
}
# Simulate with a = 1, 3, 5
results1_sim2 <- replicate(1000, simulation_2(1))
results2_sim2 <- replicate(1000, simulation_2(3))
results3_sim2 <- replicate(1000, simulation_2(5))
# Calculate mean and 95% MC interval
apply(results1_sim2, 1, mean)
apply(results2_sim2, 1, mean)
apply(results3_sim2, 1, mean)
apply(results1_sim2, 1, quantile, c(.025, .975))
apply(results2_sim2, 1, quantile, c(.025, .975))
apply(results3_sim2, 1, quantile, c(.025, .975))
