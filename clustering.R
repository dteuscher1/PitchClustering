# Load packages
library(vroom)
library(tidyverse)
library(cluster)
library(patchwork)
library(mclust)

# Read in data
baseball <- vroom("statcast_2021.csv")

# Filter uncommon pitches and remove NA values
remove_pitches <- c("Eephus", "Intentional Ball", "Knuckleball", "Forkball", "Fastball", "Pitch Out", "Screwball", "NaN")
pitching <- baseball %>%
    dplyr::select(pitch_name, release_spin_rate, pfx_x, pfx_z, p_throws, 
                  player_name, release_speed, pitch_type, events, game_date) %>%
    mutate(pfx_z = pfx_z *12,
           pfx_x = pfx_x *12,
           season = lubridate::year(game_date)) %>%
    filter(!is.na(release_spin_rate), !(pitch_name %in% remove_pitches), !is.na(pitch_name))

# Filter for pitches thrown by right handed pitchers
pitch_2021 <- pitching %>% 
    filter(p_throws == "R")

# Set seed to get reprodue results
set.seed(3)
# Take a sample of 20000 pitches
small_2021 <- pitch_2021 %>% 
    sample_n(20000)

# Calculate the average of the variables for each pitch
pitch_avg2 <- small_2021 %>%
    group_by(pitch_name) %>%
    summarize(avg_vert = mean(pfx_z),
              avg_hor = mean(pfx_x),
              avg_speed = mean(release_speed),
              avg_spin = mean(release_spin_rate))

# Create a plot of avg. horizontal and avg. vertical movement
p1 <- ggplot(pitch_avg2, aes(x = avg_hor, y = avg_vert, color = pitch_name)) +
    geom_point(size = 4) + scale_color_brewer(palette = "Paired") +
    labs(title = "MLB", color = "Pitch",
         x = "Average Horizontal Movement (in.)", y = "Average Vertical Movement (in.)") + 
    theme_bw() + xlim(-20, 15) + ylim(-15, 20)

# Scale the data
X <- small_2021 %>% 
    dplyr::select(pfx_x, pfx_z, release_speed) %>%
    scale()

# Fit k-means and Gaussian mixture models
km.res <- kmeans(X, 8, nstart = 20, iter.max = 20)
clust1 <- Mclust(X)

# Extract the results
mclust_results <- small_2021 %>%
    mutate(cluster = clust1$classification)
kmeans_results <- small_2021 %>%
    mutate(cluster = km.res$cluster)

# Calculate the averages for cluster with k-means and GMM
pitch_avg <- mclust_results %>%
    group_by(cluster) %>%
    summarize(avg_vert = mean(pfx_z),
              avg_hor = mean(pfx_x),
              avg_speed = mean(release_speed),
              avg_spin = mean(release_spin_rate))
p2 <- ggplot(pitch_avg, aes(x = avg_hor, y = avg_vert, color = factor(cluster))) +
    geom_point(size = 4) + scale_color_brewer(palette = "Paired") + 
    labs(title = "Gaussian Mixed Model", color = "Cluster",
         x = "Average Horizontal Movement (in.)", y = "Average Vertical Movement (in.)") + 
    theme_bw() + xlim(-20, 15) + ylim(-15, 20)

pitch_avg3 <- kmeans_results %>%
    group_by(cluster) %>%
    summarize(avg_vert = mean(pfx_z),
              avg_hor = mean(pfx_x),
              avg_speed = mean(release_speed),
              avg_spin = mean(release_spin_rate))
p3 <- ggplot(pitch_avg3, aes(x = avg_hor, y = avg_vert, color = factor(cluster))) +
    geom_point(size = 4) + scale_color_brewer(palette = "Paired")  +
    labs(title = "K-Means", color = "Cluster",
         x = "Average Horizontal Movement (in.)", y = "Average Vertical Movement (in.)") + 
    theme_bw() + xlim(-20, 15) + ylim(-15, 20)

# Combine the three plots together
p1 + p2 + p3

# Create a boxplot of the speed of each pitch and the pitches in each cluster
p4 <- ggplot(kmeans_results, aes(x = factor(cluster), y = release_speed)) + geom_boxplot(fill = "dodgerblue") + ylim(60, 105) +
    theme_bw() + labs(y = "Speed (mph)", x = "Cluster", title = "K-means") + coord_flip()
p5 <- ggplot(small_2021, aes(x = factor(pitch_name), y = release_speed)) + geom_boxplot(fill = "dodgerblue") + 
     ylim(60, 105) +
    theme_bw()  + labs(y = "Speed (mph)", x = "Pitch Type", title = "MLB") + coord_flip() 
p6 <- ggplot(mclust_results, aes(x = factor(cluster), y = release_speed)) + geom_boxplot(fill = "dodgerblue") + 
    ylim(60, 105) +
    theme_bw()  + labs(y = "Speed (mph)", x = "Cluster", title = "Gaussian Mixed Model") + coord_flip() 

# Combine the plots together
p5 / p4 / p6

