library(vroom)
baseball <- vroom("statcast_2021.csv")
head(baseball)

remove_pitches <- c("Eephus", "Intentional Ball", "Knuckleball", "Forkball", "Pitch Out", "Screwball", "NaN")
pitching <- baseball %>%
    dplyr::select(pitch_name, release_spin_rate, pfx_x, pfx_z, p_throws, 
                  player_name, release_speed, pitch_type, events, game_date) %>%
    mutate(pfx_z = pfx_z *12,
           pfx_x = pfx_x *12,
           season = lubridate::year(game_date)) %>%
    filter(!is.na(release_spin_rate), !(pitch_name %in% remove_pitches), !is.na(pitch_name))

pitch_2021 <- pitching %>% 
    filter(p_throws == "R")

ggplot(pitch_2021, aes(x=pfx_x, y = pfx_z, color = pitch_name))+
    geom_point() + ylim(-30,30)

pitch_avg <- pitch_2021 %>%
    group_by(pitch_name) %>%
    summarize(avg_vert = mean(pfx_z),
              avg_hor = mean(pfx_x),
              avg_speed = mean(release_speed),
              avg_spin = mean(release_spin_rate))
ggplot(pitch_avg, aes(x = avg_hor, y = avg_vert, color = pitch_name)) +
    geom_point(size = 4)
