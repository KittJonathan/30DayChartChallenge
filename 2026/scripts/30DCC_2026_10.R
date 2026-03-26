# 30 Day Chart Challenge
# 2026
# Distributions
# Day 10
# Pop Culture

# 📦 Packages ----

library(tidyverse)
# library(spotifyr)

# 📄 Data ----

spotify_songs_raw <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

# 📊 Plot ----

spotify_songs_raw %>%
  na.omit(era) %>%
  ggplot() +
  geom_density(aes(duration_min, fill=playlist_genre)) +
  facet_grid(era~playlist_genre) +
  theme_minimal() +
  labs(x='Duration (min)') +
  theme(legend.position = 'none')
