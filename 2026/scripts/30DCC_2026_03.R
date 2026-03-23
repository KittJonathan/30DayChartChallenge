# 30 Day Chart Challenge
# 2026
# Comparisons
# Day 03
# Mosaic

# https://gogonzo.github.io/marimekko/articles/getting-started.html

# 📦 Packages ----

library(tidyverse)
# library(ggmosaic)
library(marimekko)
library(palmerpenguins)
library(sysfonts)
font_add_google("Open Sans")
showtext::showtext_auto()

# Testing {marimekko} ----

ggplot(penguins) +
  geom_marimekko(aes(fill = factor(species)),
                 formula = ~ island | species) +
  geom_marimekko_text(aes(label = after_stat(paste0(round(cond_prop * 100), "%"))),
                      family = "Open Sans", size = 3) +
  scale_fill_manual(values = c("Adelie" = "darkorange",
                               "Chinstrap" = "purple",
                               "Gentoo" = "cyan4"))


# 📄 Data ----

# Source : {palmerpenguins}

df1 <- penguins |> 
  count(island) |> 
  mutate(percent = 100 * round(n / sum(n), 2),
         cumsum = cumsum(percent),
         xmin = lag(cumsum, default = 0)) |> 
  select(island, n, xmin = xmin, xmax = cumsum)

df2 <- penguins |> 
  count(island, species) |> 
  mutate(percent = 100 * round(n / sum(n), 2),
         .by = island) |> 
  arrange(island, desc(percent)) |> 
  mutate(cumsum = cumsum(percent),
         ymin = lag(cumsum, default = 0),
         .by = island) |> 
  select(island, species, ymin, ymax = cumsum)

df <- left_join(df1, df2) |> 
  select(island, species, xmin, xmax, ymin, ymax) 

labels_islands <- df |> 
  distinct(island, xmin, xmax) |> 
  mutate(x = xmin + (xmax - xmin) / 2)

labels_species <- df |> 
  head(3) |> 
  mutate(x = xmin + (xmax - xmin) / 2,
         y = ymin + (ymax - ymin) / 2) |> 
  select(species, x, y)

p <- ggplot() +
  geom_rect(data = df,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                fill = species),
            color = "#4c5265", linewidth = 1.5,
            show.legend = FALSE) +
  geom_text(data = labels_islands,
            aes(x = x, y = -6, label = island),
            family = "Open Sans", color = "white", size = 15) +
  geom_segment(aes(x = 0, xend = 100, y = -10, yend = -10),
               color = "white") +
  geom_text(aes(x = 50, y = -14, label = "Islands"),
            family = "Open Sans", color = "white", size = 15) +
  geom_text(data = labels_species,
            aes(x = x, y = y, label = species),
            family = "Open Sans", color = "white", size = 15) +
  scale_fill_manual(values = c("Chinstrap" = "purple",
                               "Gentoo" = "cyan4",
                               "Adelie" = "darkorange")) +
  labs(title = "Palmer penguins",
       subtitle = "Proportion of each species per island",
       caption = "30DayChartChallenge 2026 | Comparisons | Day 03 - Mosaic | Source: {palmerpenguins}") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#4c5265"),
        plot.background = element_rect(fill = "#4c5265"),
        plot.title = element_text(family = "Open Sans", face = "bold", color = "white", size = 40, hjust = 0.5, margin = margin(t = 15)),
        plot.subtitle = element_text(family = "Open Sans", color = "white", size = 35, hjust = 0.5),
        plot.caption = element_text(family = "Open Sans", color = "white", size = 30, hjust = 0.5, margin = margin(t = 15, b = 15)))

ggsave("2026/figs/30DCC_2026_03.png", p, dpi = 320, width = 12, height = 6) 

