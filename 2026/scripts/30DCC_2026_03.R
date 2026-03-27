# 30 Day Chart Challenge
# 2026
# Comparisons
# Day 03
# Mosaic

# https://gogonzo.github.io/marimekko/articles/getting-started.html

# 📦 Packages ----

library(tidyverse)
library(palmerpenguins)
library(marimekko)
library(showtext)
library(ggtext)

# ⚙️ Define plot parameters ----

font_add_google("Open Sans")
showtext::showtext_auto()

# Testing {marimekko} ----

p <- ggplot(penguins) +
  geom_marimekko(aes(fill = factor(species)),
                 formula = ~ island | species) +
  geom_marimekko_text(aes(label = after_stat(paste0(round(cond_prop * 100), "%"))),
                      family = "Open Sans", size = 20) +
  scale_fill_manual(values = c("Adelie" = "darkorange",
                               "Chinstrap" = "purple",
                               "Gentoo" = "cyan4")) +
  labs(title = "How many penguins?",
       subtitle = "Proportion of species (<span style = 'color:#ff8c00;'>Adelie</span>,
       <span style = 'color:#a020f0;'>Chinstrap</span>,
       <span style = 'color:#008b8b;'>Gentoo</span>) on each island",
       caption = "30DayChartChallenge 2026 | Comparisons | Day 03 - Mosaic | Source: {palmerpenguins}") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(family = "Open Sans", size = 50, color = "white"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "black", color = "black"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "black", color = "black"),
        plot.title = element_text(family = "Open Sans", face = "bold", color = "white", size = 80, hjust = 0.5, margin = margin(t = 15, b = 10)),
        plot.subtitle = element_markdown(family = "Open Sans", color = "white", size = 60, hjust = 0.5, margin = margin(b = 30)),
        plot.caption = element_text(family = "Open Sans", color = "white", size = 40, hjust = 0.5, margin = margin(t = 25, b = 15)))

ggsave("2026/figs/30DCC_2026_03.png", p, dpi = 320, width = 12, height = 6)