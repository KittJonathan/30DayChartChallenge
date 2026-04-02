# 30 Day Chart Challenge
# 2026
# Comparisons
# Day 02
# Pictogram

# 📦 Packages ----

library(tidyverse)
library(palmerpenguins)
library(ggpop)
library(ggtext)
library(showtext)

# ⚙️ Define plot parameters ----

font_add_google("Open Sans")
showtext::showtext_auto()

# 📊 Plot ----

penguins_species <- penguins |> 
  count(species, sort = TRUE) |> 
  mutate(percent = round(100 * n / sum(n)))

p <- tibble(
  x = rep(1:10, times = 10), 
  y = rep(1:10, each = 10),
  species = c(rep("Adelie", penguins_species$percent[1]),
              rep("Gentoo", penguins_species$percent[2]),
              rep("Chinstrap", penguins_species$percent[3]))) |> 
  ggplot(aes(x, y, color = species)) +
  geom_icon_point(icon = "linux", size = 2.5) +
  scale_color_manual(
    values = c("Adelie" = "darkorange",
               "Chinstrap" = "purple",
               "Gentoo" = "cyan4")
  ) +
  geom_text(x = 12, y = 9.5, label = "Chinstrap: 20% (n = 20)",
            hjust = 0, family = "Open Sans", fontface = "bold", size = 20, color = "purple") +
  geom_text(x = 12, y = 6.5, label = "Gentoo: 36% (n = 124)",
            hjust = 0, family = "Open Sans", fontface = "bold", size = 20, color = "cyan4") +
  geom_text(x = 12, y = 3, label = "Adelie: 44% (n = 152)",
            hjust = 0, family = "Open Sans", fontface = "bold", size = 20, color = "darkorange") +
  labs(title = "How many penguins?",
       subtitle = "Number of individuals per species",
       caption = "30DayChartChallenge 2026 | Comparisons | Day 02 - Pictogram | Source: {palmerpenguins}") +
  scale_x_continuous(limits = c(-1, 20)) +
  scale_y_continuous(limits = c(0, 11)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(family = "Open Sans", face = "bold", 
                              colour = "black", size = 80, hjust = 0.5,
                              margin = margin(t = 10)),
    plot.subtitle = element_text(family = "Open Sans", 
                                     colour = "black", size = 60, hjust = 0.5,
                                     margin = margin(t = 10)),
    plot.caption = element_text(family = "Open Sans", colour = "black",
                                size = 40, hjust = 0.5, 
                                margin = margin(b = 10)),
    legend.position = "none"
  )

p

ggsave("2026/figs/30DCC_2026_02.png", p, dpi = 320, width = 12, height = 6)