# 30 Day Chart Challenge
# 2026
# Comparisons
# Day 04
# Slope

# 📦 Packages ----

library(tidyverse)
library(sysfonts)
font_add_google("Open Sans")
showtext::showtext_auto()

# 📄 Data ----

# Source : Our World In Data - World population
# https://ourworldindata.org/population-growth

population <- read_csv("2026/data/population.csv")

population <- population |> 
  filter(Entity %in% c("Africa (UN)", "Americas (UN)", "Asia (UN)",
                       "Europe (UN)", "Oceania (UN)"),
         Year %in% c(1950, 2023)) |> 
  select(Entity, Year, Total = `all years`) |> 
  mutate(Entity = str_remove_all(string = Entity, pattern =  "\\ \\(UN\\)")) |> 
  pivot_wider(id_cols = Entity, names_from = Year, values_from = Total, names_prefix = "y") |> 
  mutate(y1950 = y1950 / 1e9,
         y2023 = y2023 / 1e9)

# 📊 Plot ----

p <- ggplot(population) +
  geom_hline(yintercept = seq(0, 5, 1),
             linetype = "dotted", color = "grey") +
  geom_segment(aes(x = 1950, xend = 2023,
                   y = y1950, yend = y2023,
                   color = Entity, group = Entity),
               linewidth = 1, show.legend = FALSE) +
  scale_color_manual(values = c("Africa" = "#000000",
                                "Americas" = "#dd0021",
                                "Asia" =  "#f2c401",
                                "Europe" = "#0085c8",
                                "Oceania" = "#009e3d")) +
  geom_text(aes(x = 2025, y = y2023, label = Entity, color = Entity),
            family = "Open Sans", size = 16, hjust = 0,
            show.legend = FALSE) +
  labs(x = "", y = "Population (in billions)",
       title = "Evolution of the world population from 1950 to 2023",
       caption = "30DayChartChallenge 2026 | Comparisons | Day 04 - Slope | Source: Our World In Data") +
  scale_x_continuous(limits = c(1950, 2030)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", linewidth = 1),
        panel.grid.major.y = element_line(linetype = "dotted", linewidth = 1),
        plot.title = element_text(family = "Open Sans", face = "bold", size = 40, hjust = 0.5, margin = margin(t = 15)),
        plot.caption = element_text(family = "Open Sans", size = 30, hjust = 0.5, margin = margin(t = 15, b = 15)),
        legend.position = "top",
        axis.text = element_text(family = "Open Sans", size = 35),
        axis.title.y = element_text(family = "Open Sans", size = 40, margin = margin(l = 15, r = 15)))

ggsave("2026/figs/30DCC_2026_04.png", p, dpi = 320, width = 12, height = 6) 
