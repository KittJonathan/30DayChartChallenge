# 30 Day Chart Challenge
# 2026
# Distributions
# Day 08
# Circular

# 📦 Packages ----

library(tidyverse)
library(palmerpenguins)
library(geomtextpath)

# 📊 Plot ----

p <- penguins |> 
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) |> 
  arrange(bill_ratio)|> 
  drop_na(bill_ratio) |> 
  rowid_to_column() |> 
  select(rowid, species, bill_ratio) |> 
  ggplot() +
  geom_segment(aes(x = rowid, xend = rowid,
                   y = -2, yend = 0),
               color = "black") +
  geom_segment(aes(x = rowid, xend = rowid, 
                   y = 0, yend = bill_ratio,
                   color = species),
               linewidth = 0.25) +
  scale_color_manual(
    values = c("Adelie" = "darkorange",
               "Chinstrap" = "purple",
               "Gentoo" = "cyan4")
  ) +
  coord_polar(theta = "x", start = 0) +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "black"))

p

ggsave("2026/figs/30DCC_2026_08.png", p, dpi = 320, width = 12, height = 6)
