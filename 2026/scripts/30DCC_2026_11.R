# 30 Day Chart Challenge
# 2026
# Distributions
# Day 11
# Physical

# 📦 Packages ----

library(tidyverse)
library(palmerpenguins)
library(patchwork)

# 📄 Data ----

# 📊 Plot ----

p1 <- penguins |> 
  ggplot() +
  geom_histogram(aes(x = bill_length_mm, fill = species))

p2 <- penguins |> 
  ggplot() +
  geom_histogram(aes(x = bill_depth_mm, fill = species))

p3 <- penguins |> 
  ggplot() +
  geom_histogram(aes(x = flipper_length_mm, fill = species))

p4 <- penguins |> 
  ggplot() +
  geom_histogram(aes(x = body_mass_g, fill = species))

p <- (p1 / p2 / p3 / p4)

ggsave("2026/figs/30DCC_2026_11.png", p, dpi = 320, width = 12, height = 6)
