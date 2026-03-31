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

df <- penguins |> 
  mutate(bill_length_scaled = scale(bill_length_mm),
         bill_depth_scaled = scale(bill_depth_mm),
         flipper_length_scaled = scale(flipper_length_mm),
         body_mass_scaled = scale(body_mass_g))

range(df$bill_length_scaled, na.rm = T)
range(df$bill_depth_scaled, na.rm = T)
range(df$flipper_length_scaled, na.rm = T)
range(df$body_mass_scaled, na.rm = T)

# range : -3 -> 3

# 📊 Plot ----

p1 <- df |> 
  ggplot(aes(x = bill_length_scaled, fill = species)) +
  geom_density() +
  scale_x_continuous(limits = c(-3, 3),
                     breaks = seq(-3, 3, 0.5))

p2 <- df |> 
  ggplot(aes(x = bill_depth_scaled, fill = species)) +
  geom_density() +
  scale_x_continuous(limits = c(-3, 3),
                     breaks = seq(-3, 3, 0.5))

p3 <- df |> 
  ggplot(aes(x = flipper_length_scaled, fill = species)) +
  geom_density() +
  scale_x_continuous(limits = c(-3, 3),
                     breaks = seq(-3, 3, 0.5))
p4 <- df |> 
  ggplot(aes(x = body_mass_scaled, fill = species)) +
  geom_density() +
  scale_x_continuous(limits = c(-3, 3),
                     breaks = seq(-3, 3, 0.5))

p <- (p1 / p2 / p3 / p4)

ggsave("2026/figs/30DCC_2026_11.png", p, dpi = 320, width = 12, height = 6)
