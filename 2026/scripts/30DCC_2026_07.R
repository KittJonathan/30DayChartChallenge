# 30 Day Chart Challenge
# 2026
# Distributions
# Day 07
# Multiscale

# 📦 Packages ----

library(tidyverse)
library(palmerpenguins)
library(ggdist)
library(patchwork)

# 📄 Data ----

gdp <- read_csv("2026/data/gdp-worldbank-constant-usd.csv") |> 
  janitor::clean_names() |> 
  rename(name = entity)

data("country_continent")
data("country_names")

countries <- left_join(country_names, country_continent) |> 
  select(-ID)

countries |> 
  left_join(gdp) |> 
  filter(is.na(gdp))

# 📊 Plot ----

# Utiliser les données des pingouins avec une hiérarchie naturelle
# Échelle 1: espèces, Échelle 2: îles, Échelle 3: sexes
ggplot(penguins, aes(x = species, y = body_mass_g, fill = island)) +
  stat_halfeye(alpha = 0.7, position = position_dodge(width = 0.7)) +
  stat_dotsinterval(side = "bottom", scale = 0.3, 
                    position = position_dodge(width = 0.7)) +
  facet_wrap(~sex, ncol = 2) +
  labs(
    title = "Penguin Body Mass: Multi-scale Distribution",
    subtitle = "Hierarchy: Species → Island → Sex",
    x = "Species",
    y = "Body Mass (g)",
    fill = "Island"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Test ----

# Simulation de données météo (ou utilisez des données réelles)
set.seed(42)
weather_data <- tibble(
  date = seq.Date(from = as.Date("2020-01-01"), 
                  to = as.Date("2022-12-31"), 
                  by = "day"),
  temp = 15 + 10 * sin(2 * pi * (yday(date) - 15)/365) + 
    rnorm(length(date), 0, 3)  # Température avec saisonnalité
) %>%
  mutate(
    month = floor_date(date, "month"),
    season = case_when(
      month(date) %in% c(12, 1, 2) ~ "Winter",
      month(date) %in% c(3, 4, 5) ~ "Spring",
      month(date) %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Fall"
    ),
    scale = case_when(
      row_number() %% 3 == 0 ~ "daily",
      row_number() %% 3 == 1 ~ "monthly", 
      TRUE ~ "seasonal"
    )
  )

# Visualisation multi-échelle avec ggdist
ggplot(weather_data, aes(x = scale, y = temp, fill = scale)) +
  stat_halfeye(alpha = 0.7, adjust = 1.5) +
  stat_dotsinterval(side = "bottom", scale = 0.4, alpha = 0.6) +
  labs(
    title = "Distribution of Temperature at Different Temporal Scales",
    subtitle = "Daily, Monthly Aggregated, and Seasonal Patterns",
    x = "Temporal Scale",
    y = "Temperature (°C)"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Palmerpenguins ----

p1 <- penguins |> 
  filter(species == "Adelie") |> 
  ggplot() +
  geom_histogram(aes(x = bill_length_mm / bill_depth_mm,
                     fill = species)) +
  scale_x_continuous(limits = c(1.5, 4))

p2 <- penguins |> 
  filter(island == "Dream") |> 
  ggplot() +
  geom_histogram(aes(x = bill_length_mm / bill_depth_mm,
                     fill = species)) +
  scale_x_continuous(limits = c(1.5, 4))

p3 <- penguins |> 
  ggplot() +
  geom_histogram(aes(x = bill_length_mm / bill_depth_mm,
                     fill = species))  +
  scale_x_continuous(limits = c(1.5, 4))

p <- p1 / p2 / p3

ggsave("2026/figs/30DCC_2026_07.png", p, dpi = 320, width = 12, height = 6)
