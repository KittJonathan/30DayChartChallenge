# 30 Day Chart Challenge
# 2026
# Distributions
# Day 08
# Circular

# 📦 Packages ----

library(tidyverse)

# 📄 Data ----

# https://public.opendatasoft.com/explore/assets/donnees-synop-essentielles-omm/

wind <- read_csv2("2026/data/donnees-synop-essentielles-omm.csv")

wind |> 
  janitor::clean_names() |> 
  select(date, direction_du_vent_moyen_10_mn, vitesse_du_vent_moyen_10_mn) |> 
  filter(year(date) == 2025)

# 📊 Plot ----