# 30 Day Chart Challenge
# 2026
# Comparisons
# Day 01
# Part-to-Whole

# 📦 Packages ----

library(tidyverse)
# library(ggthemes)
# library(tidyplots)
theme_set(theme_bw())

# 📄 Data ----

# Source : Our World In Data - Electricity Mix
# https://ourworldindata.org/electricity-mix

elec_mix <- read_csv("2026/data/electricity-prod-source-stacked.csv")

elec_mix <- elec_mix |> 
  select(!(Entity:Year)) |> 
  pivot_longer(everything(),
               names_to = "Source",
               values_to = "TWh") |> 
  arrange(TWh) |> 
  mutate(Source = fct_inorder(Source))

# 📊 Plot ----

elec_mix |> 
  ggplot(aes(x = 1, y = TWh, fill = Source, color = Source)) +
  geom_col(position = "stack", alpha = 0.5) +
  labs(title = "Electricity mix",
       subtitle = "France - 2025")
