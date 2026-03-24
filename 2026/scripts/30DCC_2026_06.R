# 30 Day Chart Challenge
# 2026
# Comparisons
# Day 06 - Data Day
# Reporters Without Borders

# https://rsf.org/en/rsf-world-press-freedom-index-2025-economic-fragility-leading-threat-press-freedom?year=2025&data_type=general

# 📦 Packages ----

library(tidyverse)

# 📄 Data ----

rsf_index_2025 <- read_csv2("2026/data/2025.csv")
rsf_index_2024 <- read_csv2("2026/data/2024.csv")
rsf_index_2023 <- read_csv2("2026/data/2023.csv")

rsf_index_2025 <- rsf_index_2025 |> 
  summarise(Global = mean(`Score 2025`),
            Economic = mean(`Economic Context`),
            Political = mean(`Political Context`),
            Legislative = mean(`Legal Context`),
            Social = mean(`Social Context`),
            Security = mean(Safety),
            .by = Zone) |> 
  mutate(Zone = case_when(Zone == "UE Balkans" ~ "EU Balkans",
                          Zone == "Asie-Pacifique" ~ "Asia - Pacific",
                          Zone == "Am\xe9riques" ~ "Americas",
                          Zone == "Afrique" ~ "Africa",
                          Zone == "MENA" ~ "Middle East - North Africa",
                          .default = "EEAC")) |> 
  pivot_longer(cols = -Zone, 
               names_to = "Indicator",
               values_to = "Mean") |> 
  mutate(Year = 2025, .before = Zone)

rsf_index_2024 <- rsf_index_2024 |> 
  summarise(Global = mean(Score),
            Economic = mean(`Economic Context`),
            Political = mean(`Political Context`),
            Legislative = mean(`Legal Context`),
            Social = mean(`Social Context`),
            Security = mean(Safety),
            .by = Zone) |> 
  mutate(Zone = case_when(Zone == "UE Balkans" ~ "EU Balkans",
                          Zone == "Asie-Pacifique" ~ "Asia - Pacific",
                          Zone == "Amériques" ~ "Americas",
                          Zone == "Afrique" ~ "Africa",
                          Zone == "MENA" ~ "Middle East - North Africa",
                          .default = "EEAC")) |> 
  pivot_longer(cols = -Zone, 
               names_to = "Indicator",
               values_to = "Mean") |> 
  mutate(Year = 2024, .before = Zone)

rsf_index_2023 <- rsf_index_2023 |> 
  summarise(Global = mean(Score),
            Economic = mean(`Economic Context`),
            Political = mean(`Political Context`),
            Legislative = mean(`Legal Context`),
            Social = mean(`Social Context`),
            Security = mean(Safety),
            .by = Zone) |> 
  mutate(Zone = case_when(Zone == "UE Balkans" ~ "EU Balkans",
                          Zone == "Asie-Pacifique" ~ "Asia - Pacific",
                          Zone == "Amériques" ~ "Americas",
                          Zone == "Afrique" ~ "Africa",
                          Zone == "MENA" ~ "Middle East - North Africa",
                          .default = "EEAC")) |> 
  pivot_longer(cols = -Zone, 
               names_to = "Indicator",
               values_to = "Mean") |> 
  mutate(Year = 2023, .before = Zone)
