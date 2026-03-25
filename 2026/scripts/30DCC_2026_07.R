# 30 Day Chart Challenge
# 2026
# Distributions
# Day 07
# Multiscale

# https://genomics.senescence.info/species/index.html

# 📦 Packages ----

library(tidyverse)
library(r2country)

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