# 30 Day Chart Challenge
# 2026
# Distributions
# Day 07
# Multiscale

# https://genomics.senescence.info/species/index.html

# 📦 Packages ----

library(tidyverse)

# 📄 Data ----

age <- read_tsv("2026/data/anage_data.txt")

age |> 
  filter(`Data quality` == "high")

# 📊 Plot ----