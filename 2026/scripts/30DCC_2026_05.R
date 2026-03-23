# 30 Day Chart Challenge
# 2026
# Comparisons
# Day 05
# Experimental

# 📦 Packages ----

library(tidyverse)
library(sysfonts)
font_add_google("Roboto")
font_add_google("Ubuntu")
font_add_google("Open Sans")
showtext::showtext_auto()

# 📄 Data ----

dice <- tibble(expand.grid(1:6, 1:6)) |> 
  rename(die1 = Var1, die2 = Var2) |> 
  mutate(total = die1 + die2) |> 
  count(total, name = "count_theoretical") |> 
  mutate(prop_theoretical = round(count_theoretical / 36, 3))

set.seed(42)
count_sim <- sample(x = 1:6, size = 50, replace = TRUE) + sample(x = 1:6, size = 50, replace = TRUE)

dice_sim <- count_sim |> 
  tibble() |> 
  count(count_sim) |> 
  rename(total = count_sim) |> 
  mutate(prop_sim = round(n / 50, 3)) |> 
  select(total, prop_sim)

dice |> 
  left_join(dice_sim) |> 
  mutate(prop_sim = case_when(is.na(prop_sim) ~ 0,
                              .default = prop_sim)) |> 
  ggplot() +
  geom_col(aes(x = total, y = prop_theoretical),
           alpha = 0.5) +
  geom_col(aes(x = total, y = prop_sim),
           alpha = 0.8)


# 📊 Plot ----

p1 <- ggplot(df, aes(x, y)) +
  stat_voronoi()
