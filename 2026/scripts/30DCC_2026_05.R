# 30 Day Chart Challenge
# 2026
# Comparisons
# Day 05
# Experimental

# 📦 Packages ----

library(tidyverse)
# library(sysfonts)
# font_add_google("Roboto")
# font_add_google("Ubuntu")
# font_add_google("Open Sans")
# showtext::showtext_auto()

# 📄 Data ----

two_dice_theo <- tibble(expand.grid(1:6, 1:6)) |> 
  rename(die1 = Var1, die2 = Var2) |> 
  mutate(total = die1 + die2) |> 
  count(total, name = "count_theoretical") |> 
  mutate(prop_theoretical = round(count_theoretical / 36, 3))

ggplot() +
  geom_line(data = two_dice_theo, aes(x = total, y = prop_theoretical)) +
  geom_point(data = two_dice_theo, aes(x = total, y = prop_theoretical))

set.seed(42)
count_sim <- sample(x = 1:6, size = 100, replace = TRUE) + sample(x = 1:6, size = 100, replace = TRUE)

two_dice_sim <- count_sim |> 
  tibble() |> 
  count(count_sim) |> 
  rename(total = count_sim) |> 
  mutate(prop_sim = round(n / 100, 3)) |> 
  select(total, prop_sim)

ggplot() +
  geom_col(data = two_dice_sim, aes(x = total, y = prop_sim)) +
  geom_line(data = two_dice_theo, aes(x = total, y = prop_theoretical)) +
  geom_point(data = two_dice_theo, aes(x = total, y = prop_theoretical))


# 📊 Plot ----

p1 <- ggplot(df, aes(x, y)) +
  stat_voronoi()
