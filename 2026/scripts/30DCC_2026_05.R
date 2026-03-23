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

# Simulate rolling 2 dice
set.seed(42)
die1 <- sample(x = 1:6, size = 9000, replace = TRUE)
die2 <- sample(x = 1:6, size = 9000, replace = TRUE)
dice <- die1 + die2


x <- sample(1:400, size = 100)
y <- sample(1:400, size = 100)

df <- tibble(
  x = x,
  y = y
) |> 
  mutate(
    dist_euclidian = sqrt((x - 200) ^ 2 + (y - 200) ^ 2),
    dist_manhattan = abs(x - 200) + abs(y - 200),
    dist_tchebychev = max(abs(x - 200), abs(y - 200))
  )


# 📊 Plot ----

p1 <- ggplot(df, aes(x, y)) +
  stat_voronoi()
