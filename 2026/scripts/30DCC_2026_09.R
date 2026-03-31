# 30 Day Chart Challenge
# 2026
# Distributions
# Day 09
# Wealth

# 📦 Packages ----

library(tidyverse)
library(gapminder)
library(ggdist)

# Test ----

p <- gapminder |> 
  filter(year %in% c(1977, 2007)) |> 
  ggplot() +
  geom_jitter(aes(x = gdpPercap, y = continent),
              height = 0.2) +
  facet_wrap(~ year, nrow = 2)

ggsave("2026/figs/30DCC_2026_09.png", p, dpi = 320, width = 12, height = 6)

gapminder |> 
  filter(year == 1952) |> 
  ggplot(aes(x = gdpPercap, y = continent)) +
  # geom_density()
  # geom_violin()
  stat_pointinterval()

gapminder |> 
  filter(year == 1952) |> 
  ggplot(aes(x = continent, y = gdpPercap, fill = continent)) +
  stat_ccdfinterval(aes(slab_alpha = after_stat(f)),
                    thickness = 1, position = "dodge", fill_type = "gradient"
  )

gapminder |> 
  filter(year == 2007) |> 
  ggplot(aes(y = continent, x = gdpPercap)) +
  stat_slab()

ggplot(aes(y = abc, x = value, fill = abc)) +
  stat_slab(aes(thickness = after_stat(pdf*n)), scale = 0.7) +
  stat_dotsinterval(side = "bottom", scale = 0.7, slab_linewidth = NA)

# 📊 Plot ----

p <- ggplot(gdp,
       aes(x = gdpPercap)) +
  geom_histogram() +
  facet_wrap(~continent, ncol = 1)

ggsave("2026/figs/30DCC_2026_09.png", p, dpi = 320, width = 12, height = 6)
