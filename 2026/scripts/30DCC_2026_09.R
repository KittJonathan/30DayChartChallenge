# 30 Day Chart Challenge
# 2026
# Distributions
# Day 09
# Wealth

# 📦 Packages ----

library(tidyverse)
library(gapminder)

# 📄 Data ----

gdp <- gapminder |> 
  filter(year == 2007)
  

# 📊 Plot ----

p <- ggplot(gdp,
       aes(x = gdpPercap)) +
  geom_histogram() +
  facet_wrap(~continent, ncol = 1)

ggsave("2026/figs/30DCC_2026_09.png", p, dpi = 320, width = 12, height = 6)
