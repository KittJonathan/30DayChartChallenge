# 30 Day Chart Challenge
# 2026
# Comparisons
# Day 02
# Pictogram

# 📦 Packages ----

library(tidyverse)
# remotes::install_github("hrbrmstr/waffle")
library(waffle)
library(hrbrthemes)

# 📄 Data ----

# Source : Our World In Data - Share of the population that owns a mobile phone
# https://ourworldindata.org/technological-change

mobile <- read_csv("2026/data/share-population-mobile-phone.csv") |> 
  janitor::clean_names() |> 
  filter(entity == "World",
         year == 2024) |> 
  pull(proportion_of_individuals_who_own_a_mobile_telephone)


# Around 80% of the world population owned a mobile phone in 2024

df <- tibble(
  x = rep(1:10, 10),
  y = rep(1:10, each = 10)
)

df |> 
  ggplot(aes(x, y)) +
  geom_pictogram()


data <- data.frame(
  x = c('John', 'James', 'Jeff', 'Joe', 'Jake'),
  ht = c(72, 71, 73, 69, 66),
  icon = rep('rocket', 5)
)

ggplot(data, aes(
  label = x,
  values = ht,
  color = icon
)) +
  geom_text(
    stat = "waffle", n_rows = 5, make_proportional = FALSE, size = 5, flip = TRUE,
    family = "Font Awesome 5 Free",
    position = position_nudge(y = -.9), vjust = 0
  ) +
  facet_wrap(~x, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() +
  scale_y_continuous(
    labels = function(x) x * 5,
    expand = c(0, 0),
    limits = c(0, 20)
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c(
      "rocket" = "rocket"
    )
  ) +
  theme(legend.position = "none")
