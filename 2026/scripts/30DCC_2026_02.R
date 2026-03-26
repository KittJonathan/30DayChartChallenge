# 30 Day Chart Challenge
# 2026
# Comparisons
# Day 02
# Pictogram

# 📦 Packages ----

library(tidyverse)
library(palmerpenguins)
library(ggpop)
# library(ggtext)
library(showtext)

# ⚙️ Define plot parameters ----

font_add_google("Open Sans")
showtext::showtext_auto()


# 📄 Data ----

# Source : Our World In Data - Share of the population that owns a mobile phone
# https://ourworldindata.org/technological-change

mobile <- read_csv("2026/data/share-population-mobile-phone.csv") |> 
  janitor::clean_names() |> 
  filter(entity == "World",
         year == 2024) |> 
  select(yes = proportion_of_individuals_who_own_a_mobile_telephone) |> 
  mutate(no = 100 - yes) |> 
  pivot_longer(cols = everything(),
               names_to = "owns_a_phone",
               values_to = "prop") |> 
  process_data(group_var = owns_a_phone,
               sum_var = prop) |> 
  mutate(icon = "mobile-screen")

# 📊 Plot ----

penguins |> 
  count(species)

tibble(
  x = c(1:5),
  y = c(6:10)
) |> 
  ggplot(aes(x, y)) +
  geom_icon_point(icon = "linux", size = 3) 

p <- tibble(
  species = c(rep("Chinstrap", 68), rep("Gentoo", 124), 
              rep("Adelie", 152)), 
  x = c(rep(1:6, each = 10), rep(7, 8),
        rep(1:12, each = 10), rep(13, 4),
        rep(1:15, each = 10), rep(16, 2)),
  y = c(rep(1:10, times = 6), 10:3,
        rep(12:21, times = 12), 21:18,
        rep(23:32, times = 15), 32, 31)
) |> 
  ggplot(aes(x, y, color = species)) +
  geom_icon_point(icon = "linux", size = 1) +
  theme(legend.position = "none")

ggsave("2026/figs/30DCC_2026_02_draft.png", p, dpi = 320, width = 12, height = 6)


p1 <- ggplot() +
  geom_text(aes(x = 0, y = 1.5, label = "Approximately"),
            family = "Open Sans", color = "white", size = 35) +
  geom_text(aes(x = 0, y = 0.5, label = "80%"),
            family = "Open Sans", color = "#ffa600", size = 35) +
  geom_text(aes(x = 0, y = -0.5, label = "of the world population"),
            family = "Open Sans", color = "white", size = 35) +
  geom_text(aes(x = 0, y = -1.5, label = "owns a mobile phone"),
            family = "Open Sans", color = "white", size = 35) +
  scale_y_continuous(limits = c(-4, 4)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"))

p2 <- ggplot(data = mobile, aes(icon = icon, color = type)) +
  geom_pop(size = 1.5,
           dpi = 200,
           show.legend = FALSE,
           arrange = TRUE) +
  scale_color_manual(values = c("yes" = "#ffa600",
                                "no" = "#bc5090")) +
  theme_void() +
  theme(panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"))

p <- p1 + p2 +
  plot_annotation(
    caption = "30DayChartChallenge 2026 | Comparisons | Day 02 - Pictogram | Source: Our World In Data") &
  theme(plot.background = element_rect(fill = "black", colour = "black"),
        plot.caption = element_text(family = "Open Sans", colour = "white", hjust = 0.5,
                                    size = 30))

ggsave("2026/figs/30DCC_2026_02.png", p, dpi = 320, width = 12, height = 6)
