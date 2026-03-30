# 30 Day Chart Challenge
# 2026
# Distributions
# Day 07
# Multiscale

# 📦 Packages ----

library(tidyverse)
# library(gapminder)
library(palmerpenguins)
# library(ggdist)
library(patchwork)
library(showtext)

# ⚙️ Define plot parameters ----

theme_set(theme_bw(base_family = "Open Sans"))
font_add_google("Open Sans")
showtext::showtext_auto()

my_cols <- c("Adelie" = "darkorange",
             "Chinstrap" = "purple",
             "Gentoo" = "cyan4")

tbl_cols <- c("yes" = "black", "no" = "lightgrey")

bill_length_ratio <- range(penguins$bill_length_mm, na.rm = TRUE)
bill_depth_ratio <- range(penguins$bill_depth_mm, na.rm = TRUE)

# 📊 Plot ----

tbl <- penguins |> 
  count(island, species) |> 
  complete(island, species, fill = list(n = 0)) |> 
  mutate(xmin = c(rep(0, 3), rep(1, 3), rep(2, 3)),
         xmax = c(rep(1, 3), rep(2, 3), rep(3, 3)),
         ymin = rep(c(0, 1, 2), 3),
         ymax = rep(c(1, 2, 3), 3),
         x = xmin + (xmax - xmin) / 2,
         y = ymin + (ymax - ymin) / 2)

p1 <- penguins |> 
  filter(species == "Chinstrap") |> 
  drop_na(bill_length_mm, bill_depth_mm) |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm,
             color = species)) +
  geom_point() +
  labs(x = "Bill length (mm)",
       y = "Bill depth (mm)") +
  scale_color_manual(values = my_cols) +
  scale_x_continuous(limits = bill_length_ratio) +
  scale_y_continuous(limits = bill_depth_ratio,
                     breaks = seq(12, 22, 2)) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(family = "Open Sans", size = 20),
        axis.text = element_text(family = "Open Sans", size = 18))

t1 <- ggplot() +
  geom_rect(data = tbl |> filter(n != 68), 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "white", color = "lightgrey") +
  geom_rect(data = tbl |> filter(n == 68), 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "black", color = "lightgrey") +
  geom_text(data = tbl |> filter(n != 68),
            aes(x = x, y = y, label = n), size = 14, color = "lightgrey") +
  geom_text(data = tbl |> filter(n == 68),
            aes(x = x, y = y, label = n), size = 14, color = "white") +
  geom_text(data = tbl |> distinct(island, x),
            aes(x = x, y = -0.2, label = island), size = 14) +
  geom_text(data = tbl |> distinct(species, y),
            aes(x = -0.1, y = y, label = species, color = species), hjust = 1,
            size = 14) +
  scale_color_manual(values = my_cols) +
  scale_x_continuous(limits = c(-0.5, 3.5)) +
  theme_void() +
  theme(text = element_text(family = "Open Sans", face = "bold"),
        legend.position = "none")

p2 <- penguins |> 
  filter(island == "Dream") |> 
  drop_na(bill_length_mm, bill_depth_mm) |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm,
             color = species)) +
  geom_point() +
  labs(x = "Bill length (mm)",
       y = "Bill depth (mm)") +
  scale_color_manual(values = my_cols) +
  scale_x_continuous(limits = bill_length_ratio) +
  scale_y_continuous(limits = bill_depth_ratio,
                     breaks = seq(12, 22, 2)) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(family = "Open Sans", size = 20),
        axis.text = element_text(family = "Open Sans", size = 18))

t2 <- ggplot() +
  geom_rect(data = tbl |> filter(island != "Dream"), 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "white", color = "lightgrey") +
  geom_rect(data = tbl |> filter(island == "Dream"),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "black", color = "lightgrey") +
  geom_text(data = tbl |> filter(island != "Dream"),
            aes(x = x, y = y, label = n), size = 10, color = "lightgrey") +
  geom_text(data = tbl |> filter(island == "Dream"),
            aes(x = x, y = y, label = n), size = 10, color = "white") +
  geom_text(data = tbl |> distinct(island, x),
            aes(x = x, y = -0.2, label = island), size = 8) +
  geom_text(data = tbl |> distinct(species, y),
            aes(x = -0.1, y = y, label = species, color = species), hjust = 1,
            size = 8) +
  scale_color_manual(values = my_cols) +
  scale_x_continuous(limits = c(-2.5, 4.5)) +
  theme_void() +
  theme(text = element_text(family = "Open Sans", face = "bold"),
        legend.position = "none")

p3 <- penguins |> 
  drop_na(bill_length_mm, bill_depth_mm) |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm,
             color = species)) +
  geom_point() +
  labs(x = "Bill length (mm)",
       y = "Bill depth (mm)") +
  scale_color_manual(values = my_cols) +
  scale_x_continuous(limits = bill_length_ratio) +
  scale_y_continuous(limits = bill_depth_ratio,
                     breaks = seq(12, 22, 2)) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(family = "Open Sans", size = 20),
        axis.text = element_text(family = "Open Sans", size = 18))

t3 <- ggplot() +
  # geom_rect(data = tbl |> filter(island != "Dream"), 
  #           aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  #           fill = "white", color = "lightgrey") +
  geom_rect(data = tbl,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "black", color = "lightgrey") +
  # geom_text(data = tbl |> filter(island != "Dream"),
  #           aes(x = x, y = y, label = n), size = 10, color = "lightgrey") +
  geom_text(data = tbl,
            aes(x = x, y = y, label = n), size = 10, color = "white") +
  geom_text(data = tbl |> distinct(island, x),
            aes(x = x, y = -0.2, label = island), size = 8) +
  geom_text(data = tbl |> distinct(species, y),
            aes(x = -0.1, y = y, label = species, color = species), hjust = 1,
            size = 8) +
  scale_color_manual(values = my_cols) +
  scale_x_continuous(limits = c(-2.5, 4.5)) +
  theme_void() +
  theme(text = element_text(family = "Open Sans", face = "bold"),
        legend.position = "none")


p <- (t1 / p1) | (t2 / p2) | (t3 / p3)

ggsave("2026/figs/30DCC_2026_07.png", p, dpi = 320, width = 12, height = 6)
