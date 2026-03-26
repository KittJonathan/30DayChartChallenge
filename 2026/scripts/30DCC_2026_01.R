# 30 Day Chart Challenge
# 2026
# Comparisons
# Day 01
# Part-to-Whole

# 📦 Packages ----

library(tidyverse)
library(palmerpenguins)
library(ggtext)
library(showtext)

# ⚙️ Define plot parameters ----

font_add_google("Open Sans")
showtext::showtext_auto()

# 📊 Plot ----

p <- penguins |> 
  count(island, name = "count") |> 
  mutate(fraction = count / sum(count),
         ymax = cumsum(fraction),
         ymin = lag(ymax, default = 0),
         label_y = (ymax + ymin) / 2) |> 
  ggplot(aes(xmin = 3, xmax = 4,
             ymin = ymin, ymax = ymax,
             fill = island)) +
  geom_rect(show.legend = FALSE,
            color = "black") +
  scale_fill_manual(
    values = c("Biscoe" = "#816ec7",
               "Dream" = "#e5989b",
               "Torgersen" = "#ffcdb2")
  ) +
  geom_text(x = 3.5, aes(y = label_y, label = count),
            size = 25, family = "Open Sans") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  labs(title = "How many penguins?",
       subtitle = "Number of individuals on each island: <span style = 'color:#816ec7;'>Biscoe</span>,
       <span style = 'color:#e5989b;'>Dream</span>,
       <span style = 'color:#ffcdb2;'>Torgersen</span>",
       caption = "30DayChartChallenge 2026 | Comparisons | Day 01 - Part-to-Whole | Source: {palmerpenguins}") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    plot.title = element_text(family = "Open Sans", face = "bold", 
                              colour = "white", size = 80, hjust = 0.5,
                              margin = margin(t = 10)),
    plot.subtitle = element_markdown(family = "Open Sans", 
                                     colour = "white", size = 60, hjust = 0.5,
                                     margin = margin(t = 10)),
    plot.caption = element_text(family = "Open Sans", colour = "white",
                                size = 40, hjust = 0.5, 
                                margin = margin(b = 10))
  )

# 💾 Save plot ----

ggsave("2026/figs/30DCC_2026_01.png", p, dpi = 320,
       width = 12, height = 6)
