# 30 Day Chart Challenge
# 2026
# Comparisons
# Day 04
# Slope

# 📦 Packages ----

library(tidyverse)
library(palmerpenguins)
library(showtext)

# ⚙️ Define plot parameters ----

font_add_google("Open Sans")
showtext::showtext_auto()

theme_set(theme_bw())

# 📊 Plot ----

p <- penguins |> 
  ggplot(aes(x = flipper_length_mm, y = bill_length_mm,
             color = species, fill = species)) +
  geom_point(shape = 21, size = 3, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5) +
  scale_color_manual(
    values = c("Adelie" = "darkorange",
               "Chinstrap" = "purple",
               "Gentoo" = "cyan4")
  ) +
  scale_fill_manual(
    values = c("Adelie" = "darkorange",
               "Chinstrap" = "purple",
               "Gentoo" = "cyan4")
  ) +
  facet_wrap(~species) +
  labs(title = "Relation between flipper length and bill length",
       x = "Flipper length (mm)",
       y = "Bill length (mm)",
       caption = "30DayChartChallenge 2026 | Comparisons | Day 04 - Slope | Source: {palmerpenguins}") +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "white", linetype = "dotted",
                                        linewidth = 0.2),
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        plot.title = element_text(family = "Open Sans", face = "bold", colour = "white", 
                                  size = 80, hjust = 0.5, margin = margin(t = 10, b = 20)),
        plot.caption = element_text(family = "Open Sans", colour = "white",
                                    size = 40, hjust = 0.5, 
                                    margin = margin(t = 20, b = 10)),
        axis.title = element_text(family = "Open Sans", colour = "white", size = 40),
        axis.text = element_text(family = "Open Sans", colour = "white", size = 34),
        axis.text.x = element_text(margin = margin(t = 10, b = 10)),
        axis.text.y = element_text(margin = margin(l = 10, r = 10)),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text = element_text(family = "Open Sans", colour = "black", size = 50))

# 💾 Save plot ----

ggsave("2026/figs/30DCC_2026_04.png", p, dpi = 320, width = 12, height = 6) 