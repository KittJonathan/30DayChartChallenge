# 30 Day Chart Challenge
# 2026
# Comparisons
# Day 01
# Part-to-Whole

# 📦 Packages ----

library(tidyverse)
library(khroma)
# library(ggthemes)
# library(tidyplots)
theme_set(theme_bw())

# 📄 Data ----

# Source : Our World In Data - Electricity Mix
# https://ourworldindata.org/electricity-mix

elec_mix <- read_csv("2026/data/electricity-prod-source-stacked.csv")

elec_df <- elec_mix |> 
  filter(Entity == "World", Year == 2024) |> 
  select(!(Entity:Year)) |> 
  pivot_longer(everything(),
               names_to = "Source",
               values_to = "TWh") |> 
  arrange(TWh) |> 
  mutate(Source = fct_inorder(Source))

elec_labels <- elec_mix |> 
  filter(Entity == "World", Year == 2024) |> 
  select(!(Entity:Year)) |> 
  pivot_longer(everything(),
               names_to = "Source",
               values_to = "TWh") |> 
  arrange(desc(TWh)) |> 
  mutate(seg_start_x = 1.5,
         seg_end_x = 1.75,
         seg_start_y = cumsum(lag(TWh, default = 0)),
         seg_start_y = seg_start_y + (TWh / 2),
         seg_end_y = case_when(Source == "Bioenergy" ~ seg_start_y + 750,
                               Source == "Other renewables" ~ seg_start_y + 1750,
                               .default = seg_start_y),
         label = paste0(Source, " (", round(TWh), " TWh)")
  )

# 📊 Plot ----

my_cols <- c("Coal" = "#000000",
             "Gas" = "#d55e00",
             "Hydropower" = "#56b4e9",
             "Nuclear" = "#e69f00",
             "Wind" = "#cc79a7",
             "Solar" = "#f0e442",
             "Oil" = "#ef9877",
             "Bioenergy" = "#335a21",
             "Other renewables" = "#009e73")

p <- ggplot() +
  geom_col(data = elec_df, aes(x = 1, y = TWh, fill = Source),
           position = "stack", show.legend = FALSE, width = 1) +
  scale_fill_manual(values = my_cols) +
  geom_segment(data = elec_labels,
               aes(x = seg_start_x, xend = seg_end_x,
                   y = seg_start_y, yend = seg_end_y),
               linewidth = 0.3) +
  geom_text(data = elec_labels,
            aes(x = 1.76, y = seg_end_y, label = label),
            hjust = 0) +
  scale_x_continuous(limits = c(0.5, 2)) +
  labs(title = "Electricity mix",
       subtitle = "Worldwide - 2025",
       caption = "30DayChartChallenge 2026 | Comparisons | Day 01 - Part-to-Whole | Source: Our World In Data") +
  theme_void() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold", size = 15, hjust = 0.5, margin = margin(t = 15)),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 0.5, margin = margin(b = 15)))

ggsave("2026/figs/30DCC_2026_01.png", p, dpi = 320, width = 12, height = 6)

