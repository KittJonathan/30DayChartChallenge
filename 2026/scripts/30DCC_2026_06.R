# 30 Day Chart Challenge
# 2026
# Comparisons
# Day 06 - Data Day
# Reporters Without Borders

# https://rsf.org/en/rsf-world-press-freedom-index-2025-economic-fragility-leading-threat-press-freedom?year=2025&data_type=general

# 📦 Packages ----

library(tidyverse)
library(showtext)
library(ggtext)

# ⚙️ Define plot parameters ----

font_add_google("Open Sans")
showtext::showtext_auto()

theme_set(theme_bw())

# 📄 Data ----

rsf_index_2023 <- read_csv2("2026/data/2023.csv")
rsf_index_2024 <- read_csv2("2026/data/2024.csv")
rsf_index_2025 <- read_csv2("2026/data/2025.csv")

rsf_index_2023 <- rsf_index_2023 |> 
  summarise(Global = mean(Score),
            Economic = mean(`Economic Context`),
            Political = mean(`Political Context`),
            Legislative = mean(`Legal Context`),
            Social = mean(`Social Context`),
            Security = mean(Safety),
            .by = Zone) |> 
  mutate(Zone = case_when(Zone == "UE Balkans" ~ "EU Balkans",
                          Zone == "Asie-Pacifique" ~ "Asia - Pacific",
                          Zone == "Amériques" ~ "Americas",
                          Zone == "Afrique" ~ "Africa",
                          Zone == "MENA" ~ "Middle East - North Africa",
                          .default = "EEAC")) |> 
  pivot_longer(cols = -Zone, 
               names_to = "Indicator",
               values_to = "Score") |> 
  mutate(Year = 2023, .before = Zone)

rsf_index_2024 <- rsf_index_2024 |> 
  summarise(Global = mean(Score),
            Economic = mean(`Economic Context`),
            Political = mean(`Political Context`),
            Legislative = mean(`Legal Context`),
            Social = mean(`Social Context`),
            Security = mean(Safety),
            .by = Zone) |> 
  mutate(Zone = case_when(Zone == "UE Balkans" ~ "EU Balkans",
                          Zone == "Asie-Pacifique" ~ "Asia - Pacific",
                          Zone == "Amériques" ~ "Americas",
                          Zone == "Afrique" ~ "Africa",
                          Zone == "MENA" ~ "Middle East - North Africa",
                          .default = "EEAC")) |> 
  pivot_longer(cols = -Zone, 
               names_to = "Indicator",
               values_to = "Score") |> 
  mutate(Year = 2024, .before = Zone)

rsf_index_2025 <- rsf_index_2025 |> 
  summarise(Global = mean(`Score 2025`),
            Economic = mean(`Economic Context`),
            Political = mean(`Political Context`),
            Legislative = mean(`Legal Context`),
            Social = mean(`Social Context`),
            Security = mean(Safety),
            .by = Zone) |> 
  mutate(Zone = case_when(Zone == "UE Balkans" ~ "EU Balkans",
                          Zone == "Asie-Pacifique" ~ "Asia - Pacific",
                          Zone == "Am\xe9riques" ~ "Americas",
                          Zone == "Afrique" ~ "Africa",
                          Zone == "MENA" ~ "Middle East - North Africa",
                          .default = "EEAC")) |> 
  pivot_longer(cols = -Zone, 
               names_to = "Indicator",
               values_to = "Score") |> 
  mutate(Year = 2025, .before = Zone)

rsf_index <- bind_rows(rsf_index_2023, rsf_index_2024, rsf_index_2025) |> 
  mutate(Indicator = factor(Indicator, levels = c("Global", "Economic", "Political", "Legislative",
                                                  "Social", "Security")),
         Year = str_replace_all(Year, "20", "'"))

rm(rsf_index_2023, rsf_index_2024, rsf_index_2025)

# 📊 Plot ----

p <- ggplot(rsf_index,
       aes(x = Year, y = Score)) +
  geom_line(aes(color = Zone, group = Zone), linewidth = 1) +
  geom_point(aes(color = Zone), size = 3) +
  scale_color_manual(values = c("EU Balkans" = "#eecc66",
                                "Asia - Pacific" = "#ee99aa",
                                "Americas" = "#6699cc",
                                "Africa" = "#997700",
                                "EEAC" = "#994455",
                                "Middle East - North Africa" = "#004488")) +
  facet_wrap(~Indicator, nrow = 1) +
  labs(title = "RSF - World Press Freedom Index evolution from 2023 to 2025",
       subtitle = "<span style = 'color:#997700;'>Africa</span>, <span style = 'color:#6699cc;'>Americas</span>,
       <span style = 'color:#ee99aa;'>Asia - Pacific</span>, <span style = 'color:#994455;'>EEAC</span>,
       <span style = 'color:#eecc66;'>EU Balkans</span>, <span style = 'color:#004488;'>Middle East - North Africa</span>",
       caption = "30DayChartChallenge 2026 | Comparisons | Day 06 - Data Day | Source: Reporters Withouth Borders") +
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.title = element_text(family = "Open Sans", size = 40),
        axis.text = element_text(family = "Open Sans", size = 35),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dotted", color = "grey70", linewidth = 0.5),
        plot.title = element_text(family = "Open Sans", face = "bold", colour = "black", 
                                  size = 80, hjust = 0.5, margin = margin(t = 10, b = 20)),
        plot.subtitle = element_markdown(family = "Open Sans", face = "bold", color = "black", size = 60, hjust = 0.5, margin = margin(b = 30)),
        plot.caption = element_text(family = "Open Sans", colour = "black",
                                    size = 40, hjust = 0.5, 
                                    margin = margin(t = 20, b = 10)),
        strip.background = element_rect(colour="black", fill="black"),
        strip.text = element_text(family = "Open Sans", colour = "white", size = 50))

# 💾 Save plot ----

ggsave("2026/figs/30DCC_2026_06.png", p, dpi = 320, width = 12, height = 6)
