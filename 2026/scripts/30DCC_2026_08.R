# 30 Day Chart Challenge
# 2026
# Distributions
# Day 08
# Circular

# 📦 Packages ----

library(tidyverse)
library(palmerpenguins)

# 📊 Plot ----

p <- penguins |> 
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) |> 
  arrange(desc(bill_ratio)) |> 
  drop_na(bill_ratio) |> 
  rowid_to_column() |> 
  select(rowid, species, bill_ratio) |> 
  ggplot() +
  geom_segment(aes(x = rowid, xend = rowid, 
                   y = 0, yend = bill_ratio,
                   color = species),
               linewidth = 0.3) +
  coord_polar(theta = "x", start = 0)

ggsave("2026/figs/30DCC_2026_08.png", p, dpi = 320, width = 12, height = 6)
  

# 📄 Data ----

# https://public.opendatasoft.com/explore/assets/donnees-synop-essentielles-omm/

wind <- read_csv2("2026/data/donnees-synop-essentielles-omm.csv")

wind |> 
  janitor::clean_names() |> 
  select(date, direction_du_vent_moyen_10_mn, vitesse_du_vent_moyen_10_mn) |> 
  filter(year(date) == 2025)

# 📊 Plot ----

# Nettoyage des données
penguins_df <- penguins %>%
  drop_na() %>%
  # On crée une colonne avec un petit décalage angulaire par espèce pour éviter la superposition parfaite
  # mais ici on va plutôt utiliser facet_wrap ou color
  select(species, flipper_length_mm)

# Transformation pour l'histogramme circulaire
# On découpe les longueurs de nageoires en classes (bins)
# On utilise l'angle comme variable principale

# Création du graphique
p <- ggplot(penguins_df, aes(x = flipper_length_mm, fill = species)) +
  # Histogramme classique mais on va le passer en coordonnées polaires
  geom_histogram(aes(y = after_stat(count)), 
                 binwidth = 5, 
                 alpha = 0.7, 
                 position = "identity",  # Superposition
                 color = "white", 
                 size = 0.2) +
  # Transformation en coordonnées polaires : c'est ici que la magie opère
  coord_polar(theta = "x", start = 0) +
  # Échelle pour l'axe X (qui devient l'angle)
  scale_x_continuous(breaks = seq(170, 240, by = 10)) +
  # Palette de couleurs inspirée des manchots
  scale_fill_manual(values = c("Adelie" = "#FF8C42", 
                               "Chinstrap" = "#4C9A8E", 
                               "Gentoo" = "#9B5E2E")) +
  labs(
    title = "Distribution circulaire des longueurs de nageoires",
    subtitle = "#30DayChartChallenge | Distributions & Circular | Palmer Penguins",
    caption = "Source: palmerpenguins | Chaque barre représente un angle de nageoire (en mm)",
    fill = "Espèce",
    x = "Longueur de nageoire (mm) → Angle",
    y = "Nombre d'individus"
  ) +
  theme_minimal() +
  theme(
    # Personnalisation pour un rendu "circular" propre
    axis.text.y = element_blank(),  # On cache l'axe radial (counts)
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 8, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.minor = element_blank()
  )

ggsave("2026/figs/30DCC_2026_08.png", p, dpi = 320, width = 12, height = 6)
