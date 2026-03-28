# 30 Day Chart Challenge
# 2026
# Comparisons
# Day 05
# Experimental

# 📦 Packages ----

library(tidyverse)
library(patchwork)
library(showtext)
library(ggtext)

# ⚙️ Define plot parameters ----

font_add_google("Open Sans")
showtext::showtext_auto()

# ⚒️ Function ----

plot_rolls <- function(n_dice, n_rolls, seed = 42) {
  
  dice_theo <- tibble(
    stack(
      prop.table(
        table(rowSums(expand.grid(rep(list(1:6), n_dice)))
              )
        )
      )
    ) |> 
    select(dice_total = ind, prob_theo = values) |> 
    mutate(n_dice = n_dice, n_rolls = n_rolls,
           .before = dice_total)
  
  set.seed(seed)
  
  sim_res <- list()
  
  for (i in 1:n_dice) {
    sim_res[[i]] <- sample(x = 1:6, size = n_rolls, replace = TRUE)
  }
  
  res <- tibble(
    dice_total = Reduce("+", sim_res)
  ) |> 
    count(dice_total) |> 
    mutate(prop_obs = n / sum(n),
           dice_total = factor(dice_total)) |> 
    select(!n)
  
  dice_res <- dice_theo |> 
    left_join(res) |> 
    select(n_dice, n_rolls, everything()) |> 
    replace_na(list(prop_obs = 0))
  
  ggplot(dice_res) +
    geom_col(aes(x = dice_total, y = prop_obs),
             fill = "#ffbbff") +
    geom_line(aes(x = dice_total, y = prob_theo, group = 1),
                color = "purple", linewidth = 2) +
    labs(x = "Sum of dice", y = "Probability",
         title = paste0("Simulating ", n_rolls, " throws of ", n_dice, " dice"),
         subtitle = "<span style = 'color:#ffbbff;'>Simulated values</span> versus
       <span style = 'color:#a020f0;'>theoretical values</span>",
         caption = "30DayChartChallenge 2026 | Comparisons | Day 05 - Experimental") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey", linewidth = 0.4, linetype = "dotted"),
          panel.background = element_rect(fill = "black", colour = "black"),
          plot.background = element_rect(fill = "black", colour = "black"),
          plot.title = element_text(family = "Open Sans", face = "bold", 
                                    colour = "white", size = 80, margin = margin(t = 20, b = 10)),
          plot.subtitle = element_markdown(family = "Open Sans", size = 50, 
                                           colour = "white", margin = margin(b = 10)),
          plot.caption = element_text(family = "Open Sans", colour = "white",
                                      size = 40, hjust = 0.5, 
                                      margin = margin(t = 20, b = 10)),
          axis.ticks = element_blank(),
          axis.title.x = element_text(family = "Open Sans", colour = "white", size = 60,
                                      margin = margin(t = 25)),
          axis.title.y = element_text(family = "Open Sans", colour = "white", size = 60,
                                      margin = margin(r = 25)),
          axis.text = element_text(family = "Open Sans", colour = "white",
                                   size = 40))
  
}

# 📊 Plot ----

p <- plot_rolls(n_dice = 5, n_rolls = 10000, seed = 42)

ggsave("2026/figs/30DCC_2026_05.png", p, dpi = 320, width = 12, height = 6)

