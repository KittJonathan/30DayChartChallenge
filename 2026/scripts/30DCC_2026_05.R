# 30 Day Chart Challenge
# 2026
# Comparisons
# Day 05
# Experimental

# 📦 Packages ----

library(tidyverse)
library(ggfx)
# library(sysfonts)
# font_add_google("Roboto")
# font_add_google("Ubuntu")
# font_add_google("Open Sans")
# showtext::showtext_auto()

# ⚒️ Function ----

stack(prop.table(table(do.call("+", expand.grid(rep(list(1:6), 2))))))

n_dice <- 4
n_rolls <- 10000
seed <- 42

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
             fill = "#ebdaf2") +
    with_outer_glow(
      geom_line(aes(x = dice_total, y = prob_theo, group = 1),
                color = "#b7a3d9", linewidth = 2),
      color = "#b7a3d9", sigma = 2
    ) +
    labs(x = "Sum of dice", y = "Probability",
         title = paste0(n_rolls, " throws of ", n_dice, " dice")) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey", linewidth = 0.4, linetype = "dotted"),
          panel.background = element_rect(fill = "black", colour = "black"),
          plot.background = element_rect(fill = "black", colour = "black"),
          plot.title = element_text(colour = "white"),
          axis.ticks = element_blank(),
          axis.title = element_text(colour = "white"),
          axis.text = element_text(colour = "white"))
  
}

plot_rolls(n_dice = 4, n_rolls = 10000000, seed = 42)

# 📊 Plot ----

ggplot(dice_res) +
  geom_col(aes(x = dice_total, y = prop_obs)) +
  geom_line(aes(x = dice_total, y = prob_theo, group = 1)) +
  geom_point(aes(x = dice_total, y = prob_theo))

# 📄 Data ----

# 100 throws of 2 dice
res_100throws_2dice <- sim_roll(n_dice = 2, n_rolls = 100)

# 100 throws of 5 dice
res_100throws_5dice <- sim_roll(n_dice = 5, n_rolls = 100)

# 10000 throws of 5 dice
res_10000throws_5dice <- sim_roll(n_dice = 5, n_rolls = 10000)

# 1000 throws of 2 dice
res_1000throws_2dice <- sim_roll(n_dice = 2, n_rolls = 1000)

# 10000 throws of 2 dice
res_10000throws_2dice <- sim_roll(n_dice = 2, n_rolls = 10000)

ggplot(res_100throws_2dice, aes(x = dice_total, y = n)) + geom_col()
ggplot(res_100throws_5dice, aes(x = dice_total, y = n)) + geom_col()
ggplot(res_10000throws_5dice, aes(x = dice_total, y = n)) + geom_col()



dice <- tibble(
  die1 = sim_res[[1]],
  die2 = sim_res[[2]],
  die3 = sim_res[[3]],
  die4 = sim_res[[4]],
  die5 = sim_res[[5]]
)

tbl_res <- dice |> 
  mutate(total = die1 + die2 + die3 + die4 + die5) |> 
  pull(total)

list_res <- Reduce("+", sim_res)

# 📊 Plot ----