# -----------------------------------------#
# make_plots.R
# Consumes: sim_W.rds, sim_scenario1.rds, sim_scenario2.rds, sim_scenario3.rds
# Produces: plots/simulationData.pdf
# -----------------------------------------#

library(dplyr)
library(ggplot2)
library(cowplot)
library(tidyr)

# -----------------------------------------#
# Load saved data
# -----------------------------------------#
W         <- readRDS("sim_W.rds")
Scenario1 <- readRDS("sim_scenario1.rds")
Scenario2 <- readRDS("sim_scenario2.rds")
Scenario3 <- readRDS("sim_scenario3.rds")

N <- nrow(W)
groups <- c(rep(1, N/3), rep(2, N/3), rep(3, N/3))

# -----------------------------------------#
# Plot adjacency matrix
# -----------------------------------------#
plotW <- as_tibble(W) %>%
  mutate(row = 1:N) %>%
  pivot_longer(-row, names_to = "col", values_to = "value") %>%
  mutate(col = as.numeric(gsub("V", "", col)),
         row = factor(row, levels = 1:N),
         col = factor(col, levels = N:1)) %>%
  ggplot() +
  geom_tile(aes(x = row, y = col, fill = factor(value)),
            col = "grey50", show.legend = FALSE) +
  scale_fill_manual(values = c("white", "black")) +
  xlab("") + ylab("") +
  theme_classic(14) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        aspect.ratio = 0.9/1)

# -----------------------------------------#
# Ability plots
# -----------------------------------------#
dfAbility <- tibble(
  x = 1:N,
  S1 = Scenario1$ability,
  S2 = Scenario2$ability,
  S3 = Scenario3$ability
)

plotMarginalAbility <- dfAbility %>%
  pivot_longer(-x, names_to = "scenario", values_to = "ability") %>%
  ggplot(aes(x = scenario, y = ability)) +
  geom_violin(fill = "black", alpha = 0.3) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
  xlab("Simulation scenario") + ylab("Ability") +
  theme_classic(14)

plotConditionalAbility <- dfAbility %>%
  bind_cols(groups = as.factor(groups)) %>%
  pivot_longer(-c(x, groups), names_to = "scenario", values_to = "ability") %>%
  ggplot(aes(x = groups, y = ability)) +
  geom_violin(fill = "black", alpha = 0.3) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
  labs(x = "Latent group", y = "Ability") +
  facet_wrap(~scenario) +
  theme_classic(14)

# -----------------------------------------#
# Combine and save plots
# -----------------------------------------#
simPlot <- plot_grid(
  plotW,
  plot_grid(plotMarginalAbility, plotConditionalAbility,
            nrow = 2, labels = c("(b)", "(c)")),
  labels = c("(a)", "")
)

dir.create("plots", showWarnings = FALSE)
save_plot("plots/simulationData.pdf", simPlot,
          base_width = 20, base_height = 10)