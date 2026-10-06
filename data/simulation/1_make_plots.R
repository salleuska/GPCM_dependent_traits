# -----------------------------------------#
# make_plots.R
# Consumes: sim_W_<network>.rds, sim_<network>_scenario{1,2,3}.rds
# Produces: figures/simulationData_<network>.pdf, network in {dense, sparse}
# -----------------------------------------#

library(dplyr)
library(ggplot2)
library(cowplot)
library(tidyr)
library(here)

for (net in c("dense", "sparse")) {

# -----------------------------------------#
# Load saved data
# -----------------------------------------#
W         <- readRDS(here(paste0("data/simulation/sim_W_", net, ".rds")))
Scenario1 <- readRDS(here(paste0("data/simulation/sim_", net, "_scenario1.rds")))
Scenario2 <- readRDS(here(paste0("data/simulation/sim_", net, "_scenario2.rds")))
Scenario3 <- readRDS(here(paste0("data/simulation/sim_", net, "_scenario3.rds")))

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

dir.create(here("figures"), showWarnings = FALSE)
save_plot(here(paste0("figures/simulationData_", net, ".pdf")), simPlot,
          base_width = 20, base_height = 10)

}