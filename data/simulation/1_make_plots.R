# -----------------------------------------#
# Plots of the simulated data
#   figures/simulationData.pdf      (a) abilities, (b) responses per item
#   figures/simulationNetworks.pdf  adjacency matrices
# -----------------------------------------#
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(here)

read_sim <- function(net, s) readRDS(here(paste0("data/simulation/sim_", net, "_scenario", s, ".rds")))

## scenarios 1 and 3 are the same for both networks
sims <- list("1: iid"           = read_sim("dense", 1),
             "2: CAR (dense)"   = read_sim("dense", 2),
             "2: CAR (sparse)"  = read_sim("sparse", 2),
             "3: mixture"       = read_sim("dense", 3))
groups <- rep(1:3, each = 40)

# ---- (a) abilities by network block ----
dfAbility <- bind_rows(lapply(names(sims), function(s)
  tibble(scenario = s, block = factor(groups), ability = sims[[s]]$ability)))

plotAbility <- ggplot(dfAbility, aes(x = block, y = ability)) +
  geom_hline(yintercept = 0, col = "grey70", linetype = "dashed") +
  geom_jitter(width = 0.15, size = 1.2, col = "grey50") +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, col = "black") +
  facet_grid(~ scenario) +
  labs(x = "network block", y = "true ability") +
  theme_classic(14)

# ---- (b) share of students in each score, by item ----
dfResponses <- bind_rows(lapply(names(sims), function(s)
  as_tibble(sims[[s]]$data, .name_repair = ~ as.character(1:10)) |>
    pivot_longer(everything(), names_to = "item", values_to = "score") |>
    mutate(scenario = s)))  |>
  count(scenario, item = factor(as.integer(item)), score = factor(score, 0:3), .drop = FALSE) |>
  group_by(scenario, item) |>
  mutate(share = n / sum(n)) |>
  ungroup()

plotResponses <- ggplot(dfResponses, aes(x = item, y = share, fill = score)) +
  geom_col(width = 0.8, col = "white", linewidth = 0.3) +
  scale_fill_manual(values = c("#86b6ef", "#3987e5", "#1c5cab", "#0d366b")) +
  scale_y_continuous(labels = scales::percent, breaks = c(0, 0.5, 1)) +
  facet_grid(~ scenario) +
  labs(x = "item", y = "share of students") +
  theme_classic(14) +
  theme(legend.position = "bottom")

save_plot(here("figures/simulationData.pdf"),
          plot_grid(plotAbility, plotResponses, ncol = 1, align = "v", axis = "lr",
                    labels = c("(a)", "(b)"), rel_heights = c(1, 1.15)),
          base_width = 14, base_height = 8)

# ---- networks ----
dfW <- bind_rows(lapply(c("dense", "sparse"), function(net)
  which(readRDS(here(paste0("data/simulation/sim_W_", net, ".rds"))) == 1, arr.ind = TRUE) |>
    as_tibble() |>
    mutate(network = net)))

plotW <- ggplot(dfW, aes(x = col, y = row)) +
  geom_tile(fill = "black") +
  scale_y_reverse() +
  coord_equal() +
  facet_wrap(~ network) +
  theme_void(14)

save_plot(here("figures/simulationNetworks.pdf"), plotW, base_width = 10, base_height = 5)
