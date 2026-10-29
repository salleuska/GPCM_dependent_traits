# -----------------------------------------#
# Plot of the simulated data, figures/simulationData.pdf
#   (a) networks  (b) latent trait  (c) responses
# -----------------------------------------#
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(here)

read_sim <- function(net, s) readRDS(here(paste0("data/simulation/sim_", net, "_scenario", s, ".rds")))

## scenarios 1 and 3 are the same for both networks
sims <- list("1: iid"          = read_sim("dense", 1),
             "2: CAR (dense)"  = read_sim("dense", 2),
             "2: CAR (sparse)" = read_sim("sparse", 2),
             "3: mixture"      = read_sim("dense", 3))
scenarioLevels <- names(sims)
blockColors <- c("#2a78d6", "#eb6834", "#1baf7a")

# ---- (a) networks ----
dfW <- bind_rows(lapply(c("dense", "sparse"), function(net)
  which(read_sim(net, 1)$W == 1, arr.ind = TRUE) |>
    as_tibble() |>
    mutate(network = net)))

plotW <- ggplot(dfW, aes(x = col, y = row)) +
  geom_tile(fill = "black") +
  scale_y_reverse() +
  coord_equal() +
  facet_wrap(~ network, ncol = 1) +
  theme_void(14) +
  theme(panel.border = element_rect(fill = NA, colour = "grey70"))

# ---- (b) latent trait: all students (gray) and by block (lines) ----
dfAbility <- bind_rows(lapply(scenarioLevels, function(s)
  tibble(scenario = s, block = factor(rep(1:3, each = 40)), ability = sims[[s]]$ability))) |>
  mutate(scenario = factor(scenario, scenarioLevels))

plotAbility <- ggplot(dfAbility, aes(x = ability)) +
  geom_density(fill = "grey85", col = NA, bw = 0.35) +
  stat_function(fun = dnorm, linetype = "dashed") +
  geom_density(aes(col = block), bw = 0.35, linewidth = 0.8) +
  geom_rug(aes(col = block), alpha = 0.5, length = unit(0.04, "npc")) +
  scale_color_manual(values = blockColors) +
  facet_grid(~ scenario) +
  xlim(-3.5, 3.5) +
  labs(x = "latent trait", y = "density") +
  theme_classic(14)

# ---- (c) share of students in each score, by item ----
dfResponses <- bind_rows(lapply(scenarioLevels, function(s)
  as_tibble(sims[[s]]$data, .name_repair = ~ as.character(1:10)) |>
    pivot_longer(everything(), names_to = "item", values_to = "score") |>
    mutate(scenario = s))) |>
  count(scenario = factor(scenario, scenarioLevels), item = factor(as.integer(item)),
        score = factor(score, 0:3), .drop = FALSE) |>
  group_by(scenario, item) |>
  mutate(share = n / sum(n)) |>
  ungroup()

plotResponses <- ggplot(dfResponses, aes(x = item, y = share, fill = score)) +
  geom_col(width = 0.8, col = "white", linewidth = 0.3) +
  scale_fill_manual(values = c("#86b6ef", "#3987e5", "#1c5cab", "#0d366b")) +
  scale_y_continuous(labels = scales::percent, breaks = c(0, 0.5, 1)) +
  facet_grid(~ scenario) +
  labs(x = "item", y = "share of students") +
  theme_classic(14)

# ---- combine ----
save_plot(here("figures/simulationData.pdf"),
          plot_grid(plotW,
                    plot_grid(plotAbility, plotResponses, ncol = 1, align = "v",
                              axis = "lr", labels = c("(b)", "(c)")),
                    labels = c("(a)", ""), rel_widths = c(1, 3.5)),
          base_width = 18, base_height = 8)
