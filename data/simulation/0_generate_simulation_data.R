# -----------------------------------------#
# simulate_data.R
# Generates, for each network in {dense, sparse}:
#   sim_W_<network>.rds, sim_<network>_scenario1.rds,
#   sim_<network>_scenario2.rds, sim_<network>_scenario3.rds
# Each scenario contains: $ability, $difficulties, $discrimination, $data,
# $W, $network
#
# Networks, both stochastic block models with 3 blocks:
#   dense   within 0.9,  between 0.1   (mean degree ~ 43, the original one)
#   sparse  within 0.3,  between 0.02  (mean degree ~ 13, close to the
#                                       students network, mean degree 11.6)
#
# Abilities have mean 0 and variance 1 *in distribution*. They are not
# centered/scaled with scale(): that forces sample mean 0 and sample sd 1 on
# each realization, and for scenario 2 it removes the common level shared by
# all students, which is most of the dependence implied by the CAR
# (see notes/toy_identifiability/PROBLEM.md).
#
# Scenarios 1 and 3 do not depend on the network: the abilities and the
# responses are the same for both networks, only $W changes.
# -----------------------------------------#

library(dplyr)
library(ggplot2)
library(here)
# -----------------------------------------#
# Global setup
# -----------------------------------------#
N <- 120          # number of students
p <- 10           # number of items
set.seed(1234)

# -----------------------------------------#
# 1) Stochastic block adjacency matrices
# -----------------------------------------#
groups <- c(rep(1, N/3), rep(2, N/3), rep(3, N/3))

simulate_sbm <- function(groups, pWithin, pBetween) {
  N <- length(groups)
  W <- matrix(0, N, N)
  for (i in 2:N) {
    for (j in 1:(i - 1)) {
      W[i, j] <- ifelse(groups[i] == groups[j],
                        rbinom(1, 1, pWithin),
                        rbinom(1, 1, pBetween))
      W[j, i] <- W[i, j]
    }
  }
  W
}

## dense: same seed and code as the original sim_W.rds, so it is the same matrix
Ws <- list()
Ws$dense <- simulate_sbm(groups, 0.9, 0.1)

## sparse: redraw until there are no isolated students, the proper CAR needs
## every degree > 0 (M = 1 / degree)
set.seed(4321)
repeat {
  Ws$sparse <- simulate_sbm(groups, 0.3, 0.02)
  if (all(rowSums(Ws$sparse) > 0)) break
}

for (net in names(Ws)) {
  saveRDS(Ws[[net]], here(paste0("data/simulation/sim_W_", net, ".rds")))
  cat(net, "network: mean degree", round(mean(rowSums(Ws[[net]])), 1),
      ", density", round(mean(Ws[[net]][upper.tri(Ws[[net]])]), 3), "\n")
}

# -----------------------------------------#
# 2) Latent abilities
# -----------------------------------------#

## Scenario 1: iid Normal(0,1)
set.seed(123 + 1)
Scenario1 <- lst()
Scenario1$ability <- rnorm(N, 0, 1)

## Scenario 2: CAR model (rho = 0.98, sigma2 = 20), one per network.
## theta_i = x_i / sqrt(Sigma_ii) gives Var(theta_i) = 1 for every student
## without constraining the realization; sigma2 cancels in the division.
## The same standard normal draws z are used for both networks.
set.seed(123 + 2)
z <- rnorm(N)
Scenario2 <- list()
for (net in names(Ws)) {
  S2 <- lst()
  S2$rho     <- 0.98
  S2$sigma2  <- 20
  D_w        <- diag(colSums(Ws[[net]]))
  S2$Sigma_W <- solve((D_w - S2$rho * Ws[[net]]) / S2$sigma2)
  x          <- as.vector(t(chol(S2$Sigma_W)) %*% z)
  S2$ability <- x / sqrt(diag(S2$Sigma_W))
  Scenario2[[net]] <- S2
}

## Scenario 3: Mixture of Normals by group. Group means (-0.5, 0, 0.5) and
## within variance 5/6 give marginal mean 0 and variance 5/6 + 1/6 = 1
set.seed(123 + 3)
Scenario3 <- lst()
Scenario3$trueAbilityMeans <- c(-0.5, 0, 0.5)
Scenario3$trueAbilityVars  <- rep(5/6, 3)
Scenario3$groups           <- groups

set.seed(123)
Scenario3$ability <- numeric(N)
for (i in 1:N) {
  g <- groups[i]
  Scenario3$ability[i] <- rnorm(1, Scenario3$trueAbilityMeans[g],
                                sqrt(Scenario3$trueAbilityVars[g]))
}

# -----------------------------------------#
# 3) Item parameters (common to all scenarios)
# -----------------------------------------#
Betas <- matrix(0, p, 4)
for (i in 1:p) {
  Betas[i, 2:4] <- rnorm(3, 0, 2)
}
alphas <- runif(p, 0.5, 2)

# -----------------------------------------#
# 4) Generate responses
# -----------------------------------------#

gen_responses <- function(N, p, ability, disc, diff) {
  mat <- matrix(0, N, p)
  for (i in 1:N) {
    for (j in 1:p) {
      num  <- exp(cumsum(disc[j] * (ability[i] - diff[j, ])))
      den  <- sum(num)
      prob <- num / den
      mat[i, j] <- sample(0:3, size = 1, prob = prob)
    }
  }
  mat
}

set.seed(2001)
Scenario1$data <- gen_responses(N, p, Scenario1$ability, alphas, Betas)
set.seed(2003)
Scenario3$data <- gen_responses(N, p, Scenario3$ability, alphas, Betas)
for (net in names(Ws)) {
  set.seed(2002)
  Scenario2[[net]]$data <- gen_responses(N, p, Scenario2[[net]]$ability,
                                         alphas, Betas)
}

# -----------------------------------------#
# 5) Save, one file per network and scenario
# -----------------------------------------#
for (net in names(Ws)) {
  scenarios <- list(Scenario1, Scenario2[[net]], Scenario3)
  for (s in seq_along(scenarios)) {
    sc <- scenarios[[s]]
    sc$difficulties   <- Betas
    sc$discrimination <- alphas
    sc$W              <- Ws[[net]]
    sc$network        <- net
    saveRDS(sc, here(paste0("data/simulation/sim_", net, "_scenario", s, ".rds")))
  }
}
