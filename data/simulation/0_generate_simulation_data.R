# -----------------------------------------#
# simulate_data.R
# Generates: sim_W.rds, sim_scenario1.rds, sim_scenario2.rds, sim_scenario3.rds
# Each ScenarioX contains: $ability, $difficulties, $discrimination, $data, $W
# -----------------------------------------#

library(dplyr)
library(ggplot2)
library(mvtnorm)
library(here)
# -----------------------------------------#
# Global setup
# -----------------------------------------#
N <- 120          # number of students
p <- 10           # number of items
set.seed(1234)

# -----------------------------------------#
# 1) Stochastic block adjacency W
# -----------------------------------------#
groups <- c(rep(1, N/3), rep(2, N/3), rep(3, N/3))

W <- matrix(0, N, N)
for (i in 2:N) {
  for (j in 1:(i - 1)) {
    W[i, j] <- ifelse(groups[i] == groups[j],
                      rbinom(1, 1, 0.9),
                      rbinom(1, 1, 0.1))
    W[j, i] <- W[i, j]
  }
}
saveRDS(W, here("data/simulation/sim_W.rds"))

# -----------------------------------------#
# 2) Latent abilities
# -----------------------------------------#

## Scenario 1: iid Normal(0,1)
set.seed(123 + 1)
Scenario1 <- lst()
Scenario1$ability <- rnorm(N, 0, 1)

## Scenario 2: CAR model (rho = 0.98, sigma2 = 20)
set.seed(123 + 2)
Scenario2 <- lst()
Scenario2$rho <- 0.98
Scenario2$sigma2 <- 20
D_w <- diag(colSums(W))
Scenario2$Sigma_W <- solve((D_w - Scenario2$rho * W) / Scenario2$sigma2)
Scenario2$ability <- c(rmvnorm(1, sigma = Scenario2$Sigma_W))
Scenario2$ability <- Scenario2$ability / sqrt(diag(Scenario2$Sigma_W))

## Scenario 3: Mixture of Normals by group
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
alphas <- runif(p, 0.5, 1)

Scenario1$difficulties   <- Betas
Scenario2$difficulties   <- Betas
Scenario3$difficulties   <- Betas
Scenario1$discrimination <- alphas
Scenario2$discrimination <- alphas
Scenario3$discrimination <- alphas

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

Scenario1$data <- gen_responses(N, p, Scenario1$ability, Scenario1$discrimination, Scenario1$difficulties)
Scenario1$W <- W
saveRDS(Scenario1, here("data/simulation/sim_scenario1.rds"))

Scenario2$data <- gen_responses(N, p, Scenario2$ability, Scenario2$discrimination, Scenario2$difficulties)
Scenario2$W <- W
saveRDS(Scenario2, here("data/simulation/sim_scenario2.rds"))

Scenario3$data <- gen_responses(N, p, Scenario3$ability, Scenario3$discrimination, Scenario3$difficulties)
Scenario3$W <- W
saveRDS(Scenario3, here("data/simulation/sim_scenario3.rds"))

