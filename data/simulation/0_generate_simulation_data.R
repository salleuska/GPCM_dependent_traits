# -----------------------------------------#
# Simulated data, for network in {dense, sparse}:
#   sim_W_<network>.rds, sim_<network>_scenario{1,2,3}.rds
# Abilities have mean 0 and variance 1 by construction (no scale(), see
# notes/toy_identifiability/PROBLEM.md). Scenarios 1 and 3 do not depend on
# the network.
# -----------------------------------------#
library(here)

N <- 120   # students
p <- 10    # items
groups <- rep(1:3, each = N/3)

# ---- Networks: stochastic block models ----
simulate_sbm <- function(pWithin, pBetween) {
  W <- matrix(0, N, N)
  for (i in 2:N) {
    for (j in 1:(i - 1)) {
      W[i, j] <- W[j, i] <- rbinom(1, 1, ifelse(groups[i] == groups[j], pWithin, pBetween))
    }
  }
  W
}

Ws <- list()
set.seed(1234)
Ws$dense <- simulate_sbm(0.9, 0.1)    # mean degree ~43
set.seed(4321)
repeat {                              # mean degree ~14, no isolated students
  Ws$sparse <- simulate_sbm(0.3, 0.02)
  if (all(rowSums(Ws$sparse) > 0)) break
}

# ---- Abilities ----
## scenario 1: iid N(0, 1)
set.seed(124)
Scenario1 <- list(ability = rnorm(N))

## scenario 2: CAR, divided by the marginal sd; same draws z for both networks
set.seed(125)
z <- rnorm(N)
Scenario2 <- lapply(Ws, function(W) {
  S <- list(rho = 0.98)
  S$Sigma_W <- solve(diag(rowSums(W)) - S$rho * W)
  S$ability <- as.vector(t(chol(S$Sigma_W)) %*% z) / sqrt(diag(S$Sigma_W))
  S
})

## scenario 3: mixture by group, marginal variance 5/6 + 1/6 = 1
set.seed(123)
Scenario3 <- list(trueAbilityMeans = c(-0.5, 0, 0.5),
                  trueAbilityVars  = rep(5/6, 3),
                  groups           = groups)
Scenario3$ability <- rnorm(N, Scenario3$trueAbilityMeans[groups],
                           sqrt(Scenario3$trueAbilityVars[groups]))

# ---- Item parameters ----
## steps ~ N(0, 1); an item is redrawn if a category has expected probability
## below 8% for abilities N(mu, 1), mu in {-0.5, 0, 0.5}
gpcm_probs <- function(theta, alpha, beta) {
  num <- exp(cumsum(alpha * (theta - beta)))
  num / sum(num)
}
min_expected_prob <- function(alpha, beta) {
  min(sapply(c(-0.5, 0, 0.5), function(mu)
    rowMeans(sapply(mu + qnorm(ppoints(200)), gpcm_probs, alpha, beta))))
}

set.seed(3001)
Betas  <- matrix(0, p, 4)
alphas <- numeric(p)
for (j in 1:p) {
  repeat {
    alphas[j]     <- runif(1, 0.5, 2)
    Betas[j, 2:4] <- rnorm(3, 0, 1)
    if (min_expected_prob(alphas[j], Betas[j, ]) >= 0.08) break
  }
}

# ---- Responses, coded 0:3 ----
gen_responses <- function(ability) {
  t(sapply(ability, function(th)
    sapply(1:p, function(j) sample(0:3, 1, prob = gpcm_probs(th, alphas[j], Betas[j, ])))))
}

set.seed(2001)
Scenario1$data <- gen_responses(Scenario1$ability)
set.seed(2003)
Scenario3$data <- gen_responses(Scenario3$ability)
for (net in names(Ws)) {
  set.seed(2002)
  Scenario2[[net]]$data <- gen_responses(Scenario2[[net]]$ability)
}

# ---- Save ----
for (net in names(Ws)) {
  saveRDS(Ws[[net]], here(paste0("data/simulation/sim_W_", net, ".rds")))
  scenarios <- list(Scenario1, Scenario2[[net]], Scenario3)
  for (s in 1:3) {
    sc <- c(scenarios[[s]], list(difficulties = Betas, discrimination = alphas,
                                 W = Ws[[net]], network = net))
    saveRDS(sc, here(paste0("data/simulation/sim_", net, "_scenario", s, ".rds")))
  }
}
