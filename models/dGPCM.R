##---------------------------------------------------------------##
## Generalized partial credit model (GPCM) - NIMBLE distribution
##
## For an item with K ordered categories, coded 1..K
## (i.e. score + 1), and K - 1 step difficulties beta[1:(K-1)]:
##
##   Pr(Y = k) ∝ exp( sum_{h=1}^{k-1} lambda * (theta - beta[h]) ),
##
## with an empty sum (= 0) for the lowest category k = 1.
## This matches the paper's equation with score k-1, d = K - 1
## and steps beta_{j1}, ..., beta_{jd}.
## K is taken from the length of beta: K = length(beta) + 1, so beta
## must have length >= 2 (K >= 3).
##
## The model scripts keep beta[j, 1] = 0 as a placeholder and pass the
## steps as beta[j, 2:K]. The placeholder term alpha * theta appears in
## every category, so it cancels in the normalization: this is the same
## likelihood as the previous dcat(prob[...]) formulation.
##
## sourced by 1_runNimbleModel.R before the model script
##---------------------------------------------------------------##

## ---- NIMBLE density (used inside the models) ----
dGPCM <- nimbleFunction(
  run = function(x = double(0), theta = double(0), lambda = double(0),
                 beta = double(1), log = integer(0, default = 0)) {
    returnType(double(0))
    K <- length(beta) + 1

    ## unnormalized log-probabilities, psum[1] = 0
    psum <- numeric(length = K, value = 0)
    for (k in 2:K) {
      psum[k] <- psum[k - 1] + lambda * (theta - beta[k - 1])
    }

    ## log normalizing constant (log-sum-exp for stability)
    m <- max(psum)
    logDenom <- m + log(sum(exp(psum - m)))

    ## log-probability of the observed category
    logProb <- -Inf
    for (k in 1:K) {
      if (x == k) logProb <- psum[k] - logDenom
    }

    if (log) {
      return(logProb)
    } else {
      return(exp(logProb))
    }
  })

## ---- NIMBLE random generator (required by NIMBLE) ----
rGPCM <- nimbleFunction(
  run = function(n = integer(0), theta = double(0), lambda = double(0),
                 beta = double(1)) {
    returnType(double(0))
    K <- length(beta) + 1

    psum <- numeric(length = K, value = 0)
    for (k in 2:K) {
      psum[k] <- psum[k - 1] + lambda * (theta - beta[k - 1])
    }
    p <- exp(psum - max(psum))
    p <- p / sum(p)

    return(rcat(1, p))
  })
