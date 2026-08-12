##-----------------------------------------#
## GPCM - Generalized Partial Credit Model
## normal latent trait (wide format)
##-----------------------------------------#
## Baseline model for the simulation study. One exam: Y[i, j] is the score of
## student i on item j, coded 1:K. Abilities are independent N(0, 1), the
## network is ignored.
##
## multi exam version: models/application/GPCM_normal.R
##
## sourced by 1_runNimbleModel.R, which provides
##   Y, K
##-----------------------------------------#
## ---- Model code ----
modelCode <- nimbleCode({

  ## Observed responses
  for(i in 1:N) {
    for(j in 1:nItems) {
      Y[i, j] ~ dcat(prob[i, j, 1:K])
    }
    ## Student ability
    theta[i] ~ dnorm(0, 1)
  }

  ## Linear predictor and cumulative logits
  for(i in 1:N) {
    for(j in 1:nItems) {
      for(k in 1:K) {
        eta[i, j, k]      <- alpha[j] * (theta[i] - beta[j, k])
        psum[i, j, k]     <- sum(eta[i, j, 1:k])
        exp.psum[i, j, k] <- exp(psum[i, j, k])
      }
    }
  }

  ## Normalizing probabilities
  for(i in 1:N) {
    for(j in 1:nItems) {
      for(k in 1:K) {
        prob[i, j, k] <- exp.psum[i, j, k] /
                         sum(exp.psum[i, j, 1:K])
      }
    }
  }

  ## Priors for item parameters
  for(j in 1:nItems) {
    log(alpha[j]) ~ dnorm(0.5, 0.5)

    beta[j, 1] <- 0
    for (k in 2:K) {
      beta[j, k] ~ dnorm(0, var = 2)
    }
  }

})

## ---- Data ----
## Assume `Data` has been created in the environment
data <- list(Y = Y)

## ---- Constants ----
constants <- list(
  nItems = NCOL(data$Y),
  N      = NROW(data$Y),
  K      = K
)

## ---- Initial values ----
inits <- list(
  beta      = array(rnorm(constants$nItems*constants$K, 0, 2), 
                     dim = c(constants$nItems, constants$K)),
  theta     = rnorm(constants$N, 0, 1)
)
inits$log_alpha <- rep(log(1.5), constants$nItems)

## ---- Monitors ----

monitors <- c("beta", "alpha", "theta", "log_alpha")



