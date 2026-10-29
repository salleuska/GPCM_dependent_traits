##-----------------------------------------#
## GPCM - Generalized Partial Credit Model
## normal latent trait (wide format)
##-----------------------------------------#
## Baseline model for the simulation study. One exam: Y[i, j] is the score of
## student i on item j, coded 1:K. Abilities are independent N(0, 1), the
## network is ignored.
##
## multi exam version: models/students/GPCM_normal.R
##
## sourced by 1_runNimbleModel.R, which provides
##   Y, K, and dGPCM (models/dGPCM.R)
##-----------------------------------------#
## ---- Model code ----
modelCode <- nimbleCode({

  ## Observed responses
  for(i in 1:N) {
    for(j in 1:nItems) {
      Y[i, j] ~ dGPCM(theta[i], alpha[j], beta[j, 2:K])
    }
    ## Student ability
    theta[i] ~ dnorm(0, 1)
  }

  ## Priors for item parameters
  for(j in 1:nItems) {
    log(alpha[j]) ~ dnorm(0.5, var = 0.5)

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



