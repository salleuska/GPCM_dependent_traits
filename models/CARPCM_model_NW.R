##-----------------------------------------#
## PCM – Partial Credit Model with CAR prior
##-----------------------------------------#

library(nimble)

## ---- Model code ----
modelCode <- nimbleCode({

  ## Observed responses
  for(i in 1:N) {
    for(j in 1:nItems) {
      Y[i, j] ~ dcat(prob[i, j, 1:K])
    }
  }

  ## Individual latent variables with CAR structure
  thetaTmp[1:N] ~ dcar_proper(mu   = mu[1:N],
                              adj  = adj[1:L],
                              num  = num[1:N],
                              C    = C[1:L],
                              M    = M[1:N],
                              tau  = tau,
                              gamma= gamma)

  ##  ## standardize ability parameters
  # tmp[1:N, 1:N] <- inverse(Identity[1:N, 1:N] - gamma * Cmatrix[1:N, 1:N])
  # Sigma[1:N, 1:N] <- tmp[1:N, 1:N] %*% Mmatrix[1:N, 1:N]
  # SigmaDiag[1:N] <- diag(Sigma[1:N, 1:N])

  for(i in 1:N) {
    theta[i] <- thetaTmp[i] / sqrt(SigmaDiag[i])
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

  ## Hyperpriors
  # tau ~ dgamma(0.001, 0.001)   # optional
  gamma ~ dunif(-1, 1)

})

## ---- Data ----
## Assume `Data` has been created in the environment
data <- list(Y = Y)

## ---- Constants ----
## Assume W_nimble (from as.carAdjacency) and CM (from as.carCM) are available
constants <- list(
  nItems = NCOL(data$Y),
  N      = NROW(data$Y),
  K      = 4,
  mu     = rep(0, NROW(data$Y)),
  adj    = W_nimble$adj,
  num    = W_nimble$num,
  L      = length(W_nimble$adj),
  tau    = 1,
  C      = CM$C,
  M      = CM$M,
  Cmatrix = Cmatrix,   # computed outside with CAR_calcCmatrix
  Mmatrix = diag(CM$M),
  Identity = diag(1, NROW(data$Y))
)

## ---- Initial values ----
inits <- list(
  gamma    = 0.95,
  thetaTmp = rnorm(constants$N, 0, 1),
  beta     = matrix(rnorm(constants$nItems * constants$K, 0, 1),
                    nrow = constants$nItems, ncol = constants$K),
  log_alpha= rep(log(1.5), constants$nItems)
)




