##-----------------------------------------#
## GPCM - Generalized Partial Credit Model
## with CAR prior on the latent traits
##-----------------------------------------#
## Several exams, one row of Y per response: student[n], exam[n] and item[n]
## say which response it is, and the number of categories K[e, j] varies
## across exams and items. Each student sat exactly one exam.
##
## Students are ordered so that the connected ones come first: the abilities
## of the first nConnected students follow a proper CAR prior over the
## network and are standardized by the marginal standard deviations implied
## by the CAR, the remaining ones have no link and are independent N(0, 1).
##
## one exam version: models/simulation/GPCM_CAR.R
## the block defining the latent traits must stay the same in the two files
##
## sourced by 1_runNimbleModel.R, which provides
##   Y, K, allData, W_nimble, CM, Cmatrix, and dGPCM (models/dGPCM.R)
##-----------------------------------------#
## ---- Model code ----
modelCode <- nimbleCode({

  ## Observed responses
  for(i in 1:ntot) {
    Y[i] ~ dGPCM(theta[student[i]], alpha[exam[i], item[i]],
                 beta[exam[i], item[i], 2:K[exam[i], item[i]]])
  }

  ## Individual latent variables with CAR structure
  thetaTmp[1:nConnected] ~ dcar_proper(mu   = mu[1:nConnected],
                                       adj  = adj[1:L],
                                       num  = num[1:nConnected],
                                       C    = C[1:L],
                                       M    = M[1:nConnected],
                                       tau  = tau,
                                       gamma= gamma)

  ## standardize ability parameters
  tmp[1:nConnected, 1:nConnected] <- inverse(Identity[1:nConnected, 1:nConnected] - gamma * Cmatrix[1:nConnected, 1:nConnected])
  Sigma[1:nConnected, 1:nConnected] <- tmp[1:nConnected, 1:nConnected] %*% Mmatrix[1:nConnected, 1:nConnected]
  SigmaDiag[1:nConnected] <- diag(Sigma[1:nConnected, 1:nConnected])

  for(i in 1:nConnected) {
    theta[i] <- thetaTmp[i] / sqrt(SigmaDiag[i])
  }

  ## independent normal distribution for isolated students
  for(i in (nConnected + 1):N) {
    theta[i] ~ dnorm(0, 1)
  }

  ## Priors for item parameters
  for(e in 1:nExam) {
    for(j in 1:nItem) {
      log(alpha[e, j]) ~ dnorm(0.5, 0.5)

      beta[e, j, 1] <- 0
      for (k in 2:K[e, j]) {
        beta[e, j, k] ~ dnorm(0, var = 2)
      }
    }
  }

  ## Hyperpriors
  # tau ~ dgamma(0.001, 0.001)   # optional
  gamma ~ dunif(-1, 1)

})

## ---- Data ----
data <- list(Y = Y)

## ---- Constants ----
## Assume W_nimble (from as.carAdjacency), CM (from as.carCM) and Cmatrix
## (from CAR_calcCmatrix) are available, all built on the connected students
constants <- list(ntot  = length(data$Y),
                  nExam = length(unique(allData$marks$exam)),
                  nItem = length(unique(allData$marks$item)),
                  N     = length(unique(allData$marks$student)),
                  nConnected = allData$nConnected,
                  exam    = allData$marks$exam,
                  student = allData$marks$student,
                  item    = allData$marks$item,
                  K       = K,
                  mu      = rep(0, allData$nConnected),
                  adj     = W_nimble$adj,
                  num     = W_nimble$num,
                  L       = length(W_nimble$adj),
                  tau     = 1,
                  C       = CM$C,
                  M       = CM$M,
                  Cmatrix = Cmatrix,
                  Mmatrix = diag(CM$M),
                  Identity = diag(1, allData$nConnected))

## ---- Initial values ----
inits <- list(
  gamma     = 0.95,
  beta      = array(0, dim = c(constants$nExam, constants$nItem, max(constants$K))),
  log_alpha = array(log(1.5), dim = c(constants$nExam, constants$nItem))
)

## the abilities are split over two nodes, so they are initialized here
## rather than by 1_runNimbleModel.R
abilityNode <- NULL

Sscores <- (scores - mean(scores))/sd(scores)

## connected students, on the unstandardized scale of the CAR
inits$thetaTmp <- Sscores[1:constants$nConnected]
## isolated students, theta is stochastic only for them
inits$theta <- c(rep(NA, constants$nConnected),
                 Sscores[(constants$nConnected + 1):constants$N])

## ---- Monitors ----
## thetaTmp is monitored because the offline WAIC needs every stochastic
## parent of the data nodes; theta is deterministic for the connected students

monitors <- c("beta", "alpha", "theta", "thetaTmp", "log_alpha", "gamma")
