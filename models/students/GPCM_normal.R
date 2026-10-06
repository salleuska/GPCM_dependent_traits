##-----------------------------------------#
## GPCM - Generalized Partial Credit Model
## normal latent trait (long format)
##-----------------------------------------#
## Baseline model for the application. Several exams, one row of Y per
## response: student[n], exam[n] and item[n] say which response it is, and
## the number of categories K[e, j] varies across exams and items.
## Each student sat exactly one exam. Abilities are independent N(0, 1),
## the network is ignored.
##
## one exam version: models/simulation/GPCM_normal.R
##
## sourced by 1_runNimbleModel.R, which provides
##   Y, K, allData, and dGPCM (models/dGPCM.R)
##-----------------------------------------#

modelCode <- nimbleCode({

  for(i in 1:ntot) {
    Y[i] ~ dGPCM(theta[student[i]], alpha[exam[i], item[i]],
                 beta[exam[i], item[i], 2:K[exam[i], item[i]]])
  }

  ## student ability
  for(i in 1:N){
    theta[i] ~ dnorm(0, 1)
  }

  for(e in 1:nExam) {
    for(j in 1:nItem){
      log(alpha[e, j]) ~ dnorm(0.5, 0.5)
      
      beta[e, j, 1] <- 0

      for (k in 2:K[e, j]){
        beta[e, j,  k] ~ dnorm(0, var = 2) 
      }
    }
  }

})

## ---- Data ----
data <- list(Y = Y)

## ---- Constants ----
constants <- list(ntot  = length(data$Y),
                  nExam = length(unique(allData$marks$exam)),
                  nItem = length(unique(allData$marks$item)),
                  N     = length(unique(allData$marks$student)),
                  exam    = allData$marks$exam,
                  student = allData$marks$student,
                  item    = allData$marks$item,
                  K       = K)

## ---- Initial values ----
## theta is initialized with the raw scores by 1_runNimbleModel.R
inits <- list(
  beta      = array(0, dim = c(constants$nExam, constants$nItem, max(constants$K))),
  log_alpha = array(log(1.5), dim = c(constants$nExam, constants$nItem))
)

## ---- Monitors ----

monitors <- c("beta", "alpha", "theta", "log_alpha")





