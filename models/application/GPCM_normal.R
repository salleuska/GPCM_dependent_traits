##-----------------------------------------#
## GPCM - normal latent trait
##-----------------------------------------#


modelCode <- nimbleCode({

  for(i in 1:ntot) {
    Y[i] ~ dcat(prob[student[i], exam[i], item[i], 1:K[exam[i], item[i]] ])
  }

  ## student ability
  for(i in 1:N){
    theta[i] ~ dnorm(0, 1)
  }

  for(i in 1:N) {
    for(e in 1:nExam) {
      for(j in 1:nItem) {
        for(k in 1:K[e, j]) {
          eta[i, e, j, k] <- alpha[e, j] * (theta[i] - beta[e, j, k])
          psum[i, e, j, k] <- sum(eta[i, e, j, 1:k])
          exp.psum[i, e, j, k] <- exp(psum[i, e, j, k])
        }
        
        ## Normalizing probabilities
        prob[i, e, j, 1:K[e, j]] <- exp.psum[i, e, j, 1:K[e, j]] / sum(exp.psum[i, e, j, 1:K[e, j]])
      }

    }
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

data <- list(Y = allData$marks$y)

constants <- list(ntot= length(data$Y),
                  nExam = length(unique(allData$marks$exam)),  
                  nItem = length(unique(allData$marks$item)), 
                  N = length(unique(allData$marks$student)),
                  exam = allData$marks$exam,  
                  student = allData$marks$student, 
                  item = allData$marks$item,
                  K = allData$K[, -1] + 1)

inits <- list()

inits$beta <- array(0, dim = c(constants$nExam, constants$nItem, max(constants$K)))
inits$log_alpha <- array(log(1.5), dim = c(constants$nExam, constants$nItem))
inits$theta <- rnorm(constants$N, 0, 1)

inits$alpha <- exp(inits$log_alpha)





