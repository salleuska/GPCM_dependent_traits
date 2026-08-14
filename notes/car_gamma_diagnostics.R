## Diagnostics for notes/CAR_gamma.md
## Behaviour of the proper CAR as a function of gamma: valid range, induced
## correlation between neighbours, and conditioning of (I - gamma C)
## run from the project root
suppressMessages(library(nimble))

describe <- function(W, label) {
  wn <- as.carAdjacency(W)
  CM <- as.carCM(wn$adj, wn$weights, wn$num)
  Cm <- CAR_calcCmatrix(C = CM$C, adj = wn$adj, num = wn$num)
  Mm <- diag(CM$M)
  n  <- nrow(W)
  I  <- diag(1, n)

  ev <- eigen(Cm, only.values = TRUE)$values
  cat("\n=====", label, "  n =", n, "=====\n")
  cat("eigenvalues of C in [", round(min(ev), 3), ",", round(max(ev), 3), "]\n")
  cat("valid gamma interval: (", round(1/min(ev), 3), ",", round(1/max(ev), 3), ")\n")

  ## check that (I - gamma C)^-1 M equals (D - gamma W)^-1
  D <- diag(rowSums(W)); g <- 0.5
  A <- solve(I - g * Cm) %*% Mm
  B <- solve(D - g * W)
  cat("(I-gC)^-1 M == (D-gW)^-1 :", isTRUE(all.equal(unname(A), unname(B))), "\n")

  ## marginal correlation induced between neighbours, as gamma varies
  adjPairs <- which(W == 1 & upper.tri(W), arr.ind = TRUE)
  cat("\n gamma | mean corr(neighbours) | max corr | cond(I - gamma C)\n")
  for(g in c(0.1, 0.5, 0.8, 0.9, 0.95, 0.99, 0.999, 0.9999)) {
    S  <- solve(I - g * Cm) %*% Mm
    sd <- sqrt(diag(S))
    R  <- S / outer(sd, sd)
    cat(sprintf("%7.4f | %20.4f | %8.4f | %.3e\n",
                g, mean(R[adjPairs]), max(R[adjPairs]), kappa(I - g * Cm, exact = TRUE)))
  }
}

load("data/students/students.RData")
describe(allData$networkMatrix, "students network (connected)")

sim <- readRDS("data/simulation/sim_scenario1.rds")
describe(sim$W, "simulated SBM network")
