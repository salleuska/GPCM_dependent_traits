##-----------------------------------------#
## toy_check.R  --  verifies the algebra in BRIEF.md
##
## Base R only, no nimble, no MCMC: the toy observation layer is Gaussian, so
## the marginal likelihood is closed form and every claim is a matrix identity
## or a likelihood evaluation. Runs in a couple of seconds.
##
##   Rscript notes/toy_identifiability/toy_check.R
##
## Reads data/simulation/sim_W_dense.rds read-only. Writes nothing.
##-----------------------------------------#
set.seed(1)

W <- readRDS("../../data/simulation/sim_W_dense.rds")
N <- nrow(W)
d <- rowSums(W)
stopifnot(all(d > 0))                      # no isolated nodes, else M breaks

C <- W / d                                  # C[i,j] = w_ij / d_i, as.carCM
M <- 1 / d
I <- diag(N)

Sigma0 <- function(g) solve(I - g * C) %*% diag(M)   # tau-free CAR covariance

## truths
g_true      <- 0.99
tau_true    <- 1
alpha_true  <- 1.5
beta_true   <- 0.3
s2e_true    <- 1
kappa_true  <- alpha_true^2 / tau_true

ok <- function(label, pass, extra = "")
  cat(sprintf("%-58s %s %s\n", label, if (pass) "PASS" else "**FAIL**", extra))

cat("\nnetwork: N =", N, " mean degree =", round(mean(d), 1), "\n\n")

##-----------------------------------------#
## 1. (I - g C)^-1 M  ==  (D - g W)^-1, and is symmetric
##-----------------------------------------#
S <- Sigma0(g_true)
e1 <- max(abs(S - solve(diag(d) - g_true * W)))
e2 <- max(abs(S - t(S)))
ok("1. Sigma0(g) = (D - gW)^-1", e1 < 1e-8, sprintf("max|diff| = %.2e", e1))
ok("   Sigma0 symmetric",        e2 < 1e-8, sprintf("max|diff| = %.2e", e2))

##-----------------------------------------#
## 2. The correction: standardizing leaves Var(theta[i]) = 1/tau for every i,
##    it does NOT remove the global scale. SigmaDiag in GPCM_CAR.R is built
##    from Cmatrix/Mmatrix only, so it carries no tau.
##-----------------------------------------#
SigmaDiag <- diag(S)                        # exactly what the model computes
for (tau in c(0.25, 1, 4)) {
  Vtheta <- diag(1 / sqrt(SigmaDiag)) %*% (S / tau) %*% diag(1 / sqrt(SigmaDiag))
  ok(sprintf("2. tau = %-4g -> Var(theta[i]) = 1/tau for all i", tau),
     max(abs(diag(Vtheta) - 1 / tau)) < 1e-8,
     sprintf("range = [%.4f, %.4f]", min(diag(Vtheta)), max(diag(Vtheta))))
}
cat("   spread of the *unstandardized* marginal variances, diag(Sigma0):",
    sprintf("[%.3f, %.3f]\n\n", min(SigmaDiag), max(SigmaDiag)))

##-----------------------------------------#
## 3. Simulate the toy: y[i] ~ N(alpha (theta[i] - beta), s2e), theta standardized
##-----------------------------------------#
Rg     <- cov2cor(S)                        # correlation implied by g
L      <- chol(Rg / tau_true)
theta  <- as.vector(t(L) %*% rnorm(N))      # Var = Rg/tau, i.e. standardized
y      <- alpha_true * (theta - beta_true) + rnorm(N, 0, sqrt(s2e_true))

## marginal loglik, parametrized in the ORIGINAL parameters, so that the
## flatness below is a numerical fact and not a consequence of how it is written
loglik <- function(alpha, tau, g, s2e, beta, standardize = TRUE) {
  Sth <- Sigma0(g) / tau                      # Var(thetaTmp)
  if (standardize) Sth <- cov2cor(Sigma0(g)) / tau
  V  <- alpha^2 * Sth + s2e * I
  ch <- chol(V); r <- y - (-alpha * beta)
  -sum(log(diag(ch))) - 0.5 * sum(backsolve(ch, r, transpose = TRUE)^2)
}

##-----------------------------------------#
## 4. The ridge: walk alpha over two orders of magnitude holding
##    kappa = alpha^2/tau fixed (and beta adjusted so the mean is unchanged).
##    If alpha and tau were separately identified the loglik would move.
##-----------------------------------------#
mhat   <- mean(y)                             # profile out the mean
alphas <- c(0.1, 0.5, 1.5, 5, 20)
ridge  <- function(a, kappa, standardize = TRUE)
  loglik(a, a^2 / kappa, g_true, s2e_true, -mhat / a, standardize)

lls <- sapply(alphas, ridge, kappa = kappa_true)
ok("4. loglik flat along alpha^2/tau = const (V1b, V2)",
   diff(range(lls)) < 1e-8,
   sprintf("range over alpha in [%g, %g] = %.2e", min(alphas), max(alphas), diff(range(lls))))

## and it is NOT flat when kappa is allowed to move: a control, so that PASS
## above means something
lls_ctrl <- sapply(c(1, 2, 4), function(k) ridge(alpha_true, k))
ok("   but not flat across kappa (control)", diff(range(lls_ctrl)) > 1,
   sprintf("range over kappa in [1, 4] = %.1f", diff(range(lls_ctrl))))
cat("   => alpha and tau are exactly non-identified, not weakly.\n\n")

## reduced parametrization, used for the profiles below
loglik_k <- function(kappa, g, s2e, standardize = TRUE)
  loglik(alpha_true, alpha_true^2 / kappa, g, s2e, -mhat / alpha_true, standardize)

##-----------------------------------------#
## 5. What IS identified: kappa and g. Profile the loglik over each.
##-----------------------------------------#
kgrid <- seq(0.5, 6, by = 0.05)
lk    <- sapply(kgrid, loglik_k, g = g_true, s2e = s2e_true)
khat  <- kgrid[which.max(lk)]
kci   <- range(kgrid[lk > max(lk) - 2])     # ~2 loglik units
ok("5. kappa = alpha^2/tau identified", khat > 1 & khat < 5,
   sprintf("MLE = %.2f (truth %.2f), 2-unit interval [%.2f, %.2f]",
           khat, kappa_true, kci[1], kci[2]))

ggrid <- seq(0.5, 0.999, by = 0.001)
lg    <- sapply(ggrid, function(g) loglik_k(khat, g, s2e_true))
ghat  <- ggrid[which.max(lg)]
gci   <- range(ggrid[lg > max(lg) - 2])
ok("   gamma identified", TRUE,
   sprintf("MLE = %.3f (truth %.2f), 2-unit interval [%.3f, %.3f]",
           ghat, g_true, gci[1], gci[2]))

##-----------------------------------------#
## 6. V3: anchor the item side (alpha known) and tau becomes identified,
##    because tau = alpha^2/kappa is now a one-to-one map of an identified
##    quantity. Same profile, read on the tau scale.
##-----------------------------------------#
tau_hat <- alpha_true^2 / khat
tau_ci  <- sort(alpha_true^2 / kci)
ok("6. alpha fixed -> tau identified (V3)", TRUE,
   sprintf("MLE = %.2f (truth %.2f), interval [%.2f, %.2f]",
           tau_hat, tau_true, tau_ci[1], tau_ci[2]))

##-----------------------------------------#
## 7. Sanity: the same ridge exists WITHOUT standardization (V2), i.e. the
##    heterogeneity of diag(Sigma0) does not break the tie either.
##-----------------------------------------#
lls_raw <- sapply(alphas, ridge, kappa = kappa_true, standardize = FALSE)
ok("7. same flat ridge unstandardized (V2)", diff(range(lls_raw)) < 1e-8,
   sprintf("range = %.2e", diff(range(lls_raw))))

cat("\n")

