# Why scenario 2 does not beat iid: a statement of the problem

Written 2026-08-28. Supersedes most of `BRIEF.md` in this folder, which was
written for an identifiability question that turned out not to be the issue
(`tau` is fixed at 1 in the model, so the ability scale is pinned).

No code has been changed. This note states the problem and the quantities that
were computed; the decision about what to change is open.

## Notation

`W` fixed adjacency, `d_i = sum_j w_ij`, `D = diag(d)`, `C = D^{-1} W`, `M = D^{-1}`.

    Sigma0(g) = (I - g C)^{-1} M = (D - g W)^{-1}
    R(g)      = corr matrix of Sigma0(g),  i.e. diag(Sigma0)^{-1/2} Sigma0 diag(Sigma0)^{-1/2}

## What the model says

`models/simulation/GPCM_CAR.R`:

    thetaTmp ~ N(0, Sigma0(g)/tau),   tau = 1 fixed
    theta_i  = thetaTmp_i / sqrt(Sigma0_ii)

so the model's marginal law of the ability vector is exactly

    theta ~ N_N(0, R(g)).                                            (M)

E[theta_i] = 0 and Var(theta_i) = 1 for each i. These are statements about the
**distribution**, not about any realized vector.

## What the generator does

`data/simulation/0_generate_simulation_data.R`, scenario 2, rho = 0.98,
sigma2 = 20:

    theta*  ~ N(0, sigma2 (D - rho W)^{-1})
    theta   = (theta* - mean(theta*) 1) / sd(theta*)                 (G)

`scale()` imposes two **sample** constraints on the realization:

    1'theta = 0    and    ||theta||^2 = N.

Under (M) both events have probability zero. So the saved scenario-2 abilities
are not a draw from the model being fitted. The commented-out alternative on
line 49, `theta*_i / sqrt(Sigma0_ii * sigma2)`, *is* a draw from (M): it imposes
the marginal moments without constraining the realization.

## Why the centering specifically destroys the signal

Log-likelihood ratio of (M) against iid, for an observed theta:

    log L = -1/2 theta' (R^{-1} - I) theta - 1/2 log|R|

Write the spectral decomposition R = sum_k lambda_k v_k v_k'. For a proper CAR
with g near 1 the leading eigenvector v_1 is close to the constant vector
1/sqrt(N), and it carries a large share of the trace. Centering sets the
component of theta along 1 to exactly zero, i.e. it annihilates the coordinate
with the largest lambda_k — the single term contributing most to log L.

Equivalently, in terms of the natural discriminating statistic T = 1'theta / N:

    Var_M(T)   = 1' R 1 / N^2  ~=  mean pairwise correlation
    Var_iid(T) = 1 / N

For the simulated network these are 0.29 vs 0.0083, a factor of 35 in variance.
The dispersion of T is the primary evidence of positive dependence. `scale()`
sets T = 0 identically, removing it.

## The second problem: the network is too dense to carry the rest

After centering, the only surviving evidence is the contrast between neighbour
and non-neighbour correlations in R(g), at g = 0.98:

| network                        | N   | mean deg | corr nb | corr non-nb | gap   |
|--------------------------------|-----|----------|---------|-------------|-------|
| simulated SBM (0.9 / 0.1)      | 120 | 43.2     | 0.310   | 0.269       | 0.040 |
| SBM 0.30 / 0.02                | 120 | 13.5     | 0.355   | 0.251       | 0.105 |
| SBM 0.12 / 0.005               | 119 | 4.9      | 0.458   | 0.205       | 0.252 |
| students (real)                | 77  | 13.0     | 0.479   | 0.269       | 0.210 |

At mean degree 43 out of 119 possible, almost every pair is a neighbour and
R(g) is close to equicorrelated. An equicorrelated R is exactly the case where
all the information sits in the constant eigenvector — the one centering removes.
The simulated network is therefore the worst case, and it is far from the real
students network, which is sparse.

## Numbers observed

log L = loglik(CAR) - loglik(iid), evaluated on the true abilities, simulated
network, g = 0.98, 300 draws:

| transformation of the abilities  | mean log L | CAR preferred |
|----------------------------------|------------|---------------|
| `scale(x)` (current generator)   | -3.92      | 11%           |
| `x / sqrt(diag(Sigma))`          | +22.86     | 95%           |

The scenario-2 abilities as saved on disk give log L = -3.03, i.e. they prefer
iid. This is upstream of the GPCM: no item model can recover a difference that
is absent from the abilities.

Same test after centering, varying the network: CAR preferred in 6% (current
SBM), 43% (SBM 0.30/0.02), 81% (SBM 0.12/0.005), 39% (students).

## Two consequences, separable

1. **Generator/model mismatch.** (G) is not a draw from (M). Replacing `scale()`
   with the division by `sqrt(diag(Sigma_W))` makes it one, and restores
   log L from -3.92 to +22.86 on the existing network.

2. **The restored signal is mostly a common level, not network structure.**
   With `beta[j,1] <- 0` pinned in the GPCM the overall location of theta is
   identified, so the GPCM can use it — but an exchangeable random intercept
   would explain it equally well. Evidence specific to *this network* is the
   0.040 gap, which is negligible. Sparsifying the SBM (row 3 of the density
   table) is what makes scenario 2 a test of network dependence rather than of
   dependence in general.

Fixing 1 without 2 gives a simulation that beats iid for a reason that does not
require the network.

## Open decisions, not yet taken

- Whether to change the generator at all, given that regenerating
  `sim_scenario1/2/3.rds` invalidates everything under `output/`.
- Whether scenarios 1 and 3 keep `scale()`. For scenario 1 centering is close to
  a no-op (sample mean ~ 0.09); for scenario 3 it removes the global mean but
  preserves the between-group contrasts. Neither is damaged the way scenario 2
  is, which is itself the asymmetry: the same line of code is a no-op for the
  null and a large intervention for the alternative.
- Whether the CAR at g = 0.98 on a dense graph is the intended alternative
  hypothesis at all — see `notes/CAR_gamma.md` on g needing to approach 1 before
  the CAR expresses appreciable dependence.

## Reproducing

`notes/toy_identifiability/toy_check.R` covers the identifiability algebra only.
The numbers in this note came from short read-only snippets over `sim_W.rds`,
`sim_scenario2.rds` and `data/students/networkData.rds`; they are not yet in a
script.
