# The CAR correlation parameter piles up at 1

Note written 2026-08-12, after getting the CAR models to run again.
Open question to discuss, no code change made.

## What gamma is in our models

`GPCM_CAR.R` and `GPCM_SSCAR.R` give the abilities a proper CAR prior through
nimble's `dcar_proper`:

```r
thetaTmp[1:n] ~ dcar_proper(mu, adj, num, C, M, tau, gamma)
```

with `C` and `M` from `as.carCM`, i.e. `C[i,j] = w_ij / d_i` and `M[i] = 1 / d_i`,
where `d_i` is the degree of student i. Under that parametrization

```
Var(thetaTmp) = tau^-1 (I - gamma C)^-1 M = tau^-1 (D - gamma W)^-1
```

(verified numerically for both networks, see `car_gamma_diagnostics.R`).

Two consequences worth stating explicitly:

* **gamma is exactly the `rho` used to generate the simulated data.**
  `0_generate_simulation_data.R` builds scenario 2 as
  `solve((D_w - rho * W) / sigma2)` with `rho = 0.98`, so scenario 2 has a known
  true `gamma = 0.98` and `tau = 1/sigma2 = 0.05`. The simulation is therefore a
  direct test of whether we can recover gamma.
* **`tau` is fixed at 1 in the model, and this is harmless.** We standardize,
  `theta[i] <- thetaTmp[i] / sqrt(SigmaDiag[i])`, where `SigmaDiag` is the
  diagonal of `(I - gamma C)^-1 M`. That makes `theta` have marginal variance
  `1/tau`, so with `tau = 1` the abilities are on the unit scale by construction
  and `tau` is not separately identified. Fixing it is a choice, not a bug.

## What we observed

Application data (70 students, 64 connected), spike and slab model, 2000
iterations, 500 burn-in:

| start | g | gamma_H1 |
|---|---|---|
| `g = 1`, `gamma_H1 = 0.95` | 1500 / 1500 at 1 | [0.998, 1.000] |
| `g = 0`, `gamma_H1 = 0.10` | 1500 / 1500 at 1 | [0.999, 1.000] |

The chain reaches the same place from both corners, so this is **not** a stuck
sampler. The posterior genuinely wants gamma at its upper boundary of 1, and the
posterior inclusion probability of the network effect is 1.

Mixing of `gamma_H1` is poor though: about 3% acceptance with the default RW
sampler, and adaptation does not kick in before ~200 iterations.

## Why this happens

gamma is a badly behaved measure of dependence strength: the induced
correlation between neighbours is a very steep function of gamma near 1, and
close to zero everywhere else. For our two networks:

**Students network (n = 64), valid gamma in (-1.768, 1)**

| gamma | mean corr between neighbours | max corr | cond(I - gamma C) |
|---|---|---|---|
| 0.50 | 0.055 | 0.24 | 2.8 |
| 0.90 | 0.221 | 0.54 | 18.6 |
| 0.95 | 0.334 | 0.60 | 38.8 |
| 0.99 | 0.668 | 0.80 | 2.0e2 |
| 0.999 | 0.944 | 0.98 | 2.0e3 |

**Simulated SBM network (n = 120), valid gamma in (-5.82, 1)**

| gamma | mean corr between neighbours | max corr | cond(I - gamma C) |
|---|---|---|---|
| 0.50 | 0.018 | 0.02 | 2.2 |
| 0.90 | 0.094 | 0.10 | 11.6 |
| 0.95 | 0.161 | 0.18 | 23.3 |
| 0.99 | 0.465 | 0.50 | 1.2e2 |
| 0.999 | 0.894 | 0.91 | 1.2e3 |

So gamma has to exceed ~0.99 before the prior expresses any appreciable
dependence at all. A posterior sitting at 0.999 is the model saying "the data
want real dependence", expressed in the only region of the parameter space where
the CAR can deliver it. This is a known property of the proper CAR rather than
something specific to our data; worth checking Wall (2004, JSPI, on the spatial
structure implied by CAR and SAR models) and Besag and Kooperberg (1995) before
writing it up.

Note the conditioning column: `I - gamma C` stays well conditioned (~2e3 at
gamma = 0.999). The problem is **identifiability and interpretation, not
numerical breakdown**. The matrix only becomes singular at gamma = 1 exactly.

## Why it matters for the paper

1. **`Unif(-1, 1)` on gamma is not a neutral prior.** Mapped to neighbour
   correlation, almost all of its mass sits at essentially zero dependence:
   97.5% of a `Unif(-1, 1)` lies below gamma = 0.95, which on the students
   network means a mean neighbour correlation below 0.33. So the prior is
   informative
   *against* dependence, and the spike and slab inclusion probability inherits
   that. A referee can reasonably ask how the inclusion probability changes
   under a prior that is uniform on the correlation scale instead.
2. **The spike and slab is comparing independence against a near-intrinsic
   CAR**, since that is where the slab's posterior mass ends up, rather than
   against a moderate dependence alternative.
3. **A boundary posterior invites the question of whether the proper CAR is the
   right model at all**, as opposed to the intrinsic CAR (`dcar_normal`), which
   is the gamma = 1 limit and drops the parameter entirely at the cost of an
   improper prior and a sum-to-zero constraint.

## Things to settle, roughly in order

1. **Does the simulation recover gamma?** Scenario 2 has true gamma = 0.98 and
   scenario 1 has true gamma = 0 (iid abilities). Running `GPCM_CAR.R` on both
   is the cleanest evidence of whether the boundary behaviour is a property of
   the students data or of the model and sampler. This is cheap and should come
   first. Note the simulation models have not been run since the CAR fix.
2. **Is the spike and slab calibrated?** Scenario 1 should give a low inclusion
   probability for g. If it does not, the model comparison is not usable.
3. **Sampler for `gamma_H1`.** 3% acceptance is too low. Options: a slice
   sampler, a RW on a transformed scale, or blocking gamma with `thetaTmp`.
4. **Prior sensitivity.** Rerun with at least one alternative prior on gamma
   and report the inclusion probability under both.
5. **Decide how to report gamma.** Either on its own scale with the caveat
   above, or translated into an implied neighbour correlation, which is far more
   interpretable and is a one-line transformation of the posterior samples.

## Reproducing the numbers

`notes/car_gamma_diagnostics.R` produces both tables and checks the
`(I - gamma C)^-1 M = (D - gamma W)^-1` identity.
