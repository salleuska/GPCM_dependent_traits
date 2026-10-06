# Toy model brief: what is identified in the CAR ability layer

Written 2026-08-28. Specification only, no code here. Intended to be handed to an
agent with R access, which should put everything it writes **inside this folder**.
Nothing in `models/`, `data/`, `1_runNimbleModel.R` or `2_summarizeSimulations.R`
may be modified. The only file read from the pipeline is
`data/simulation/sim_W.rds` (read-only).

Companion note: `notes/CAR_gamma.md`, which this brief follows in notation.

## Purpose

Strip the GPCM down to its ability layer and answer, empirically:

1. Does standardizing the abilities (the `theta[i] <- thetaTmp[i]/sqrt(SigmaDiag[i])`
   line in `models/simulation/GPCM_CAR.R`) identify the CAR precision `tau`?
2. If not, what does identify it — anchoring the item side instead?
3. Is `gamma` recoverable at all at our network sizes, and does standardizing
   help or hurt its recovery?

No items, no `dcat`, no `K` categories. A Gaussian observation layer, so the
likelihood is available in closed form and every fit takes seconds.

## Notation (repo convention, keep it)

- `C[i,j] = w_ij / d_i`, `M[i] = 1 / d_i`, from `as.carCM`; `Cmatrix` from
  `CAR_calcCmatrix`.
- `Sigma0(gamma) = (I - gamma C)^{-1} M`, the tau-free CAR covariance.
- `thetaTmp ~ dcar_proper(mu=0, ..., tau, gamma)` has `Var(thetaTmp) = Sigma0(gamma)/tau`.
- `tau` is a **precision**; `1/tau` is the ability variance. Do not call the
  observation noise `tau` — that is `sigma2_e` here.
- `gamma` is exactly the `rho` of `0_generate_simulation_data.R`.

## Correction to the earlier sketch, which changes the design

Standardizing does **not** make the ability scale cancel. `SigmaDiag` in the
model is computed from `Cmatrix`/`Mmatrix` only, i.e. it is `diag(Sigma0)`, with
no `tau` in it. So

    Var(theta[i]) = 1/tau   for every i.

Standardization removes the *heterogeneity* of the marginal variances, not the
*global* scale. The global scale is still free, and in the GPCM it is confounded
with the discriminations through `alpha[j] * theta[i]`. That — not a vanishing
`sigma^2` — is the real Case 2. The toy must therefore keep a multiplicative
discrimination, or it will not reproduce the phenomenon we care about.

## Data-generating model

Network: use the repo's simulated network, `data/simulation/sim_W.rds`
(N = 120, stochastic block model, 3 blocks). Read it, do not regenerate it, and
hold it fixed across every replicate and variant — identifiability is a property
at fixed `W`. Optional second network for a robustness check: the students
network built the way `1_runNimbleModel.R` builds it (connected students only,
n = 64). Check for isolated nodes before building `M`.

Ability layer, identical to the paper:

    thetaTmp ~ N(0, Sigma0(gamma) / tau)

Observation layer, one Gaussian "item" replacing the whole GPCM:

    y[i] ~ N( alpha * (theta[i] - beta), sigma2_e )

`alpha` plays the discrimination role, `beta` the difficulty role, `sigma2_e` has
no GPCM counterpart (the `dcat` link fixes it) and is mostly there so we can ask
what happens when it is free.

Truths for simulation:

- `gamma_true` in {0, 0.9, 0.99} — run all three. `gamma = 0` is the null control
  (iid abilities), `0.99` is the region the students data actually wants,
  `0.9` is a moderate middle where `notes/CAR_gamma.md` says the induced
  neighbour correlation is still small (~0.09 on this network).
- `tau_true = 1`, `alpha_true = 1.5`, `beta_true = 0.3`, `sigma2_e_true = 1`.

Also report, for each `gamma_true`, the implied mean neighbour correlation, so
the results are readable on a scale that means something.

## The algebra the fits should confirm

Marginalizing `theta` out, `y` is Gaussian with

    E[y] = -alpha * beta * 1
    Var(y) = (alpha^2 / tau) * S(gamma) + sigma2_e * I

where `S(gamma) = Sigma0(gamma)` unstandardized, or the correlation-matrix
version of it when standardized. So the likelihood sees `alpha` and `tau` **only**
through `kappa = alpha^2 / tau`. Any variant that leaves both free has an exactly
flat likelihood ridge, not a weak one. This is the prediction to falsify.

## The four fits

Same data, same MCMC settings, same priors wherever a parameter is free.

| variant | standardize theta | tau | alpha | sigma2_e | question |
|---|---|---|---|---|---|
| V1 (current repo model) | yes | fixed 1 | free | fixed 1 | does the current choice recover gamma, alpha, beta? |
| V1b | yes | **free** | free | fixed 1 | does standardizing identify tau? (predicted: no) |
| V2 | no | free | free | fixed 1 | does the heterogeneity of `diag(Sigma0)` break the tie? |
| V3 | no | free | **fixed 1.5** | fixed 1 | does anchoring the item side identify tau? (predicted: yes) |

Optional V4, only if V1–V3 behave as predicted: V3 with `sigma2_e` free as well,
to see whether all three of `tau`, `gamma`, `sigma2_e` separate once `alpha` is
pinned.

Priors, held identical across variants:

- `gamma ~ dunif(lower, upper)` with the bounds from nimble's proper-CAR bounds
  for this `C` (`carMinBound`/`carMaxBound`), not a hard-coded (-1, 1). Report the
  numeric bounds for the network used.
- `tau ~ dgamma(0.01, 0.01)` — proper, and deliberately diffuse, so that
  "posterior = prior" is visually unmistakable. Record the prior density so it
  can be overlaid.
- `log(alpha) ~ dnorm(0.5, 0.5)`, matching `models/simulation/GPCM_CAR.R`.
- `beta ~ dnorm(0, var = 2)`, likewise.

Implementation note: build `adj`, `num`, `C`, `M` with `as.carAdjacency` /
`as.carCM` and `Cmatrix` with `CAR_calcCmatrix`, exactly as `1_runNimbleModel.R`
does, so the toy exercises the same code path as the real model. Reuse the
standardization block verbatim from `models/simulation/GPCM_CAR.R`.

## What to compute, for every variant

1. **Prior-vs-posterior overlay for `tau`.** The cleanest identifiability
   diagnostic. If the posterior reproduces the prior, `tau` is not identified.
   Report the prior/posterior overlap coefficient as a number, not just a plot.
2. **Recovery table**: posterior mean, 95% CrI, CrI width, and coverage of the
   truth, for `gamma`, `tau`, `alpha`, `beta`.
3. **Joint posteriors**: scatter of `(log alpha, log tau)` — the predicted ridge
   has slope 1/2 on that scale, since only `alpha^2/tau` is identified, so plot
   the line and check the cloud lies along it. Also `(gamma, tau)` and
   `(gamma, alpha)`.
4. **The identified combination**: posterior for `kappa = alpha^2 / tau`. In every
   variant this should be well concentrated around its truth even where `alpha`
   and `tau` separately are not. That contrast is the headline result.
5. **Mixing**: trace plots and ESS per parameter, plus acceptance rate for the
   `gamma` sampler. `notes/CAR_gamma.md` reports ~3% acceptance with the default
   RW; note whether the toy reproduces that.
6. **Gamma on the interpretable scale**: transform each posterior draw of `gamma`
   into the implied mean neighbour correlation and summarize that too.

## Structure of the runs

Do it in two passes.

- **Pass 1, one dataset.** `gamma_true = 0.99`, all four variants, all six
  diagnostics. This is enough to see the ridge or not see it. Do not scale up
  before looking at pass 1 — if V1b shows a clean `tau` posterior, the algebra
  above is wrong somewhere and the design needs revisiting before spending
  compute.
- **Pass 2, R = 50 replicate datasets** per (variant x gamma_true) cell, same `W`
  and same truths, new `thetaTmp` and `y` each replicate. Report coverage of the
  95% CrIs across replicates, and the sampling distribution of the posterior
  means. Coverage far from 0.95 for `gamma` at `gamma_true = 0.9` would be worth
  knowing on its own.

MCMC: 4 chains, enough iterations for ESS > 400 on the worst parameter in the
best-behaved variant, identical settings everywhere. Set and record seeds. Save
raw draws so plots can be redone without refitting.

## Predictions, stated so they can fail

- V1: `gamma`, `alpha`, `beta` recovered; nothing to say about `tau`, it is fixed.
- V1b: `tau` posterior = prior, or close to it; `(log alpha, log tau)` scatter on
  the slope-1/2 ridge; `kappa` tight. **Standardization alone does not identify
  the ability scale.**
- V2: same ridge as V1b. If instead `tau` separates, the heterogeneity in
  `diag(Sigma0)` is carrying identifying information, which is a genuinely
  interesting finding and changes the argument in the paper.
- V3: `tau` identified and centred on 1. Anchoring the item side is sufficient.
- Across `gamma_true`: `gamma` recovered at 0.99, poorly at 0.9, and at
  `gamma_true = 0` the posterior should be diffuse rather than piled at the upper
  bound. If it piles up at the bound even under the null, the boundary behaviour
  in the students data is a property of the model and prior, not of the data —
  which is exactly item 1 of the "things to settle" list in `notes/CAR_gamma.md`.

## Deliverables

Inside `notes/toy_identifiability/`:

- one R script per stage (simulate, fit, summarize) or one script with clear
  sections, sourcing nothing from the pipeline except `sim_W.rds`;
- saved draws under `notes/toy_identifiability/output/`;
- figures under `notes/toy_identifiability/figures/`;
- `RESULTS.md`: the recovery tables, the overlap numbers, the figures, and a
  short paragraph per prediction saying whether it held.
