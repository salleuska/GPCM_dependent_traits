# Reorganized Project

This project was reorganized to follow a cleaner, example-driven structure.

## Layout

- `scripts/`: entry-point scripts you run by hand.
  - `simulations/`: scripts for synthetic experiments.
- `models/`: Nimble model definitions.
- `R/`: shared helpers (you can add more over time).
- `data/`
  - `raw/`: original CSVs or external data dumps.
  - `processed/`: RDS/RData that are ready-to-use (e.g., `voti.rds`, `networkData.rds`).
  - `simulations/`: simulated datasets.
- `results/`
  - `posterior/`: RDS outputs from MCMC.
  - `plots/`: figures.
  - `simulations/`: results from simulation runs.

## Usage

Run scripts **from the project root** (the folder containing this README). Paths inside scripts are now relative to this root, pointing at `data/` and `results/`.

Example:
```r
# from project root
source('scripts/10_fit_irt.R')
```

## Notes

- Any old references to `dati/`, `posteriorSamples/`, `simulationData/`, or `simulationResults/` were updated to the new folders.
- If you use RStudio, consider creating an `.Rproj` file in the root so `getwd()` defaults here.
- You can further factor common code into `R/utils.R` later.

## Quick start (example-style)

Run a model:
```bash
Rscript 1_runNimbleModel.R --model irt_car --iter 5000 --burn 1000
```

Then post-process:
```bash
Rscript 2_postprocess.R
```

This mirrors the example convention of `1_runNimbleModel.R` and `2_processSamples.R` (here named `2_postprocess.R`).
