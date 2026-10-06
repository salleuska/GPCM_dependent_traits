#!/bin/bash

#############################################
## Runs the three models on each simulated scenario.
##
## The three models of a scenario run in parallel, the scenarios one after
## the other. Each job is a separate Rscript process, so nothing is shared:
## nimble compiles into the temporary directory of its own session and each
## model writes to its own output file.
##
## Data files are data/simulation/sim_<network>_scenario<s>.rds, results go
## to output/sim_<network>_scenario<s>/. Choose what to run below.
##
## MCMC settings match the original scripts, so that results can be
## compared with the ones in the IRT_networks repository:
##   niter = 10000, nburnin = 4000
##
## logs/*.joblog reports the exit code of each job, a non zero Exitval
## means that model failed.
#############################################
NETWORKS="dense sparse"
SCENARIOS="1 2 3"

mkdir -p figures output logs

for net in $NETWORKS; do
  for s in $SCENARIOS; do
    name="sim_${net}_scenario${s}"
    echo "Network ${net}, scenario ${s}"
    parallel --jobs 3 --joblog logs/${name}.joblog --tagstring "{/.}" \
        Rscript 1_runNimbleModel.R \
        --model={} \
        --data=data/simulation/${name}.rds \
        --niter=10000 \
        --nburnin=4000 \
        --nthin=1 \
        ::: models/simulation/GPCM_normal.R \
            models/simulation/GPCM_CAR.R \
            models/simulation/GPCM_SSCAR.R
  done
done
