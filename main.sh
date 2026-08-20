#!/bin/bash

#############################################
## Runs the three models on each simulated scenario.
##
## The three models of a scenario run in parallel, the scenarios one after
## the other. Each job is a separate Rscript process, so nothing is shared:
## nimble compiles into the temporary directory of its own session and each
## model writes to its own output file.
##
## MCMC settings match the original scripts, so that results can be
## compared with the ones in the IRT_networks repository:
##   niter = 10000, nburnin = 4000
##
## logs/*.joblog reports the exit code of each job, a non zero Exitval
## means that model failed.
#############################################
`# 
mkdir -p figures output logs

#############################################
## scenario 1
#############################################

echo "Scenario 1"
parallel --jobs 3 --joblog logs/sim_scenario1.joblog --tagstring "{/.}" \
    Rscript 1_runNimbleModel.R \
    --model={} \
    --data=data/simulation/sim_scenario1.rds \
    --niter=10000 \
    --nburnin=4000 \
    --nthin=1 \
    ::: models/simulation/GPCM_normal.R \
        models/simulation/GPCM_CAR.R \
        models/simulation/GPCM_SSCAR.R

#############################################
## scenario 2
#############################################

echo "Scenario 2"
parallel --jobs 3 --joblog logs/sim_scenario2.joblog --tagstring "{/.}" \
    Rscript 1_runNimbleModel.R \
    --model={} \
    --data=data/simulation/sim_scenario2.rds \
    --niter=10000 \
    --nburnin=4000 \
    --nthin=1 \
    ::: models/simulation/GPCM_normal.R \
        models/simulation/GPCM_CAR.R \
        models/simulation/GPCM_SSCAR.R
`
#############################################
## scenario 3
#############################################

echo "Scenario 3"

parallel --jobs 3 --joblog logs/sim_scenario3.joblog --tagstring "{/.}" \
    Rscript 1_runNimbleModel.R \
    --model={} \
    --data=data/simulation/sim_scenario3.rds \
    --niter=10000 \
    --nburnin=4000 \
    --nthin=1 \
    ::: models/simulation/GPCM_normal.R \
        models/simulation/GPCM_CAR.R \
        models/simulation/GPCM_SSCAR.R
