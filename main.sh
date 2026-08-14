#!/bin/bash

#############################################
## Runs every model on every dataset.
##
## MCMC settings match the original scripts, so that results can be
## compared with the ones in the IRT_networks repository:
##   simulations  niter = 10000, nburnin = 4000
##   application  niter = 50000, nburnin = 10000
#############################################

mkdir -p figures

mkdir -p output

#############################################
## 1) RUN SIMULATIONS
############################################

#############################################
## scenario 1
#############################################

Rscript 1_runNimbleModel.R  \
--model=models/simulation/GPCM_normal.R \
--data=data/simulation/sim_scenario1.rds \
--niter=10000 \
--nburnin=4000 \
--nthin=1

Rscript 1_runNimbleModel.R  \
--model=models/simulation/GPCM_CAR.R \
--data=data/simulation/sim_scenario1.rds \
--niter=10000 \
--nburnin=4000 \
--nthin=1

Rscript 1_runNimbleModel.R  \
--model=models/simulation/GPCM_SSCAR.R \
--data=data/simulation/sim_scenario1.rds \
--niter=10000 \
--nburnin=4000 \
--nthin=1

#############################################
## scenario 2
#############################################

Rscript 1_runNimbleModel.R  \
--model=models/simulation/GPCM_normal.R \
--data=data/simulation/sim_scenario2.rds \
--niter=10000 \
--nburnin=4000 \
--nthin=1

Rscript 1_runNimbleModel.R  \
--model=models/simulation/GPCM_CAR.R \
--data=data/simulation/sim_scenario2.rds \
--niter=10000 \
--nburnin=4000 \
--nthin=1

Rscript 1_runNimbleModel.R  \
--model=models/simulation/GPCM_SSCAR.R \
--data=data/simulation/sim_scenario2.rds \
--niter=10000 \
--nburnin=4000 \
--nthin=1

#############################################
## scenario 3
#############################################

Rscript 1_runNimbleModel.R  \
--model=models/simulation/GPCM_normal.R \
--data=data/simulation/sim_scenario3.rds \
--niter=10000 \
--nburnin=4000 \
--nthin=1

Rscript 1_runNimbleModel.R  \
--model=models/simulation/GPCM_CAR.R \
--data=data/simulation/sim_scenario3.rds \
--niter=10000 \
--nburnin=4000 \
--nthin=1

Rscript 1_runNimbleModel.R  \
--model=models/simulation/GPCM_SSCAR.R \
--data=data/simulation/sim_scenario3.rds \
--niter=10000 \
--nburnin=4000 \
--nthin=1

#############################################
## 2) RUN APPLICATION
#############################################

Rscript 1_runNimbleModel.R  \
--model=models/students/GPCM_normal.R \
--data=data/students/students.RData \
--niter=50000 \
--nburnin=10000 \
--nthin=1

Rscript 1_runNimbleModel.R  \
--model=models/students/GPCM_CAR.R \
--data=data/students/students.RData \
--niter=50000 \
--nburnin=10000 \
--nthin=1

Rscript 1_runNimbleModel.R  \
--model=models/students/GPCM_SSCAR.R \
--data=data/students/students.RData \
--niter=50000 \
--nburnin=10000 \
--nthin=1
