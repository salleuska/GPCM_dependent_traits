#!/bin/bash

#############################################
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
--nburnin=5000 \
--nthin=1 

Rscript 1_runNimbleModel.R  \
--model=models/simulation/GPCM_CAR.R \
--data=data/simulation/sim_scenario1.rds \
--niter=10000 \
--nburnin=5000 \
--nthin=1 

Rscript 1_runNimbleModel.R  \
--model=models/simulation/GPCM_SSCAR.R \
--data=data/simulation/sim_scenario1.rds \
--niter=10000 \
--nburnin=5000 \
--nthin=1 

#############################################
## scenario 2
#############################################

Rscript 1_runNimbleModel.R  \
--model=models/simulation/GPCM_normal.R \
--data=data/simulation/sim_scenario2.rds \
--niter=10000 \
--nburnin=5000 \
--nthin=1 

Rscript 1_runNimbleModel.R  \
--model=models/simulation/GPCM_CAR.R \
--data=data/simulation/sim_scenario2.rds \
--niter=10000 \
--nburnin=5000 \
--nthin=1 

Rscript 1_runNimbleModel.R  \
--model=models/simulation/GPCM_SSCAR.R \
--data=data/simulation/sim_scenario2.rds \
--niter=10000 \
--nburnin=5000 \
--nthin=1 

#############################################
## scenario 3
#############################################

Rscript 1_runNimbleModel.R  \
--model=models/simulation/GPCM_normal.R \
--data=data/simulation/sim_scenario3.rds \
--niter=10000 \
--nburnin=5000 \
--nthin=1 

Rscript 1_runNimbleModel.R  \
--model=models/simulation/GPCM_CAR.R \
--data=data/simulation/sim_scenario3.rds \
--niter=10000 \
--nburnin=5000 \
--nthin=1 

Rscript 1_runNimbleModel.R  \
--model=models/simulation/GPCM_SSCAR.R \
--data=data/simulation/sim_scenario3.rds \
--niter=10000 \
--nburnin=5000 \
--nthin=1 

