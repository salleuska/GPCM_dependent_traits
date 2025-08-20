##-----------------------------------------#
## last update: 
## R version 
## nimble version
##-----------------------------------------#
## This scripts runs a nimble model
##-----------------------------------------#
args <- R.utils::commandArgs(asValue=TRUE)

## Accepted arguments
## model 				path to R script with model code
## data 				path to data file
## niter        number of iterations for MCMC
## nburnin 			number of burnin iterations for MCMC
## nthin 				thinning (deafault is 1)
## dirResults	  (optional) path directory for results
##-----------------------------------------##
## TMP - for testing the script
##-----------------------------------------##
args <- list()
args$model= "models/PCM_model.R"
args$data = "data/simulation/sim_scenario1.rds"
args$niter 		= 1000
args$nburnin 	= 500
args$nthin 		= 1

##-----------------------------------------##
## Load libraries and functions
##-----------------------------------------##
library(nimble)
library(here)

##-----------------------------------------##
## Set variables from args list
##-----------------------------------------##
calcWAIC <- FALSE

## results directory
if(is.null(args$dirResults)) dir <- "output" else dir <- args$dirResults

## filename used for output
filename <- unlist(strsplit(basename(args$model), "[\\.]"))[1]

## MCMC settings
MCMCcontrol 		<- list()
MCMCcontrol$niter 	<- as.numeric(args$niter)
MCMCcontrol$nburnin <- as.numeric(args$nburnin)

## set seed based on slurm task id if available
task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
if(task_id == "") seed <- 1 else seed <- 1 + as.numeric(task_id)

MCMCcontrol$seed <- seed

cat("##--------------------------------##\n")
cat("Model ", filename, "\n")
cat("Data ", args$data, "\n")
cat("##--------------------------------##\n")

##---------------------------------------##
## Read data 
## Note: constants and inits are defined within each model script

if(grepl("sim", args$data)) {
	## simuled data contains also true values on simulation
	simData 	<- readRDS(args$data)

	## matrix of scores
	Y <- simData$data

	## adjacency matrix - added in constants
	W <- simData$W

}

## createa adjacency matrix 
W_nimble = as.carAdjacency(W)

##---------------------------------------##
## Source model code, and definition of data, constants, inits
source(args$model)
##---------------------------------------##

# scores 			<- 
# Sscores 		<- (scores - mean(scores))/sd(scores)
# inits$eta 	<- Sscores


##---------------------------------------------------##
## Create model and MCMC configuration
##---------------------------------------------------##

model <- nimbleModel(code 			= modelCode,
										 data 			= data,  
										 constants	= constants,
										 inits 			= inits, 
										 calculate 	= FALSE)

##---------------------------------------------------##
## SP: ?? SOME SETTING UNDECIDED - here in case we 
## are interested in setting up monitoring the log-likelihood
## or log posterior of the model
##---------------------------------------------------##


## update monitors - placeholder if we want to monitor the loklikelihood
## monitors <- c(monitors, "myLogProbAll", "myLogProbSome", "myLogLik")

## Flag for WAIC
# if(calcWAIC) {
# ## conditional WAIC - grouped students
#   if(grepl("timss", args$data)) {
# 		indList <- split(seq_along(alldata$id), alldata$id)
# 		groups <- sapply(indList, function(x)  paste0('y[', x, ']'))  
#   }
#   else {
# 		groups <- paste0('y[', 1:constants$N, ', 1:', constants$I, ']')
#   }


# 	mcmcConf <- configureMCMC(model, monitors = monitors, 
# 		enableWAIC = TRUE, 
# 		waicControl = list(dataGroups = groups))

# } else {
# 	mcmcConf <- configureMCMC(model, monitors = monitors)

# }


mcmcConf <- configureMCMC(model, monitors = monitors)
mcmc <- buildMCMC(mcmcConf)	

##---------------------------------------------------##
## Compile model & MCMC 
##---------------------------------------------------##
compilationTime <- system.time({
    Cmodel <- try(compileNimble(model))
    if(inherits(Cmodel, 'try-error')) {
      stop("There was a problem compiling the nimble model.")
    }
    Cmcmc <- try(compileNimble(mcmc, project = model))
    if(inherits(Cmodel, 'try-error')) {
      stop("There was a problem compiling the nimble MCMC.")
    }
})

##---------------------------------------------------##
## Run MCMC 
##---------------------------------------------------##
runningTime <- system.time({try(
	res <- runMCMC(Cmcmc, 
				   niter 	= MCMCcontrol$niter, 
				   nburnin  = MCMCcontrol$nburnin,
				   setSeed  = seed)
	)
	if(inherits(res, 'try-error')) {
  		warning(paste0("There was a problem running nimble MCMC."))
  }
})

if(grepl("semi", args$model)) {
  if(grepl("osce", args$data)) {
    outputG <- getSamplesDPmeasure(Cmcmc)
  }
}

##---------------------------------------------------##
## Save results, times, settings
##---------------------------------------------------##
results <- list(samples  = res,
				compilationTime  = compilationTime,
				samplingTime     = runningTime*(1 - MCMCcontrol$niter/MCMCcontrol$nburnin),
				runningTime      = runningTime,
				MCMCcontrol      = MCMCcontrol)


if(grepl("semi", args$model)) {
  if(grepl("osce", args$data)) {
    results$outputG <- outputG
  }
}

if(calcWAIC) results$modelWAIC <- Cmcmc$getWAIC()$WAIC


##---------------------------------------------------##
## Set up directory names to save for output
##---------------------------------------------------##
modelType       <- unlist(strsplit(basename(args$model), "[\\_\\.]"))[1]
dataName        <- unlist(strsplit(basename(args$data), "[.]"))[1]

outDir <- paste0(dir, "/", dataName, "/", modelType, "/")
filenameOutput <- paste0(outDir, filename, ".rds")

dir.create(file.path(outDir), 
						recursive = TRUE, 
						showWarnings = FALSE)


saveRDS(results, file = filenameOutput )
