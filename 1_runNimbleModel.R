##-----------------------------------------#
## last update: 2026-08-12
## R version 4.5.3
## nimble version 1.4.1
##-----------------------------------------#
## This scripts runs a nimble model
##
## Results are saved in
##   <dirResults>/<data file name>/<model type>/<model file name>.rds
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
## Interface with the model scripts in models/
##
## this script defines, before sourcing the model script:
##   dataType     "simulation" (one exam, wide format) or
##                "students"   (several exams, long format)
##   Y            scores, coded 1:K. A students x items matrix for the
##                simulation, a vector with one row per response otherwise
##   scores       raw scores per student (used to initialize the abilities)
##   K            number of response categories. A scalar for the simulation,
##                an exams x items matrix otherwise
##   W            adjacency matrix of the network. For the students data it
##                covers the connected students only
##   W_nimble     as.carAdjacency(W)
##   CM           as.carCM(...), the proper CAR parametrization
##   Cmatrix      CAR_calcCmatrix(...), used to standardize the abilities
##   allData      the students data, only when dataType is "students"
##
## each model script must define:
##   modelCode    nimbleCode
##   data         list of data for nimbleModel
##   constants    list of constants for nimbleModel
##   inits        list of initial values
##   monitors     parameters to monitor
##   abilityNode  (optional) node initialized with the raw scores, defaults
##                to "theta"; set it to NULL to initialize the abilities
##                within the model script
##-----------------------------------------##
## TMP - for testing the script
##-----------------------------------------##
# args <- list()
# args$model= "models/PCM_model.R"
# args$data = "data/simulation/sim_scenario1.rds"
# args$niter 		= 10
# args$nburnin 	= 5
# args$nthin 		= 1


##-----------------------------------------##
## Load libraries and functions
##-----------------------------------------##
library(nimble)
library(here)

##-----------------------------------------##
## Set variables from args list
##-----------------------------------------##
calcWAIC <- TRUE

## results directory
if(is.null(args$dirResults)) dir <- "output" else dir <- args$dirResults

## filename used for output
filename <- unlist(strsplit(basename(args$model), "[\\.]"))[1]

## MCMC settings
MCMCcontrol 		<- list()
MCMCcontrol$niter 	<- as.numeric(args$niter)
MCMCcontrol$nburnin <- as.numeric(args$nburnin)
if(is.null(args$nthin)) MCMCcontrol$nthin <- 1 else MCMCcontrol$nthin <- as.numeric(args$nthin)

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
	## one exam, wide format
	dataType 		<- "simulation"

	## simuled data contains also true values on simulation
	simData 	<- readRDS(args$data)

	## matrix of scores
	Y 					<- simData$data + 1
	## computing raw scores
	scores 			<- rowSums(simData$data)

	## adjacency matrix - added in constants
	W <- simData$W

	K <- 4

} else {
	## students data: several exams, long format, one row per response.
	## Students are ordered so that the connected ones come first, the last
	## nIsolated students have no link in the network
	dataType 		<- "students"

	load(args$data)		## provides allData

	## responses, already coded 1:K
	Y 					<- allData$marks$y
	## raw score of each student, over the single exam they sat
	scores 			<- as.numeric(tapply(allData$marks$y, allData$marks$student, sum))

	## adjacency matrix of the connected students only
	W <- allData$networkMatrix

	## number of categories of each exam and item
	K <- allData$K[, -1] + 1
}

## createa adjacency matrix
W_nimble = as.carAdjacency(W)

## quantities used by the CAR models (ignored by models without a CAR prior)
## C, M define the proper CAR parametrization; Cmatrix is used to standardize
## the ability parameters
CM      <- as.carCM(W_nimble$adj, W_nimble$weights, W_nimble$num)
Cmatrix <- CAR_calcCmatrix(C = CM$C, adj = W_nimble$adj, num = W_nimble$num)

##---------------------------------------##
## Source model code, and definition of data, constants, inits
source(args$model)
##---------------------------------------##

## initialize students ability with raw scores
## each model script declares which node holds the abilities: theta when it is
## stochastic, thetaTmp for the CAR models where theta is deterministic.
## A model that sets abilityNode to NULL builds these inits itself, as the
## application CAR models do, where the abilities are split between the
## connected and the isolated students
if(!exists("abilityNode")) abilityNode <- "theta"

Sscores <- (scores - mean(scores))/sd(scores)

if(!is.null(abilityNode)) inits[[abilityNode]] <- Sscores


##---------------------------------------------------##
## Create model and MCMC configuration
##---------------------------------------------------##

model <- nimbleModel(code 			= modelCode,
										 data 			= data,  
										 constants	= constants,
										 inits 			= inits, 
										 calculate 	= TRUE)

##---------------------------------------------------##
## SP: ?? SOME SETTING UNDECIDED - here in case we 
## are interested in setting up monitoring the log-likelihood
## or log posterior of the model
##---------------------------------------------------##
## update monitors - placeholder if we want to monitor the loklikelihood
## monitors <- c(monitors, "myLogProbAll", "myLogProbSome", "myLogLik")

# Flag for WAIC
if(calcWAIC) {
	## conditional WAIC - all the responses of a student form one observation,
	## so that WAIC compares the models on predicting a new student rather
	## than a new item response; this is what differs across the models
	if(dataType == "simulation") {
		## wide format, one row of Y per student
		groups <- paste0('Y[', 1:constants$N, ', 1:', constants$nItems, ']')
	} else {
		## long format, the rows of a student are scattered over Y, so each
		## group is a vector of node names.
		## Note: lapply, not sapply. Every student has the same number of
		## responses, so sapply would return a matrix and the grouping would
		## be silently ignored. unname because nimble compares the expanded
		## node names to the data nodes with identical(), and the names left
		## by split() alone make that comparison fail
		indList <- split(seq_along(allData$marks$student), allData$marks$student)
		groups  <- unname(lapply(indList, function(x) paste0('Y[', x, ']')))
	}

	## note: the argument is controlWAIC, a wrong name would be absorbed by
	## the dots of configureMCMC and the grouping silently ignored
	mcmcConf <- configureMCMC(model,
		monitors = monitors,
		enableWAIC = TRUE,
		controlWAIC = list(dataGroups = groups))

} else {
	mcmcConf <- configureMCMC(model, monitors = monitors)

}

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
				   thin     = MCMCcontrol$nthin,
				   setSeed  = seed,
				   WAIC = calcWAIC)
	)
	if(inherits(res, 'try-error')) {
  		warning(paste0("There was a problem running nimble MCMC."))
  }
})

##---------------------------------------------------##
## Save results, times, settings
##---------------------------------------------------##
## runMCMC returns a list when WAIC = TRUE, keep results$samples a matrix
if(calcWAIC) samples <- res$samples else samples <- res

results <- list(samples  = samples,
				compilationTime  = compilationTime,
				samplingTime     = runningTime*(1 - MCMCcontrol$nburnin/MCMCcontrol$niter),
				runningTime      = runningTime,
				MCMCcontrol      = MCMCcontrol)

##---------------------------------------------------##
## WAIC - both variants, computed on the same samples
## grouped   : one observation is a student, i.e. all their responses.
##             Sensitive to the prior on the abilities, which is what
##             differs across the models.
## pointwise : one observation is a single item response, nimble's default.
##             Mostly reflects item fit.
## Both are conditional on the sampled abilities. The two are on different
## scales: compare models within a variant, never across variants.
##---------------------------------------------------##
if(calcWAIC) {
	results$WAICgrouped   <- Cmcmc$getWAIC()
	results$WAICpointwise <- calculateWAIC(samples, model)
}


##---------------------------------------------------##
## Set up directory names to save for output
##---------------------------------------------------##
## the model file name already identifies the model, e.g.
## output/sim_scenario1/GPCM_CAR.rds
dataName        <- unlist(strsplit(basename(args$data), "[.]"))[1]

outDir <- paste0(dir, "/", dataName, "/")
filenameOutput <- paste0(outDir, filename, ".rds")

dir.create(file.path(outDir), 
						recursive = TRUE, 
						showWarnings = FALSE)


saveRDS(results, file = filenameOutput )
