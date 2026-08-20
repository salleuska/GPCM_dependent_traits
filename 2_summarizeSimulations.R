##-----------------------------------------#
## last update: 2026-08-14
## R version 4.5.3
##-----------------------------------------#
## Summarizes the models fitted to one simulated scenario:
## posterior summaries, comparison with the true values, WAIC, traceplots
##-----------------------------------------#
args <- R.utils::commandArgs(asValue=TRUE)

## Accepted arguments
## data 				path to the scenario file, holding the true values
## dirResults	  (optional) directory with the results, default "output"
##
## Reads the samples from <dirResults>/<scenario> and writes plots and
## tables to <dirResults>/<scenario>/figures. The figures directory at the
## root of the project is left for the figures of the paper
##-----------------------------------------##
## TMP - for testing the script
##-----------------------------------------##
# args <- list()
# args$data = "data/simulation/sim_scenario1.rds"

library(ggplot2)
library(bayestestR)

if(is.null(args$dirResults)) dirResults <- "output"  else dirResults <- args$dirResults

dataName    <- unlist(strsplit(basename(args$data), "[.]"))[1]
samplesDir  <- file.path(dirResults, dataName)
figuresDir  <- file.path(dirResults, dataName, "figures")

dir.create(figuresDir, recursive = TRUE, showWarnings = FALSE)

modelNames <- c("GPCM_normal", "GPCM_CAR", "GPCM_SSCAR")

cat("##--------------------------------##\n")
cat("Scenario ", dataName, "\n")
cat("Samples  ", samplesDir, "\n")
cat("##--------------------------------##\n")

##---------------------------------------##
## Read true values and results
##---------------------------------------##
simData <- readRDS(args$data)

results <- list()
for(m in modelNames) {
	f <- file.path(samplesDir, paste0(m, ".rds"))
	if(file.exists(f)) results[[m]] <- readRDS(f) else warning("missing results: ", f)
}

if(!length(results)) stop("no results found in ", samplesDir)

##---------------------------------------##
## Columns to compare with the true values.
## The names are built rather than searched for, so that they are in the same
## order as the true values. beta[j, 1] is fixed to 0 and is left out
##---------------------------------------##
N <- length(simData$ability)
p <- nrow(simData$difficulties)
K <- ncol(simData$difficulties)

parameters <- list(
	ability        = list(cols = paste0("theta[", 1:N, "]"),
						  true = simData$ability),
	difficulty     = list(cols = paste0("beta[", rep(1:p, times = K - 1), ", ",
										rep(2:K, each = p), "]"),
						  true = as.vector(simData$difficulties[, 2:K])),
	discrimination = list(cols = paste0("alpha[", 1:p, "]"),
						  true = simData$discrimination)
)

##---------------------------------------##
## Posterior mean and 95% HDI of every parameter, for every model
##---------------------------------------##
estimates <- list()

for(m in names(results)) {
	samples <- results[[m]]$samples

	for(v in names(parameters)) {
		cols <- parameters[[v]]$cols
		hdi  <- sapply(cols, function(cc) {
					h <- bayestestR::ci(samples[, cc], method = "HDI")
					c(h$CI_low, h$CI_high)
				})

		estimates[[paste(m, v)]] <- data.frame(
			model     = m,
			parameter = v,
			true      = parameters[[v]]$true,
			mean      = colMeans(samples[, cols]),
			lower     = hdi[1, ],
			upper     = hdi[2, ],
			row.names = NULL)
	}
}

estimates <- do.call(rbind, estimates)
estimates$model <- factor(estimates$model, levels = modelNames)

ggsave(file.path(figuresDir, "estimates.pdf"),
	ggplot(estimates, aes(x = true, y = mean)) +
		geom_abline(intercept = 0, slope = 1, col = "grey60") +
		geom_linerange(aes(ymin = lower, ymax = upper), col = "grey70") +
		geom_point(size = 1) +
		facet_grid(model ~ parameter, scales = "free") +
		labs(x = "True value", y = "Posterior mean") +
		theme_classic(14),
	width = 10, height = 10)

##---------------------------------------##
## Mean absolute error and mean squared error of the posterior means with
## respect to the true simulation values, for each model and each group of
## parameters
##---------------------------------------##
estimates$absError <- abs(estimates$mean - estimates$true)
estimates$sqError  <- (estimates$mean - estimates$true)^2

accuracyTable <- aggregate(cbind(MAE = absError, MSE = sqError) ~ model + parameter,
						   data = estimates, FUN = mean)

## one row per model, the three groups of parameters side by side
accuracyTable <- reshape(accuracyTable, idvar = "model", timevar = "parameter",
						 direction = "wide")

accuracyTable <- accuracyTable[order(match(accuracyTable$model, modelNames)), ]

##---------------------------------------##
## Correlation parameter of the CAR models.
## Under the spike and slab gamma is 0 whenever g is 0, so it is summarized
## on the iterations with g = 1 and the inclusion probability is mean(g)
##---------------------------------------##
gammaDraws  <- list()
gammaTable  <- list()

for(m in names(results)) {
	samples <- results[[m]]$samples
	if(!"gamma" %in% colnames(samples)) next

	if("g" %in% colnames(samples)) keep <- samples[, "g"] == 1 else keep <- rep(TRUE, nrow(samples))
	hdi <- bayestestR::ci(samples[keep, "gamma"], method = "HDI")

	gammaDraws[[m]] <- data.frame(model = m, gamma = samples[keep, "gamma"])
	gammaTable[[m]] <- data.frame(model     = m,
								  gammaMean = mean(samples[keep, "gamma"]),
								  gammaLow  = hdi$CI_low,
								  gammaUp   = hdi$CI_high,
								  inclusion = if("g" %in% colnames(samples)) mean(samples[, "g"]) else NA)
}

if(length(gammaDraws)) {
	gammaDraws <- do.call(rbind, gammaDraws)
	gammaDraws$model <- factor(gammaDraws$model, levels = modelNames)

	pGamma <- ggplot(gammaDraws, aes(x = gamma, y = after_stat(density))) +
		geom_histogram(bins = 40, col = "black", fill = "grey80") +
		facet_wrap(~ model, nrow = 1) +
		labs(x = expression(gamma), y = "density") +
		theme_classic(14)

	## scenario 2 is generated from a CAR with a known correlation
	if(!is.null(simData$rho))
		pGamma <- pGamma + geom_vline(xintercept = simData$rho, col = "red", lty = "dashed")

	ggsave(file.path(figuresDir, "gamma.pdf"), pGamma, width = 10, height = 4)
}

##---------------------------------------##
## WAIC, both variants
##---------------------------------------##
# waicTable <- list()

# for(m in names(results)) {
# 	r <- results[[m]]
# 	waicTable[[m]] <- data.frame(
# 		model          = m,
# 		WAICgrouped    = as.numeric(r$WAICgrouped$WAIC),
# 		pWAICgrouped   = as.numeric(r$WAICgrouped$pWAIC),
# 		WAICpointwise  = as.numeric(r$WAICpointwise$WAIC),
# 		pWAICpointwise = as.numeric(r$WAICpointwise$pWAIC),
# 		runningTime    = r$runningTime[["elapsed"]])
# }

# waicTable <- do.call(rbind, waicTable)

##---------------------------------------##
## Traceplots of a few parameters per model
##---------------------------------------##
traceDraws <- list()

for(m in names(results)) {
	samples <- results[[m]]$samples
	cols    <- intersect(c("theta[1]", "theta[2]", "alpha[1]", "beta[1, 2]", "gamma", "g"),
						 colnames(samples))

	for(cc in cols)
		traceDraws[[paste(m, cc)]] <- data.frame(model = m, parameter = cc,
												 iteration = seq_len(nrow(samples)),
												 value = samples[, cc])
}

traceDraws <- do.call(rbind, traceDraws)
traceDraws$model <- factor(traceDraws$model, levels = modelNames)

ggsave(file.path(figuresDir, "trace.pdf"),
	ggplot(traceDraws, aes(x = iteration, y = value)) +
		geom_line(linewidth = 0.2) +
		facet_wrap(model ~ parameter, scales = "free_y", ncol = 3) +
		labs(x = "iteration", y = "") +
		theme_classic(10),
	width = 12, height = 12)

##---------------------------------------##
## Save and report
## everything goes in <dirResults>/<scenario>, the tables both as csv and,
## together with the parameter by parameter estimates, in summaries.rds
##---------------------------------------##
if(length(gammaTable)) gammaTable <- do.call(rbind, gammaTable) else gammaTable <- NULL

saveRDS(list(estimates = estimates,
			 accuracy  = accuracyTable,
			 gamma     = gammaTable),
		file = file.path(figuresDir, "summaries.rds"))

write.csv(accuracyTable, file = file.path(figuresDir, "accuracy.csv"),  row.names = FALSE)
# write.csv(waicTable,     file = file.path(figuresDir, "waic.csv"),      row.names = FALSE)
write.csv(estimates,     file = file.path(figuresDir, "estimates.csv"), row.names = FALSE)

if(!is.null(gammaTable))
	write.csv(gammaTable, file = file.path(figuresDir, "gamma.csv"), row.names = FALSE)

cat("\nMAE and MSE of the posterior means\n")
print(accuracyTable, row.names = FALSE)

# cat("\nWAIC\n")
# print(waicTable, row.names = FALSE)

if(!is.null(gammaTable)) {
	cat("\nCAR correlation parameter\n")
	print(gammaTable, row.names = FALSE)
}

cat("\nplots and tables written to", figuresDir, "\n")
