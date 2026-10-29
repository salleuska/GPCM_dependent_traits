##-----------------------------------------#
## last update: 2026-10-29
## R version 4.5.3
##-----------------------------------------#
## Puts together the results of all the simulated scenarios, to see which
## model works better where.
##
## Run after 2_summarizeSimulations.R on every scenario: it reads the tables
## that script writes, <dirResults>/<scenario>/figures/{accuracy,gamma}.csv,
## and the WAIC stored in the fitted models, <dirResults>/<scenario>/<model>.rds
##
##   Rscript 3_compareScenarios.R [--dirResults=output]
##
## Writes to <dirResults>/comparison/
##   comparison.csv       one row per network x scenario: ability MSE and WAIC
##                        of each model, best model, SSCAR gamma and P(network)
##   accuracy_long.csv    every accuracy measure, one row per
##                        network x scenario x model x parameter group
##   relative_error.pdf   MSE of each model divided by the MSE of GPCM_normal
##-----------------------------------------#
args <- R.utils::commandArgs(asValue=TRUE)

library(ggplot2)

if(is.null(args$dirResults)) dirResults <- "output"  else dirResults <- args$dirResults

outDir <- file.path(dirResults, "comparison")
dir.create(outDir, recursive = TRUE, showWarnings = FALSE)

networks   <- c("dense", "sparse")
scenarios  <- c("1" = "iid", "2" = "CAR", "3" = "mixture")
modelNames <- c("GPCM_normal", "GPCM_CAR", "GPCM_SSCAR")
groups     <- c("ability", "difficulty", "discrimination")

##---------------------------------------##
## Read the tables of every scenario
##---------------------------------------##
accuracy <- list()
summary  <- list()

for(net in networks) {
	for(s in names(scenarios)) {
		dataName <- paste0("sim_", net, "_scenario", s)
		figDir   <- file.path(dirResults, dataName, "figures")

		f <- file.path(figDir, "accuracy.csv")
		if(!file.exists(f)) {
			warning("missing ", f, ", run 2_summarizeSimulations.R on ", dataName)
			next
		}
		acc <- read.csv(f)

		## accuracy in long format: one row per model and parameter group
		for(g in groups) {
			accuracy[[length(accuracy) + 1]] <- data.frame(
				network  = net,
				scenario = scenarios[[s]],
				model    = acc$model,
				group    = g,
				MAE      = acc[[paste0("MAE.", g)]],
				MSE      = acc[[paste0("MSE.", g)]])
		}

		## WAIC, stored in the fitted models
		waic <- sapply(modelNames, function(m) {
			r <- file.path(dirResults, dataName, paste0(m, ".rds"))
			if(file.exists(r)) readRDS(r)$WAIC$WAIC else NA
		})

		## spike and slab: gamma and inclusion probability of the network
		gam <- file.path(figDir, "gamma.csv")
		ss  <- if(file.exists(gam)) subset(read.csv(gam), model == "GPCM_SSCAR") else NULL

		mse  <- setNames(acc$MSE.ability, acc$model)[modelNames]
		summary[[length(summary) + 1]] <- data.frame(
			network      = net,
			scenario     = scenarios[[s]],
			MSE_normal   = mse[["GPCM_normal"]],
			MSE_CAR      = mse[["GPCM_CAR"]],
			MSE_SSCAR    = mse[["GPCM_SSCAR"]],
			best_ability = sub("GPCM_", "", names(which.min(mse))),
			WAIC_normal  = waic[["GPCM_normal"]],
			WAIC_CAR     = waic[["GPCM_CAR"]],
			WAIC_SSCAR   = waic[["GPCM_SSCAR"]],
			best_WAIC    = sub("GPCM_", "", names(which.min(waic))),
			SSCAR_gamma  = if(NROW(ss)) sprintf("%.3f [%.3f, %.3f]",
										ss$gammaMean, ss$gammaLow, ss$gammaUp) else NA,
			P_network    = if(NROW(ss)) ss$inclusion else NA)
	}
}

if(!length(summary)) stop("no results found in ", dirResults)

accuracy <- do.call(rbind, accuracy)
summary  <- do.call(rbind, summary)

##---------------------------------------##
## Error of each model relative to GPCM_normal, the model that ignores the
## network: below 1 the model is better, above 1 it is worse
##---------------------------------------##
baseline <- subset(accuracy, model == "GPCM_normal",
				   select = c(network, scenario, group, MSE))
names(baseline)[names(baseline) == "MSE"] <- "MSE_normal"
accuracy <- merge(accuracy, baseline, by = c("network", "scenario", "group"))
accuracy$relMSE <- accuracy$MSE / accuracy$MSE_normal

accuracy$scenario <- factor(accuracy$scenario, levels = scenarios)
accuracy$network  <- factor(accuracy$network,  levels = networks)
accuracy$model    <- factor(sub("GPCM_", "", accuracy$model),
							levels = sub("GPCM_", "", modelNames))
accuracy <- accuracy[order(accuracy$network, accuracy$scenario,
						   accuracy$group, accuracy$model), ]

##---------------------------------------##
## Figure: one tile per model and scenario, colored by the relative MSE.
## Blue = better than GPCM_normal, red = worse, gray = the same. The color
## scale is on log2, so 0.5x and 2x are equally far from gray, and it is
## capped at 0.5x and 2x
##---------------------------------------##
accuracy$label <- sprintf("%.2f", accuracy$relMSE)

pRel <- ggplot(accuracy, aes(x = model, y = scenario,
							 fill = pmin(pmax(log2(relMSE), -1), 1))) +
	geom_tile(col = "#fcfcfb", linewidth = 1) +
	geom_text(aes(label = label), col = "#0b0b0b", size = 3.5) +
	scale_fill_gradient2(low = "#2a78d6", mid = "#f0efec", high = "#e34948",
						 midpoint = 0, limits = c(-1, 1),
						 breaks = c(-1, 0, 1),
						 labels = c("2x better", "same", "2x worse"),
						 name = "MSE vs normal") +
	scale_y_discrete(limits = rev) +
	facet_grid(network ~ group) +
	labs(x = "model", y = "true abilities generated as",
		 title = "Error of each model relative to the normal model (MSE ratio)") +
	theme_classic(13) +
	theme(strip.background = element_blank(),
		  strip.text = element_text(face = "bold"),
		  axis.line = element_blank(), axis.ticks = element_blank())

ggsave(file.path(outDir, "relative_error.pdf"), pRel, width = 10, height = 5.5)

##---------------------------------------##
## Save and report
##---------------------------------------##
write.csv(summary, file.path(outDir, "comparison.csv"), row.names = FALSE)
write.csv(accuracy[, c("network", "scenario", "model", "group", "MAE", "MSE", "relMSE")],
		  file.path(outDir, "accuracy_long.csv"), row.names = FALSE)

show <- summary
num  <- sapply(show, is.numeric)
show[num] <- lapply(show[num], round, 3)
cat("\nAbility MSE, WAIC and network detection, by network and scenario\n")
print(show, row.names = FALSE)

cat("\ntables and figure written to", outDir, "\n")
