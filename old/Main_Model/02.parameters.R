################################################################################
# MCC-POLLUTION PROJECT: PRELIMINARY SO2 ANALYSIS 
################################################################################

library(dlnm) ; library(mixmeta) ; library(splines)
library(tsModel) ; library(mgcv)

################################################################################
# DEFINE THE MAIN PARAMETERS FOR THE ANALYSIS
################################################################################

# DEGREE OF FREEDOM FOR SEASONALITY
dfseas <- 7

# CREATE INDICATOR FOR ONLY NON-EXTERNAL MORTALITY
indnonext <- sapply(dlist,function(x) !"all"%in%names(x))

# DEFINE THE OUTCOME
out <- "all"

# FUNCTION FOR COMPUTING THE Q-AIC
QAIC <- function(model) {
  phi <- summary(model)$dispersion
	loglik <- sum(dpois(model$y, model$fitted.values, log=TRUE))
  return(-2*loglik + 2*summary(model)$df[3]*phi)
}

#
