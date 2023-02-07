################################################################################
# DEFINE THE MAIN PARAMETERS FOR THE ANALYSIS
################################################################################

# PARAMETERS OF FUNCTION FOR TEMPERATURE
arglagtmean <- list(fun="strata", breaks=1)
lagtmean <- 3

# DEGREE OF FREEDOM FOR TREND
dftime <- 7

# DEFINE THE FIRST-STAGE MODEL FORMULA
fmod <- death ~ runMean(so2,0:3) + cbtmean + dow + spltime

# FUNCTION FOR COMPUTING THE Q-AIC
QAIC <- function(model) {
  phi <- summary(model)$dispersion
	loglik <- sum(dpois(model$y, model$fitted.values, log=TRUE))
  return(-2*loglik + 2*summary(model)$df[3]*phi)
}

# MULTIPLIER FOR 95%CI
qn <- qnorm(0.975)

# PARALLELIZATION
ncores <- detectCores()
pack <- c("dlnm", "data.table", "gnm", "tsModel", "splines", "MASS", "abind")
