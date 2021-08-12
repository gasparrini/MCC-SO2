################################################################################
# MCC-POLLUTION PROJECT: PRELIMINARY SO2 ANALYSIS 
################################################################################

################################################################################
# NON-LINEAR EXPOSURE RESPONSE
################################################################################

# DEFINE A FUNCTION FOR A LINEAR SPLINE
# linspl <- function(x, knots) {
#   basis <- lapply(knots, function(k) pmax(x-k,0))
#   basis <- do.call(cbind, c(list(x), basis))
#   attr(basis, "knots") <- knots
#   basis
# }

# SET PARAMETERS FOR LINEAR SPLINE
linknots <- c(20, 50, 100, 150)
nlargvarso2 <- list(fun="bs", knots=linknots, degree=1)

# LOOP ACROSS CITIES (SEE 03.firststage.R)
cl <- parallel::makeCluster(max(1,ncores-2))
registerDoParallel(cl)
nlstage1list <- foreach(data=dlist, i=seq(dlist), .packages=pack) %dopar% {
  
  # REPEAT STEPS OF FIRST-STAGE
  subrange <- range(seq(nrow(data))[!is.na(data$so2)&!is.na(data$tmean)])
  data <- data[subrange[1]:subrange[2],]
  data$y <- if(indnonext[i]) as.integer(data$nonext) else data[[out]]
  knotstmean <- quantile(data$tmean, c(10,75,90)/100, na.rm=T)
  argvartmean <- list(fun="bs", knots=knotstmean, degree=2)
  cbtmean <- crossbasis(data$tmean, lag=lagtmean, argvar=argvartmean,
    arglag=arglagtmean)
  spltime <- ns(data$date, df=round(dftime*nrow(data)/365.25))
  
  # DEFINE A LINEAR SPLINE FOR MOVING AVERAGE
  # NB: TO AVOID NEED OF REDUCING DLNMS IN THE PRESENCE OF MISSING COEF
  cbso2 <- do.call(onebasis, c(list(x=runMean(data$so2, 0:lagso2)), nlargvarso2))

  # FIT THE MODEL AND COLLECT PARS MANUALLY
  mod <- glm(formula(deparse(fmod)), data, family=quasipoisson)
  list(coefall=coef(mod)[2:6], vcovall=vcov(mod)[2:6,2:6])
}
names(nlstage1list) <- cities$city
stopCluster(cl)

# EXTRACT ESTIMATES FROM FIRST-STAGE MODELS
nlcoefall <- drop(t(sapply(nlstage1list, "[[", "coefall")))
nlvcovall <- lapply(nlstage1list, "[[", "vcovall")

# PERFORM THE META-ANALYSIS
# NB: REDUCE (R)IGLS ITERATIONS TO SPEED UP
nlmeta <- mixmeta(nlcoefall, nlvcovall, random=~1|country/city, data=cities, 
  control=list(showiter=T, igls.inititer=10))
#summary(meta)

# COMPUTE THE PROPORTION OF STUDIES BY EXPOSURE RANGE
proprange <- rowSums(sapply(dlist, function(x) 
  0:200 > min(x$so2, na.rm=T) & 0:200 < max(x$so2, na.rm=T))) / length(dlist)*100

# SAVE THE WORKSPACE
#save.image("temp/temp.RData")
