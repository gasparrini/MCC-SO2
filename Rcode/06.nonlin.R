################################################################################
# R code for reproducing the analysis in:
#
#  O'Brien E, et al. Short-Term association between sulfur dioxide and 
#    mortality: a multicountry analysis in 399 cities. Environmental Health
#    Perspectives. 2023;131(3):37002.
#  https://doi.org/10.1289/ehp11112
#
# * an updated version of this code, compatible with future versions of the
#   software, is available at:
#   https://github.com/gasparrini/MCC-SO2
################################################################################

# SET PARAMETERS FOR THE EXPOSURE-RESPONSE
nlargvarso2 <- list(fun="poly", degree=5, scale=100)
dfso2 <- 5

# LOOP ACROSS CITIES (SEE 03.firststage.R)
cl <- parallel::makeCluster(max(1,ncores-2))
registerDoParallel(cl)
nlstage1list <- foreach(data=dlist, i=seq(dlist), .packages=pack) %dopar% {
  
  # REPEAT STEPS OF FIRST-STAGE
  subrange <- range(seq(nrow(data))[!is.na(data$so2)&!is.na(data$tmean)])
  data <- data[subrange[1]:subrange[2],]
  knotstmean <- quantile(data$tmean, c(10,75,90)/100, na.rm=T)
  argvartmean <- list(fun="bs", knots=knotstmean, degree=2)
  cbtmean <- crossbasis(data$tmean, lag=lagtmean, argvar=argvartmean,
    arglag=arglagtmean)
  spltime <- ns(data$date, df=round(dftime*nrow(data)/365.25))
  data$dow <- weekdays(data$date)

  # DEFINE A SPLINE FUNCTION FOR MOVING AVERAGE
  # NB: TO AVOID NEED OF REDUCING DLNMS IN THE PRESENCE OF MISSING COEF
  bso2 <- do.call(onebasis, c(list(x=runMean(data$so2, 0:3)), nlargvarso2))

  # FIT THE MODEL
  mod <- glm(death ~ bso2 + cbtmean + dow + spltime, data, family=quasipoisson)
  
  # EXTRACT THE ESTIMATES
  ind <- seq(dfso2)+1
  coefall <- coef(mod)[ind]
  vcovall <- vcov(mod)[ind,ind]
  
  list(coefall=coefall, vcovall=vcovall)
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
#summary(nlmeta)
