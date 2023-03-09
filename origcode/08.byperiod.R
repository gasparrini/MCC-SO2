################################################################################
# Original R code for the analysis in:
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

################################################################################
# BY PERIOD
################################################################################

# DEFINE APPROXIMATE STEP IN YEARS
ystep <- 5

# LOOP ACROSS CITIES (SEE 03.firststage.R)
cl <- parallel::makeCluster(max(1,ncores-2))
registerDoParallel(cl)
perstage1list <- foreach(data=dlist, i=seq(dlist), .packages=pack) %dopar% {

  # REPEAT STEPS OF FIRST-STAGE
  subrange <- range(seq(nrow(data))[!is.na(data$so2)&!is.na(data$tmean)])
  data <- data[subrange[1]:subrange[2],]
  data$y <- if(indnonext[i]) as.integer(data$nonext) else data[[out]]
  knotstmean <- quantile(data$tmean, c(10,75,90)/100, na.rm=T)
  argvartmean <- list(fun="bs", knots=knotstmean, degree=2)
  cbtmean <- crossbasis(data$tmean, lag=lagtmean, argvar=argvartmean,
    arglag=arglagtmean)
  spltime <- ns(data$date, df=round(dftime*nrow(data)/365.25))
  
  # DEFINE THE PERIODS
  nstep <- max(round(nrow(data)/(ystep*365.25)), 1)
  step <- nrow(data)/nstep
  period <- cut(seq(nrow(data)), step*(0:nstep))
  levels(period) <- paste0("per",seq(nstep))
  midyear <- year(data$date[1]) + 
    (yday(data$date[1]) + step*(1:nstep) - step/2)/365.25

  # RUN THE MODEL (FORMULA DEPENDENT ON PRESENCE OF MULTIPLE PERIODS)
  fmodalt <- if(nstep==1) y ~ runMean(so2,0:3) + cbtmean + dow + spltime else 
    y ~ runMean(so2,0:3):period + cbtmean + dow + spltime
  mod <- glm(fmodalt, data, family=quasipoisson)
  
  # IDENTIFY PERIODS REMOVED FROM ESTIMATION
  nmper <-unique(period[!seq(nrow(data))%in%mod$na.action])

  # EXTRACT THE ESTIMATES (ONLY VARIANCES TO PUT THEM IN LONG FORMAT LATER)
  ind <- grepl("so2", names(coef(mod)))
  coefall <- coef(mod)[ind]
  vcovall <- diag(vcov(mod))[ind]
  names(coefall) <- names(vcovall) <- nmper
  midyear <- midyear[levels(period) %in% nmper]

  # STORE
  list(coefall=coefall, vcovall=vcovall, midyear=midyear)
}
names(perstage1list) <- cities$city
stopCluster(cl)

# EXTRACT ESTIMATES FROM FIRST-STAGE MODELS
coefall <- unlist(lapply(perstage1list, "[[", "coefall"))
vcovall <- unlist(lapply(perstage1list, "[[", "vcovall"))
midyear <- unlist(lapply(perstage1list, "[[", "midyear"))

# NUMBER OF PERIODS BY CITY
nper <- sapply(perstage1list, function(x) length(x$coefall))

# PUT THEM IN A DATAFRAME
dataper <- data.frame(coef=coefall, vcov=vcovall, midyear=midyear,
  city=rep(cities$city, nper), country=rep(cities$country, nper)
)

# PERFORM THE META-ANALYSIS
# NB: EXCLUDE TEHRAN TO AVOID CONVERGENCE ISSUES
permeta <- mixmeta(coef~I(midyear-2000), vcov, random=~1|country/city, 
  data=subset(dataper, country!="irn0215"), 
  control=list(showiter=T, igls.inititer=10))
#summary(permeta)
