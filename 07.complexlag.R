################################################################################
# MORE COMPLEX LAG
################################################################################

# SET PARAMETERS FOR LAG-RESPONSE
cllagso2 <- 7
clarglagso2 <- list(fun="ns", knots=c(1,3), int=T)

# LOOP ACROSS CITIES (SEE 03.firststage.R)
cl <- parallel::makeCluster(max(1,ncores-2))
registerDoParallel(cl)
clstage1list <- foreach(data=dlist, i=seq(dlist), .packages=pack) %dopar% {

  # REPEAT STEPS OF FIRST-STAGE
  subrange <- range(seq(nrow(data))[!is.na(data$so2)&!is.na(data$tmean)])
  data <- data[subrange[1]:subrange[2],]
  knotstmean <- quantile(data$tmean, c(10,75,90)/100, na.rm=T)
  argvartmean <- list(fun="bs", knots=knotstmean, degree=2)
  cbtmean <- crossbasis(data$tmean, lag=lagtmean, argvar=argvartmean,
    arglag=arglagtmean)
  spltime <- ns(data$date, df=round(dftime*nrow(data)/365.25))
  data$dow <- weekdays(data$date)

  # DEFINE CROSSBASIS FOR SO2 (MODIFIED) 
  cbso2 <- crossbasis(data$so2, lag=cllagso2, argvar=list("lin"),
    arglag=clarglagso2)

  # RUN THE MODEL
  mod <- glm(death ~ cbso2 + cbtmean + dow + spltime, data, family=quasipoisson)

  # EXTRACT THE ESTIMATES (OVERALL CUMULATIVE AND LAG-RESPONSE FOR 10-UNIT)
  crlag <- crossreduce(cbso2, mod, type="var", value=10, cen=0)

  # STORE
  list(coeflag=coef(crlag), vcovlag=vcov(crlag))
}
names(stage1list) <- cities$city
stopCluster(cl)

# EXTRACT ESTIMATES FROM FIRST-STAGE MODELS
clcoeflag <- drop(t(sapply(clstage1list, "[[", "coeflag")))
clvcovlag <- lapply(clstage1list, "[[", "vcovlag")

# PERFORM THE META-ANALYSIS
# NB: REDUCE (R)IGLS ITERATIONS TO SPEED UP
clmeta <- mixmeta(clcoeflag, clvcovlag, random=~1|country/city, data=cities, 
  control=list(showiter=T, igls.inititer=10))
#summary(clmeta)
