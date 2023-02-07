################################################################################
# FIRST-STAGE ANALYSIS: RUN THE MODEL IN EACH CITY, REDUCE AND SAVE
################################################################################

################################################################################
# RUN THE LOOP

# PRINT WARNINGS IMMEDIATELY (TO DETECT ISSUES)
options(warn=1)

# PREPARE THE PARALLELIZATION (REGISTER CLUSTERS AND WRITE TRACE FILE)
cl <- parallel::makeCluster(max(1,ncores-2))
registerDoParallel(cl)
writeLines(c(""), "temp/logstage1.txt")
cat(as.character(as.POSIXct(Sys.time())),file="temp/logstage1.txt",append=T)

# LOOP ACROSS CITIES
stage1list <- foreach(data=dlist, i=seq(dlist), .packages=pack) %dopar% {
  
  # STORE ITERATION (1 EVERY 10)
  if(i%%10==0) cat("\n", "iter=",i, as.character(Sys.time()), "\n",
    file="temp/logstage1.txt", append=T)

  # SUBSET WITH EXCLUDED INITIAL AND/OR FINAL MISSING PERIODS
  subrange <- range(seq(nrow(data))[!is.na(data$so2)&!is.na(data$tmean)])
  data <- data[subrange[1]:subrange[2],]
  
  # DEFINE CROSSBASIS FOR TEMPERATURE
  knotstmean <- quantile(data$tmean, c(10,75,90)/100, na.rm=T)
  argvartmean <- list(fun="bs", knots=knotstmean, degree=2)
  cbtmean <- crossbasis(data$tmean, lag=lagtmean, argvar=argvartmean,
    arglag=arglagtmean)
  
  # DEFINE THE SPLINE OF TIME AND THE DAY OF THE WEEK INDICATORS
  spltime <- ns(data$date, df=round(dftime*nrow(data)/365.25))
  data$dow <- weekdays(data$date)

  # RUN THE MODEL (RE-CREATE FORMULA TO AVOID ISSUE WITH ENVIRONMENTS)
  mod <- glm(formula(deparse(fmod)), data, family=quasipoisson)
  
  # EXTRACT THE ESTIMATES
  ind <- grep("so2", names(coef(mod)))
  coefall <- coef(mod)[ind]
  vcovall <- vcov(mod)[ind,ind]

  # STORE
  list(coefall=coefall, vcovall=vcovall, conv=mod$converged,
    qaic=QAIC(mod), sumso2=quantile(data$so2, 0:100/100, na.rm=T))
}
names(stage1list) <- cities$city

# REMOVE PARALLELIZATION
stopCluster(cl)
#file.remove("temp/logstage1.txt")
