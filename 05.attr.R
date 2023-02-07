################################################################################
# EXCESS MORTALITY
################################################################################

# NUMBER OF SIMULATION RUNS FOR COMPUTING EMPIRICAL CI
nsim <- 1000

# PREPARE THE PARALLELIZATION (REGISTER CLUSTERS AND WRITE TRACE FILE)
cl <- parallel::makeCluster(max(1,ncores-2))
registerDoParallel(cl)
writeLines(c(""), "temp/logattr.txt")
cat(as.character(as.POSIXct(Sys.time())),file="temp/logattr.txt",append=T)

# RUN THE LOOP
ansim <- foreach(data=dlist, i=seq(dlist), nm=names(dlist), .packages=pack,
  .combine=rbind) %dopar% {
  
  # STORE ITERATION (1 EVERY 10)
  if(i%%10==0) cat("\n", "iter=",i, as.character(Sys.time()), "\n",
    file="temp/logattr.txt", append=T)

  # DEFINE ONEBASIS FOR SO2 (TOTAL AND CUT TO 40)
  oneso2_1 <- onebasis(data$so2, "lin")
  oneso2_2 <- onebasis(pmin(data$so2,40), "lin")
  
  # FORWARD MOVING AVERAGE OF DEATHS
  y <- rowMeans(as.matrix(Lag(data$death, -lagtmean:0)))
  
  # YEAR
  period <- cut(year(data$date), seq(1980,2020,by=10), right=F,
    labels=paste0(seq(1980,2010,by=10),"-",seq(1980,2010,by=10)+9))
  
  # COMPUTE THE EXCESS DEATHS USING THE COUNTRY-SPECIFIC BLUPS
  anday1 <- (1-exp(-oneso2_1%*%blupcountry[i, "blup"]))*y
  anday2 <- (1-exp(-oneso2_2%*%blupcountry[i, "blup"]))*y
  an <- rbind(tot=tapply(anday1, period, sum, na.rm=T),
    below40=tapply(anday2, period, sum, na.rm=T))
  
  # SAMPLE THE COEF OF THE META-REGRESSION
  set.seed(13041975+i)
  coefsim <- mvrnorm(nsim, blupcountry[i, "blup"], (blupcountry[i, "se"])^2)
  
  # SIMULATED DISTRIBUTION OF EXCESS DEATHS
  ansim <- lapply(coefsim, function(b) {
    anday1 <- (1-exp(-oneso2_1%*%b))*y
    anday2 <- (1-exp(-oneso2_2%*%b))*y
    rbind(tot=tapply(anday1, period, sum, na.rm=T),
      below40=tapply(anday2, period, sum, na.rm=T))
  })
  
  # BIND TOGETHER
  ansim <- abind(c(list(an), ansim), along=3)
  dimnames(ansim)[[3]] <- c("est", paste0("sim", seq(nsim)))

  # ADD WHOLE PERIOD
  ansim <- abind(ansim, apply(ansim, c(1,3), sum, na.rm=T), along=2)
  dimnames(ansim)[[2]][length(levels(period))+1] <- "full"
  
  # CREATE DATASET
  andata <- as.data.table(ansim)
  names(andata) <- c("range", "period", "sim", "an")
  andata$city <- nm

  # ADD TOTAL DEATHS AND DAYS (ACCOUNTING FOR MISSING)
  ndata <- data.frame(period=levels(period),
    ndeath=tapply(y*!is.na(anday1), period, sum, na.rm=T),
    nday=tapply(!is.na(anday1), period, sum, na.rm=T))
  ndata <- rbind(ndata,
    cbind(data.frame(period="full"), t(colSums(ndata[,-1], na.rm=T))))
  andata <- merge(andata, ndata, sort=F, by="period")
  
  # RETURN
  andata[,c(5,1:4,6:7)]
}

# REMOVE PARALLELIZATION
stopCluster(cl)
#file.remove("temp/logattr.txt")

################################################################################
# RESULTS

# AGGREGATE BY COUNTRY
ancountry <- merge(ansim, cities[c("city","country")], by="city")
ancountry <- ancountry[, list(an=sum(an), ndeath=sum(ndeath), nday=sum(nday)),
  by=c("country","range","period","sim")]

# ADD TOTAL ACROSS MCC
antot <- ancountry[, list(an=sum(an), ndeath=sum(ndeath), nday=sum(nday)),
  by=range:sim]

# COMPUTE eCI
ancountry <- merge(ancountry[sim=="est"],
  ancountry[sim!="est", list(an_low=quantile(an,0.025,na.rm=T),
    an_high=quantile(an,0.975,na.rm=T)), by=c("country","range","period")])
antot <- merge(antot[sim=="est"],
  antot[sim!="est", list(an_low=quantile(an,0.025,na.rm=T),
    an_high=quantile(an,0.975,na.rm=T)), by=c("range","period")])

# CLEAN AND ORDER
ancountry$sim <- antot$sim <- NULL
#ancountry[, countryname:=factor(country, levels=unique(cities$country))]
setkey(ancountry, range, period)

################################################################################
# SAVE

# CLEAN
rm(ansim)

# SAVE THE WORKSPACE
#save.image("temp/attr.RData")
