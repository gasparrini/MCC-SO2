################################################################################
# MCC-POLLUTION PROJECT: PRELIMINARY SO2 ANALYSIS 
################################################################################

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
attrlist <- foreach(data=dlist, i=seq(dlist), .packages=pack) %dopar% {
  
  # STORE ITERATION (1 EVERY 100)
  if(i%%50==0) cat("\n", "iter=",i, as.character(Sys.time()), "\n",
    file="temp/logattr.txt", append=T)

  # EXTRACT THE DATA
  data <- dlist[[i]]
  
  # DEFINE ONEBASIS FOR SO2
  oneso2 <- do.call(onebasis, c(list(x=data$so2), argvarso2))
  
  # FORWARD MOVING AVERAGE OF DEATHS
  y <- if(indnonext[i]) as.integer(data$nonext) else data[[out]]
  y <- rowMeans(as.matrix(Lag(y, -lagso2:0)))
  
  # INDICATOR FOR SO2 ABOVE 20
  ind20 <- data$so2>=20

  # COMPUTE THE EXCESS DEATHS USING THE CITY-SPECIFIC BLUPS
  anday <- (1-exp(-oneso2%*%blupcountry[i, "blup"]))*y
  an <- c(sum(anday, na.rm=T), sum(anday[ind20], na.rm=T))
  
  # SAMPLE THE COEF OF THE META-REGRESSION
  set.seed(13041975+i)
  coefsim <- mvrnorm(nsim, blupcountry[i, "blup"], (blupcountry[i, "se"])^2)
  
  # SIMULATED DISTRIBUTION OF EXCESS DEATHS
  ansim <- sapply(coefsim, function(b) {
    anday <- (1-exp(-oneso2%*%b))*y
    c(sum(anday, na.rm=T), sum(anday[ind20], na.rm=T))
  })
  ansim <- cbind(an, ansim)
  dimnames(ansim) <- list(c("tot","above20"), c("est", paste0("sim", seq(nsim))))

  # TOTAL DEATHS AND DAYS (ACCOUNTING FOR MISSING)
  ndeath <- sum(y[!is.na(anday)])
  nday <- sum(!is.na(anday))
  
  # RETURN
  list(ansim=ansim, ndeath=ndeath, nday=nday)
}
names(attrlist) <- cities$city

# REMOVE PARALLELIZATION
stopCluster(cl)
#file.remove("temp/logattr.txt")

################################################################################
# BY CITY

# EXTRACT ansim, BIND, AND PERMUTE ITS DIMENSIONS
ansim <- aperm(abind(lapply(attrlist, "[[", "ansim"), along=3), c(3,1,2))

# EXTRACT AN FROM ANSIM AND TRANSFORM BOTH IN DATA.TABLES
ansim <- as.data.table(ansim)
names(ansim) <- c("city", "range", "sim", "excdeath")
ancity <- ansim[sim=="est", c(1:2,4)]
ansim <- ansim[sim!="est",]

# COMPUTE AN AND eCI
ancity <- merge(ancity, ansim[, list("excdeath_low"=quantile(excdeath, 0.025),
  "excdeath_high"=quantile(excdeath, 0.975)), by=city:range],
  by=c("city", "range"))

# ADD TOTAL DEATHS AND DAYS (USE MERGE TO AVOID ISSUES WITH ORDERING)
ndeath <- sapply(attrlist, "[[", "ndeath")
nday <- sapply(attrlist, "[[", "nday")
ancity <- merge(ancity, data.frame(city=cities$city, ndeath=ndeath, nday=nday),
  by="city")

# REORDER
ancity <- ancity[cities$city,c(1:2,6:7,3:5)]

################################################################################
# BY COUNTRY

# AGGREGATE BY COUNTRY
ancountry <- merge(ancity[,1:5], cities[c("city","countryname")], by="city")
ancountry <- ancountry[, list(ndeath=sum(ndeath), nday=sum(nday), 
  ncity=length(unique(city)), excdeath=sum(excdeath)), 
  by=c("countryname", "range")]
temp <- merge(ansim, cities[c("city","countryname")], by="city")
temp <- temp[, list(excdeath=sum(excdeath)), by=c("countryname", "range", "sim")]

# COMPUTE eCI
ancountry <- merge(ancountry, temp[, list("excdeath_low"=quantile(excdeath, 0.025),
  "excdeath_high"=quantile(excdeath, 0.975)), by=countryname:range],
  by=c("countryname", "range"))
rm(temp)

# REORDER
ancountry <- ancountry[unique(cities$countryname),]

################################################################################
# OVERALL

# AGGREGATE ALL
anall <- ancity[, list(ndeath=sum(ndeath), nday=sum(nday),
  ncity=length(unique(city)), excdeath=sum(excdeath)), by=c("range")]
temp <- ansim[, list(excdeath=sum(excdeath)), by=c("range", "sim")]

# COMPUTE eCI
anall <- merge(anall, temp[, list("excdeath_low"=quantile(excdeath, 0.025),
  "excdeath_high"=quantile(excdeath, 0.975)), by=range], by=c("range"))

################################################################################
# SAVE

# CLEAN
rm(ansim)

# SAVE THE WORKSPACE
#save.image("temp/temp.RData")
