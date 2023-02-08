################################################################################
# MCC-POLLUTION PROJECT: SO2 ANALYSIS 
################################################################################

################################################################################
# BI-POLLUTANT MODELS
################################################################################

# SELECT OTHER POLLUTANTS
seqpoll <- c("pm10","pm25","o38h","no2","co")

# PERFORM THE MODELS WIHT AND WITHOUT CO-POLLUTANTS IN SUBSETS OF CITIES  
copoll <- lapply(seqpoll, function(poll) {
  
  # PRINT
  cat(poll,"")
  
  # SELECT THE CITIES WITH BOTH POLLUTANTS
  sub <- sapply(dlist, function(x) 
    poll %in% names(x) && nrow(na.omit(cbind(x$so2, runMean(x[,poll], 0:3))))>365
  )
  cities <- cities[sub,]
  dlist <- dlist[sub]
  indnonext <- indnonext[sub]
  
  # RESET PARAMETERS IN THIS INTERNAL ENVIRONMENT
  # NB: ADD local=T TO SOURCE IN THE LOCAL ENVIRONMENT OF THE PARALLELISATION
  source("02.param.R", local=T)
  
  # PERFOR FIRST AND SECOND-STAGE WITHOUT CO-POLLUTANT
  source("03.firststage.R", local=T)
  source("04.secondstage.R", local=T)
  
  # STORE
  coefwithout <- coef(meta)*10
  sewithout <- as.numeric(sqrt(vcov(meta))*10)
  
  # DEFINE VARIABLE FOR OTHER POLLUTANT
  dlist <- lapply(dlist, function(x) cbind(x, poll=x[[poll]]))
  
  # RE-DEFINE THE FIRST-STAGE MODEL FORMULA
  fmod <- y ~ runMean(so2,0:3) + cbtmean + dow + spltime + runMean(poll, 0:3)
  
  # PERFOR FIRST AND SECOND-STAGE WITH CO-POLLUTANT
  source("03.firststage.R", local=T)
  source("04.secondstage.R", local=T)

  # STORE
  coefwith <- coef(meta)*10
  sewith <- as.numeric(sqrt(vcov(meta))*10)
  
  # RETURN
  list(cities=cities[,1:4], coefwithout=coefwithout, sewithout=sewithout,
    coefwith=coefwith, sewith=sewith)
})

# NAMES
names(copoll) <- seqpoll

# SAVE THE WORKSPACE
#save.image("temp/bipollutant.RData")
