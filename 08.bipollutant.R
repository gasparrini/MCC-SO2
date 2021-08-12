################################################################################
# MCC-POLLUTION PROJECT: PRELIMINARY SO2 ANALYSIS 
################################################################################

################################################################################
# BI-POLLUTANT MODELS
################################################################################

# SELECT OTHER POLLUTANTS
seqpoll <- c("pm10","pm25","o38h","no2","co")

# PERFORM THE MODELS WIHT AND WITHOUT CO-POLLUTANTS IS SUBSETS OF CITIES  
copoll <- do.call(rbind, lapply(seqpoll, function(poll) {
  
  # PRINT
  cat(poll,"")
  
  # SELECT THE CITIES WITH BOTH POLLUTANTS
  sub <- sapply(dlist, function(x) 
    poll %in% names(x) && nrow(na.omit(cbind(x$so2, runMean(x[,poll], 0:1))))>365
  )
  cities <- cities[sub,]
  dlist <- dlist[sub]
  indnonext <- indnonext[sub]
  
  # RESET PARAMETERS IN THIS INTERNAL ENVIRONMENT
  source("02.param.R", local=T)
  
  # PERFOR FIRST AND SECOND-STAGE WITHOUT CO-POLLUTANT
  source("03.firststage.R", local=T)
  source("04.secondstage.R", local=T)
  
  # STORE
  coefwithout <- coef(meta)*10
  sewithout <- sqrt(vcov(meta))*10
  
  # DEFINE VARIABLE FOR OTHER POLLUTANT
  dlist <- lapply(dlist, function(x) cbind(x, poll=x[[poll]]))
  
  # RE-DEFINE THE FIRST-STAGE MODEL FORMULA
  fmod <- y ~ cbso2 + cbtmean + dow + spltime + runMean(poll, 0:1)
  
  # PERFOR FIRST AND SECOND-STAGE WITH CO-POLLUTANT
  source("03.firststage.R", local=T)
  source("04.secondstage.R", local=T)

  # STORE
  coefwith <- coef(meta)*10
  sewith <- sqrt(vcov(meta))*10
  
  # RETURN
  data.frame(n=nrow(cities), coefwithout=coefwithout, sewithout=sewithout,
    coefwith=coefwith, sewith=sewith)
}))

# SAVE THE WORKSPACE
#save.image("temp/temp.RData")
