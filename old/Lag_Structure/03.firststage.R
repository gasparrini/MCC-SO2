################################################################################
# MCC-POLLUTION PROJECT: PRELIMINARY SO2 ANALYSIS 
################################################################################

################################################################################
# FIRST-STAGE ANALYSIS: RUN THE MODEL IN EACH CITY, REDUCE AND SAVE
################################################################################

################################################################################
# CREATE THE OBJECTS TO STORE THE RESULTS

# MATRIX TO STORE THE COEF AND VCOV FOR THE OVERALL CUMULATIVE EFFECT
est1 <- array(NA,dim=c(nrow(cities),4))
vcov1 <- vector("list",nrow(cities))

est2 <- array(NA,dim=c(nrow(cities),3))
vcov2 <- vector("list",nrow(cities))

# MATRIX TO STORE THE Q-AIC
qaic <- array(NA,dim=c(nrow(cities),2))

# VECTOR STORING INDICATOR OF CONVERGENCE
converg <- vector("numeric", length(dlist))

################################################################################
# RUN THE LOOP

# PRINT WARNINGS IMMEDIATELY (TO DETECT ISSUES)
options(warn=1)

# LOOP ACROSS CITIES
time <- proc.time()[3]
for(i in seq(length(dlist))) {

  # PRINT
  cat("\n",i,as.character(cities$city[i]))
  
  # EXTRACT THE DATA
  data <- dlist[[i]]
  
  # SUBSET WITH EXCLUDED INITIAL AND/OR FINAL MISSING PERIODS
  subrange <- range(seq(nrow(data))[!is.na(data$so2)&!is.na(data$tmean)])
  data <- data[subrange[1]:subrange[2],]
  
  # KEEP CITIES WITH AT LEAST ONE YEAR OF DATA
  if(nrow(data)<365) {
    converg[i] <- FALSE
    cat(" - non-convergence")
    next
  }
  
  # DEFINE THE OUTCOME
  data$y <- if(indnonext[i]) as.integer(data$nonext) else data[[out]]
  
  # DEFINE ONEBASIS FOR so2 (NB: EASIER TO EXTRACT EFFECTS FOR 1-UNIT INCREASE)
  # THIS SPECIFICATION MODEL THE so2 AS MOVING AVERAGE (MA) OVER LAGS 0-2; NOTE THAT
  # THE DEFAULT SPECIFICATION FOR arglag IS stata with 1 df. THIS GIVES THE MA FOR so2
  cbso2lags <- crossbasis(data$so2, lag=7, argvar=list("lin"), arglag=list(fun="strata",breaks= c(1,4)))
  
  # THIS SPECIFICATION MODEL THE so2 WITH UNCONTSRAINED EFFECT OVER LAGS 0-2
  cbso2lag <- crossbasis(data$so2, lag=7, argvar=list("lin"), arglag=list(fun="ns",knots= c(1,4)))
 
  # DEFINE CROSS-BASES FOR TEMPERATURE: MOVING AVERAGE OF 0-2
  tempknots <- quantile(data$tmean,c(10,75,90)/100,na.rm=T)
  cbtemp <- crossbasis(data$tmean, lag=3, 
    argvar=list(fun="bs",degree=2,knots=tempknots), arglag=list(fun="strata",breaks= c(1)))
  
  # RUN THE MODELS (NB: USE THE SAME OBS)
  m1 <- glm(y~cbso2lag+cbtemp+dow+bs(date,df=round(dfseas*length(date)/365.25)),
    data, family=quasipoisson)
  sub <- !seq(nrow(data)) %in% m1$na.action
  m2 <- glm(y~cbso2lags+cbtemp+dow+bs(date,df=round(dfseas*length(date)/365.25)),
    data, family=quasipoisson, subset=sub)
  
  # CHECK CONVERGENCE
  if(!(converg[i] <- m1$converged)) {
    cat(" - non-convergence")
    next
  }
  
  # FOR MODEL m2 WE HAVE TO SUM THE EFFECT OVER LAGS 0-2

  cp1 <- crossreduce(cbso2lag, m1, type="var",cen=0, value=10)
  cpbb <- crosspred(cbso2lag, m1, cen=0, at=10)
  est1[i,] <- cp1$coef
  vcov1[[i]]<-cp1$vcov
  
  cp2 <- crossreduce(cbso2lags, m2, type="var",cen=0, value=10)
  cpbb2 <- crosspred(cbso2lags, m2, cen=0, at=10)
  est2[i,] <- cp2$coef
  vcov2[[i]]<-cp2$vcov
  
    # HERE THE QAIC IS STORED FOR EACH MODEL
  qaic[i,] <- c(QAIC(m1),QAIC(m2))
}
proc.time()[3]-time

################################################################################

# SAVE THE WORKSPACE
#save.image("firststage.RData")

#
