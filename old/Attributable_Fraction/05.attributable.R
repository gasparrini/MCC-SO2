
####  3. ESTIMATION OF THE ATTRIBUTABLE MORTALITY 

setwd("H:/Sera/MCC/MCC-SO2/Attributable_Fraction")

# LOAD THE FUNCTION FOR COMPUTING THE ATTRIBUTABLE RISK MEASURES
source("attrdl.R")

# CREATE THE VECTORS TO STORE THE TOTAL MORTALITY (ACCOUNTING FOR MISSING)
totdeathall <- rep(NA,nrow(cities))
names(totdeathall) <- cities$city

# CREATE THE MATRIX TO STORE THE ATTRIBUTABLE DEATHS USING THE CITYBLUPS
matsim <- matrix(NA,nrow(cities),2,dimnames=list(cities$city,
  c("b20", "Total")))

# NUMBER OF SIMULATION RUNS FOR COMPUTING EMPIRICAL CI
nsim <- 1000

# CREATE THE ARRAY TO STORE THE CI OF ATTRIBUTABLE DEATHS USING THE CITYBLUPS
arraysim <- array(NA,dim=c(nrow(cities),2,nsim),dimnames=list(cities$city,
  c("b20","Total")))

# CREATE A LIST OF BLUPS LENGTH CITY WITH COUNTRY BLUPS
mvmlblupcountry_city <- list()

countr_id <- as.numeric(cities$country)
for (i in 1:length(dlist)){
  mvmlblupcountry_city[[i]] <- blupcountry1[[countr_id[i]]]
}

################################################################################

maxlago <- 3
arglago <- list(fun="integer") # UNCONSTRAINT
argvaro <- list(fun="lin")

dlist<-dlist[indnomiss]

# CREATE INDICATOR FOR ONLY NON-EXTERNAL MORTALITY
indnonext <- sapply(dlist,function(x) !"all"%in%names(x))

# DEFINE THE OUTCOME
out <- "all"

# RUN THE LOOP
for(i in seq(dlist)){
  
  # PRINT
  cat(i,"")
  
  # EXTRACT THE DATA
  data <- dlist[[i]]
  
  #DEFINE CROSSBASIS OZONE
  cbo <- crossbasis(data$so2, lag=maxlago, 
   argvar=argvaro, arglag=arglago) 
  
  data$y <- if(indnonext[i]) as.integer(data$nonext) else data[[out]]

  # COMPUTE THE ATTRIBUTABLE DEATHS USING THE CITY-SPECIFIC
  # NB: THE REDUCED COEFFICIENTS ARE USED HERE
  matsim[i,"b20"] <- attrdl(data$so2,cbo,data$y,coef=mvmlblupcountry_city[[i]]$blup,
    vcov=mvmlblupcountry_city[[i]]$vcov,type="an",dir="forw",range=c(20,1000), cen=20)
  matsim[i,"Total"] <- attrdl(data$so2,cbo,data$y,coef=mvmlblupcountry_city[[i]]$blup,
    vcov=mvmlblupcountry_city[[i]]$vcov,type="an",dir="forw",range=c(0,1000), cen=0)
  
  # COMPUTE EMPIRICAL OCCURRENCES OF THE ATTRIBUTABLE DEATHS
  # USED TO DERIVE CONFIDENCE INTERVALS
  arraysim[i,"b20",] <- attrdl(data$so2,cbo,data$y,coef=mvmlblupcountry_city[[i]]$blup,
    vcov=mvmlblupcountry_city[[i]]$vcov,type="an",dir="forw",range=c(20,1000), cen=20,
    sim=T,nsim=nsim)
  arraysim[i,"Total",] <- attrdl(data$so2,cbo,data$y,coef=mvmlblupcountry_city[[i]]$blup,
    vcov=mvmlblupcountry_city[[i]]$vcov,type="an",dir="forw",range=c(0,1000), cen=0,
    sim=T,nsim=nsim)  
  
  totdeathall[i] <- sum(data$y,na.rm=T)
}

################################################################################
# ATTRIBUTABLE NUMBERS

# CITY-SPECIFIC
ancity <- matsim[,1]
ancitylow <- apply(arraysim[,1,],c(1),quantile,0.025)
ancityhigh <- apply(arraysim[,1,],c(1),quantile,0.975)
rownames(ancity) <- rownames(ancitylow) <- rownames(ancityhigh) <- cities$city

# TOTAL
antot <- colSums(matsim)[1]
antotlow <- quantile(apply(arraysim[,1,],c(2),sum),0.025)
antothigh <- quantile(apply(arraysim[,1,],c(2),sum),0.975)

################################################################################
# TOTAL MORTALITY

# BY COUNTRY
totdeathattrtot <- sum(totdeathall)

################################################################################
# ATTRIBUTABLE FRACTIONS

# CITY-SPECIFIC
afcity <- ancity/totdeathall*100
afcitylow <- ancitylow/totdeathall*100
afcityhigh <- ancityhigh/totdeathall*100
ancityse<-apply(arraysim/totdeathall*100,c(1,2),sd)

# TOTAL
aftot <- antot/totdeathattrtot*100
aftotlow <- antotlow/totdeathattrtot*100
aftothigh <- antothigh/totdeathattrtot*100

################################################################################
# I CALCULATE COUNTRY SPEFIFIC ATTRIBUTABLE DEATHS

ancountry<-aggregate(matsim[,1], by=list(cities$country), FUN=sum)
ancountry<-ancountry[order(ancountry$Group.1,ancountry$Group.2),]
names(ancountry)<-c("country","antot")

ndim<-nrow(ancountry)

ancountry.ci<-matrix(NA, nrow=ndim, ncol=4)

for (i in 1:ndim) {
  id<-(as.character(cities$country)==as.character(ancountry[i,1]))
  arraysim.c<-arraysim[id,,]
  
  if (length(dim(arraysim.c))==2) {
    ancountrylow <- quantile(arraysim.c[1,],0.025)
    ancountryhigh <- quantile(arraysim.c[1,],0.975)
  }
  
  if (length(dim(arraysim.c))==3) {
    ancountrylow <- quantile(apply(arraysim.c[,1,],c(2),sum),0.025)
    ancountryhigh <- quantile(apply(arraysim.c[,1,],c(2),sum),0.975)
  }

  ancountry.ci[i,1]<-i
  ancountry.ci[i,2]<-ancountry[i,2]
  ancountry.ci[i,3]<-ancountrylow
  ancountry.ci[i,4]<-ancountryhigh
  print(i)
}



################################################################################
# TOTAL MORTALITY

# BY COUNTRY

totdeath.country<-aggregate(totdeathall, by=list(cities$country), FUN=sum)
totdeath.country<-totdeath.country[order(totdeath.country$Group.1,totdeath.country$x),]
names(totdeath.country)<-c("country","deaths")


################################################################################
# ATTRIBUTABLE FRACTIONS

# BY COUNTRY
afcountry<-ancountry.ci[,2]/totdeath.country[,2]
afcountrylow<-ancountry.ci[,3]/totdeath.country[,2]
afcountryhigh<-ancountry.ci[,4]/totdeath.country[,2]

afcountry.ci<-cbind(1:ndim,afcountry,afcountrylow,afcountryhigh)


write.csv(afcountry.ci,file=paste("AFcountry",".csv",sep=""))
write.csv(ancountry.ci,file=paste("ANcountry",".csv",sep=""))

