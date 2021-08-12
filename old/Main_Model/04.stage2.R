################################################################################
# MCC-POLLUTION PROJECT: PRELIMINARY SO2 ANALYSIS 
################################################################################

################################################################################
# META-ANALYSIS
################################################################################

indnomiss<-!is.na(est1[,1])
sum(indnomiss)

# FIRST MODEL NO2 AS 0-2 MOVING AVERAGE
y1 <- est1[indnomiss,1]
S1 <- est1[indnomiss,2]
cities<-cities[indnomiss,]

meta1 <- mixmeta(y1, S1, random=~1|country/city, data=cities, 
  control=list(showiter=T))

summary(meta1)

# THESE COMMANDS DERIVE THE RER OF THE POOLED EFFECT
beta1<-meta1$coefficients
se1<-sqrt(meta1$vcov)
(betaoverall1<-(exp(beta1)-1)*100)
(betaoverallinf1<-(exp((beta1-1.96*se1))-1)*100)
(betaoverallsup1<-(exp((beta1+1.96*se1))-1)*100)

# STORE BLUPS AT LOCATION AND COUNTRY LEVEL
blupcity1 <- blup(meta1,vcov=T,format="list")
blupcountry1 <- unique(blup(meta1,vcov=T,level=1,format="list"))

countries<-unique(cities$country)
resultsbycountry1<-matrix(NA, nrow=length(countries), ncol=4)
resultsbycountry1<-as.data.frame(resultsbycountry1)

for (i in 1:length(countries)) {
  beta1<-blupcountry1[[i]]$blup
  se1<-sqrt(blupcountry1[[i]]$vcov)
  betaoverall1<-(exp(beta1)-1)*100
  betaoverallinf1<-(exp((beta1-1.96*se1))-1)*100
  betaoverallsup1<-(exp((beta1+1.96*se1))-1)*100
  resultsbycountry1[i,1]<-as.character(countries[i])
  resultsbycountry1[i,2]<-betaoverall1
  resultsbycountry1[i,3]<-betaoverallinf1
  resultsbycountry1[i,4]<-betaoverallsup1
}

colnames(resultsbycountry1)<-c("Country","ERR","ERRInf","ERRSup")

print(resultsbycountry1)

# PREDICTION
so2pred <- seq(0,100)
so2lin <- onebasis(so2pred,fun="lin")

lcp1.all <- crosspred(so2lin,coef=coef(meta1),vcov=vcov(meta1)
  ,model.link="log",by=0.1,cen=50)

plot(lcp1.all)

# SECOND MODEL so2 WITH UNCONSTRAINED LAGGED EFFECTS AT LAGS 0-2
y2 <- est2[indnomiss,1]
S2 <- est2[indnomiss,2]

meta2 <- mixmeta(y2, S2, random=~1|country/city, data=cities, 
  control=list(showiter=T))

summary(meta2)

# THESE COMMANDS DERIVE THE RER OF THE POOLED EFFECT
beta2<-meta2$coefficients
se2<-sqrt(meta2$vcov)
(betaoverall2<-(exp(beta2)-1)*100)
(betaoverallinf2<-(exp((beta2-1.96*se2))-1)*100)
(betaoverallsup2<-(exp((beta2+1.96*se2))-1)*100)

# STORE BLUPS AT LOCATION AND COUNTRY LEVEL
blupcity2 <- blup(meta2,vcov=T,format="list")
blupcountry2 <- unique(blup(meta2,vcov=T,level=1,format="list"))

resultsbycountry2<-matrix(NA, nrow=length(countries), ncol=4)
resultsbycountry2<-as.data.frame(resultsbycountry2)

for (i in 1:length(countries)) {
  beta2<-blupcountry2[[i]]$blup
  se2<-sqrt(blupcountry2[[i]]$vcov)
  betaoverall2<-(exp(beta2)-1)*100
  betaoverallinf2<-(exp((beta2-1.96*se2))-1)*100
  betaoverallsup2<-(exp((beta2+1.96*se2))-1)*100
  resultsbycountry2[i,1]<-as.character(countries[i])
  resultsbycountry2[i,2]<-betaoverall2
  resultsbycountry2[i,3]<-betaoverallinf2
  resultsbycountry2[i,4]<-betaoverallsup2
}

colnames(resultsbycountry2)<-c("Country","ERR","ERRInf","ERRSup")

print(resultsbycountry2)

# PREDICTION
lcp2.all <- crosspred(so2lin,coef=coef(meta2),vcov=vcov(meta2)
  ,model.link="log",by=0.1,cen=50)

plot(lcp2.all)

# HERE WE COMPARE MODEL 1 AND 2 USING THE SUM OF QAIC STORED FOR EACH LOCATIONS
# THERE IS A TENDENCY OF MODEL 2 TO HAVE A LOWER QAIC

colSums(qaic,na.rm=TRUE)

