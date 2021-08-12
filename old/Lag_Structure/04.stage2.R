################################################################################
# MCC-POLLUTION PROJECT: PRELIMINARY SO2 ANALYSIS 
################################################################################

################################################################################
# META-ANALYSIS
################################################################################

indnomiss<-!is.na(est1[,1])
sum(indnomiss)

# FIRST MODEL NO2 AS 0-2 MOVING AVERAGE
y1 <- est1[indnomiss,]
S1 <- vcov1[indnomiss]
cities<-cities[indnomiss,]

meta1 <- mixmeta(y1, S1, random=~1|country/city, data=cities, 
  control=list(showiter=T))

summary(meta1)

# PREDICTION
so2pred <- seq(0,7)
so2lin <- onebasis(so2pred,fun="ns",knots=c(1,4),intercept=TRUE,Boundary.knots=c(0,7))

lcp1.all <- crosspred(so2lin,coef=coef(meta1),vcov=vcov(meta1),
  model.link="log",by=0.1)

plot(lcp1.all)

# FIRST MODEL NO2 AS 0-2 MOVING AVERAGE
y2 <- est2[indnomiss,]
S2 <- vcov2[indnomiss]

meta2 <- mixmeta(y2, S2, random=~1|country/city, data=cities, 
  control=list(showiter=T))

summary(meta2)

# PREDICTION
so2pred <- seq(0,7)
so2lin <- onebasis(so2pred,fun="strata",breaks=c(1,4),intercept=TRUE)

lcp2.all <- crosspred(so2lin,coef=coef(meta2),vcov=vcov(meta2),
  model.link="log",by=1)

plot(lcp2.all, ci="bars")

colSums(qaic,na.rm=TRUE)

