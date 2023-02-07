################################################################################
# ADDITIONAL RESULTS
################################################################################

# SUMMARY STATS
tabcntrydescr[nrow(tabcntrydescr),]

# SUMMARY SO2 DISTRIBUTION ALONG DECADES
so2meanper[nrow(so2meanper),,drop=F]
so2aboveper[nrow(so2meanper),,drop=F]

# POOLED ESTIMATE OF THE ASSOCIATION
round(ci.exp(meta, ctr.mat=matrix(10)), 4)

# EXCESS MORTALITY FRACTION
subset(tabcntaf, range=="tot" & country=="MCC")
subset(tabcntaf, range!="tot" & country=="MCC")

# AVERAGE STUDY PERIOD
mean(sapply(dlist, function(x) diff(range(x$date))/365.25))

# HETEROGENEITY
summary(meta)
qtest(meta)

# PERCENTAGE OF CITIES EXPOSED TO LEVELS HIGHER THAN 150
sum(sapply(dlist, function(x) any(na.omit(x$so2)>150)))/length(dlist)

# TEST OF TEMPORAL TREND
summary(permeta)

# PERCENTAGE OF EXCESS DEATHS ATTRIBUTABLE TO LEVELS BELOW 40
subset(antot, range=="below40"&period=="full")$an / 
  subset(antot, range=="tot"&period=="full")$an *100
