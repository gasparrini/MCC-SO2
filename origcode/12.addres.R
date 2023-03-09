################################################################################
# Original R code for the analysis in:
#
#  O'Brien E, et al. Short-Term association between sulfur dioxide and 
#    mortality: a multicountry analysis in 399 cities. Environmental Health
#    Perspectives. 2023;131(3):37002.
#  https://doi.org/10.1289/ehp11112
#
# * an updated version of this code, compatible with future versions of the
#   software, is available at:
#   https://github.com/gasparrini/MCC-SO2
################################################################################

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
subset(tabcntaf, range=="tot" & countryname=="MCC")
subset(tabcntaf, range!="tot" & countryname=="MCC")

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
