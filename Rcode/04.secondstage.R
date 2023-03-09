################################################################################
# R code for reproducing the analysis in:
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

# EXTRACT ESTIMATES FROM FIRST-STAGE MODELS
coefall <- unlist(lapply(stage1list, "[[", "coefall"))
vcovall <- unlist(lapply(stage1list, "[[", "vcovall"))

# PERFORM THE META-ANALYSIS
meta <- mixmeta(coefall, vcovall, random=~1|country/city, data=cities, 
  control=list(showiter=F))
#summary(meta)

# STORE BLUPS AT COUNTRY LEVEL
blupcountry <- blup(meta, se=T, pi=T, level=1)
