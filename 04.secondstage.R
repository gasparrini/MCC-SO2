################################################################################
# SECOND-STAGE META-ANALYSIS
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
