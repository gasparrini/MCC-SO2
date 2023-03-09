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

# LOAD DATA
data <- read.csv("data/simdata.csv")
data$date <- as.Date(data$date)

# CREATE THE METADATA AND A LIST OF CITY-SPECIFIC DATASETS
cities <- unique(data[c("country","city")])
dlist <- split(data, data$city)[cities$city]

# SELECT THE COUNTRY-SPECIFIC DATASETS WITH SO2 DATA (AT LEAST 1 YEAR)
sub <- sapply(dlist, function(x) 
  "so2" %in% names(x) && length(na.omit(x$so2))>365)
cities <- cities[sub,]
dlist <- dlist[sub]

# REMOVE INITIAL/FINAL PERIODS WITH NO S02 MEASUREMENTS
dlist <- lapply(dlist, function(x)
  x[min(which(!is.na(x$so2))):max(which(!is.na(x$so2))),])

# REMOVE PRE-1980
dlist <-  lapply(dlist, function(x) subset(x, year(date)>=1980))

# CHECK NUMBER OF SO2-DAYS FOR EACH COUNTRY
cities$n <- sapply(dlist, function(x) length(na.omit(x$so2)))
unique(cities$country)
tapply(cities$n,cities$country,mean)
tapply(cities$n,cities$country,sum)

################################################################################
# REORDER

# REORDER FIRST BY COUNTRY AND CITY
ord <- order(cities$country, cities$city)
cities <- cities[ord,]

# REORDER THE LIST OF DATASETS
dlist <- dlist[cities$city]

# RENAME CITIES
rownames(cities) <- NULL

################################################################################
# REMOVE OBJECTS AND SAVE

rm(list=setdiff(ls(), c("dlist","cities")))

