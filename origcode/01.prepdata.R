################################################################################
# MCC-POLLUTION PROJECT: SO2 ANALYSIS 
################################################################################

################################################################################
# PREPARE THE DATA
################################################################################

# LOAD DATA
path <- "V:/VolumeQ/AGteam/MCCdata/air_pollution/MCC air pollution dataset/Processed"
#path <- "C:/Users/anton/OneDrive - London School of Hygiene and Tropical Medicine/Work/data/MCCdata"
load(paste(path, "MCCdata_Pollution_20210407.RData",sep="/"))

# TRANSFORM FACTORS IN CHARACTERS IN CITIES
cities[,sapply(cities, is.factor)] <- 
  lapply(cities[,sapply(cities, is.factor)], as.character)

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

# SELECT COUNTRY DATASETS (NB: KEEP BOTH FOR JAPAN)
subcountry <- c("aus8809","bra9711","can8615","chi9615","col9813","cze9415",
  "ecu1418","est9718","fnl9414","ger9315","irn0215","jap1115","jap7209",
  "kor9215","per0814","por8018","pue0916","rom9416","spa9014","sui9513",
  "tha9908","twn9414","uk9016Poll","usa7306")
dlist <- dlist[cities$country%in%subcountry]
cities <- cities[cities$country%in%subcountry,]

# REMOVE DUPLICATE CITIES FOR JAPAN AND RENAME COUNTRY
dupjap <- intersect(subset(cities, country=="jap1115")$cityname,
    subset(cities, country=="jap7209")$cityname)
sub <- !(cities$cityname%in%dupjap & cities$country=="jap1115")
cities <- cities[sub,]
dlist <- dlist[sub]
cities$country[cities$countryname=="Japan"] <- "jap"

# SEPARATE USA IN REGIONS
path <- "V:/VolumeQ/AGteam/MCCdata/original/USA211cities/Final_Dataset"
load(paste(path, "usa7306.RData",sep="/"))
usaregion <- data.frame(Region=1:9, regionname=paste("USA", c("Central", 
  "NECentral", "NWCentral", "NorthEast", "NorthWest", "South", "SouthEast",
  "SouthWest", "West"), sep="-"))
temp <- merge(citiesusa7306[c("city","Region")], usaregion)
temp <- merge(cities, temp)
temp$countryname <- temp$country <- temp$regionname
temp$Region <- NULL ; temp$regionname <- NULL
cities <- rbind(subset(cities, country!="usa7306"), temp)

# EXCLUDE TANGSHAN IN CHINA, WITH WEIRD DEATH COUNTS DISTRIBUTION
dlist[cities$city=="tngs.chi9615"] <- NULL
cities <- cities[-which(cities$city=="tngs.chi9615"),]

# EXCLUDE NANJING IN CHINA, WITH NO COUNTS FOR ALL/NONEXTERNAL MORTALITY
dlist[cities$city=="nnjn.chi9615"] <- NULL
cities <- cities[-which(cities$city=="nnjn.chi9615"),]

################################################################################
# REORDER

# REORDER FIRST BY COUNTRY AND CITY
ord <- order(cities$country, cities$city)
cities <- cities[ord,]

# FACTOR FOR REGION WITH PROPER ORDER
levreg <- c("North America", "Central America", "South America", "North Europe",
  "Central Europe", "South Europe", "Middle-East Asia", "East Asia", 
  "South-East Asia", "Australia")
cities$region <- factor(cities$region, levels=levreg)

# REORDER BY REGION 
cities <- cities[order(cities$region),]

# REORDER THE LIST OF DATASETS
dlist <- dlist[cities$city]

# CHANGE NAME OF PUERTO RICO
cities$countryname[cities$countryname=="Puertorico"] <- "Puerto Rico"

# RENAME CITIES
rownames(cities) <- NULL

################################################################################
# REMOVE OBJECTS AND SAVE

rm(list=setdiff(ls(), c("dlist","cities")))
