################################################################################
# MCC-POLLUTION PROJECT: PRELIMINARY SO2 ANALYSIS 
################################################################################

# SET WORKING DIRECTORY
setwd("H:/Sera/MCC/MCC-SO2/Non_linearity")

# LOAD DATA
ind <- "V:/VolumeG/AGteam/MCCdata/air_pollution/MCC air pollution dataset"
load(paste(ind,"Processed/MCCdata_Pollution_20200110.RData",sep="/"))
infopoll <- read.csv(paste(ind,"/Metadata/Last/Metadatamccfinal20200110.csv",
  sep="/"))

subcities<-as.character(infopoll[,2])

dlist <- dlist[cities$city%in%subcities]
cities <- cities[cities$city%in%subcities,]

subcoutries<-as.character(unique(cities$country))
countries<-countries[countries$country%in%subcoutries,]

citiesso2<-infopoll$so2==TRUE

dlist <- dlist[citiesso2]
cities <- cities[citiesso2,]

subcoutries<-as.character(unique(cities$country))
countries<-countries[countries$country%in%subcoutries,]

table(cities$country)

# SELECT COUNTRIES WITH POLLUTION DATA AND CITIES WITH NO2
subcountry <- cities$country %in% sort(c("aus8809","bra9711","can8615",
  "chi9615","col9813","cze9415","est9715","fnl9414","ger9315",
  "jap1115","kor9215","por8018","rom9416","spa9014","sui9513",
  "tha9908","twn9414","uk9016Poll", "usa7306"))

# SELECT THE SUBSET
dlist <- dlist[subcountry]
cities <- cities[subcountry,]

# EXCLUDE TANGSHAN IN CHINA, WITH WEIRD DEATH COUNTS DISTRIBUTION
dlist[cities$city=="tngs.chi9615"] <- NULL
cities <- cities[-which(cities$city=="tngs.chi9615"),]

# EXCLUDE NANJING IN CHINA, WITH NO COUNTS FOR ALL/NONEXTERNAL MORTALITY
dlist[cities$city=="nnjn.chi9615"] <- NULL
cities <- cities[-which(cities$city=="nnjn.chi9615"),]

# REMOVE EMPTY LEVELS OF FACTORS
cities[1:4] <- as.data.frame(as.matrix(cities[1:4]))

# REORDER
ord <- order(cities$country,cities$city)
dlist <- dlist[ord]
cities <- cities[ord,]

table(cities$countryname)

################################################################################
# REMOVE OBJECTS

rm(list=ls()[!ls()%in%c("dlist","cities")])

#
