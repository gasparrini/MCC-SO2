################################################################################
# MCC-POLLUTION PROJECT: SO2 ANALYSIS 
################################################################################

################################################################################
# TABLES
################################################################################

# FUNCTION TO FORMAT VARIABLES WITH INTERVALS
funformat <- function(est, lci=NULL, hci=NULL, digits=0) {
  paste0(formatC(est, digits=digits, format="f", big.mark=","), " (",
    formatC(lci, digits=digits, format="f", big.mark=",") , " to ",
    formatC(hci, digits=digits, format="f", big.mark=","), ")")
}

################################################################################
# DESCRIPTIVE STATS

# COUNTRY FACTOR
fcntry <- factor(cities$countryname, levels=unique(cities$countryname))

# SO2 AVERAGE BY CITY
avgso2 <- sapply(dlist, function(x) mean(x$so2, na.rm=T))

# DAYS ABOVE WHO LIMIT
so2day <- sapply(dlist, function(x) length(na.omit(x$so2)))
so2dayhigh <- sapply(dlist, function(x) sum(na.omit(x$so2)>40))

# EXTRACT STATS
tabcntrydescr <- data.frame(country = levels(fcntry),
  ncity = tapply(cities$city, fcntry, function(x) length(unique(x))),
  minperiod = tapply(sapply(dlist, function(x) min(year(x$date))), fcntry, min),
  maxperiod = tapply(sapply(dlist, function(x) max(year(x$date))), fcntry, max),
  nday = ancountry[ancountry$range=="tot",]$nday,
  ndeath = ancountry[ancountry$range=="tot",]$ndeath,
  medso2 = tapply(avgso2, fcntry, median),
  minso2 = tapply(avgso2, fcntry, min),
  maxso2 = tapply(avgso2, fcntry, max),
  ndaywho = tapply(so2dayhigh, fcntry, sum)/tapply(so2day, fcntry, sum)*100
)

# ADD TOT
tabcntrydescr <- rbind(tabcntrydescr, data.frame(
  country = paste(length(levels(fcntry)), "MCC countries"),
  ncity = sum(tabcntrydescr$ncity),
  minperiod = min(tabcntrydescr$minperiod), maxperiod = max(tabcntrydescr$maxperiod),
  nday = sum(tabcntrydescr$nday), ndeath = sum(tabcntrydescr$ndeath), 
  medso2 = median(avgso2), minso2 = min(avgso2), maxso2 = max(avgso2),
  ndaywho = sum(so2dayhigh)/sum(so2day)*100
))

# FORMAT
tabcntrydescr <- cbind(tabcntrydescr[1:2], 
  period = paste(tabcntrydescr$minperiod, tabcntrydescr$maxperiod, sep="-"),
  nday = formatC(tabcntrydescr$nday, format="f", big.mark=",", digits=0),
  ndeath = formatC(tabcntrydescr$ndeath, format="f", big.mark=",", digits=0),
  so2 = paste0(formatC(tabcntrydescr$medso2, format="f", big.mark=",", digits=1),
    " (", formatC(tabcntrydescr$minso2, format="f", big.mark=",", digits=1), "-",
    formatC(tabcntrydescr$maxso2, format="f", big.mark=",", digits=1), ")"),
  ndaywho = paste0(formatC(tabcntrydescr$ndaywho, format="f", digits=1), "%")
)

# SAVE
write.csv(tabcntrydescr, row.names=F, file="tables/tabcntrydescr.csv")

################################################################################
# EXCESS MORTALITY BY CITY

# FORMAT EXCESS DEATHS AND FRACTION
af <- funformat(ancity$excdeath/ancity$ndeath*100, 
  ancity$excdeath_low/ancity$ndeath*100,
  ancity$excdeath_high/ancity$ndeath*100, digits=2)
an <- funformat(ancity$excdeath/ancity$nday*365.25,
  ancity$excdeath_low/ancity$nday*365.25,
  ancity$excdeath_high/ancity$nday*365.25)
afsum <- funformat(anall$excdeath/anall$ndeath*100,
  anall$excdeath_low/anall$ndeath*100,
  anall$excdeath_high/anall$ndeath*100, digits=2)
ansum <- funformat(anall$excdeath/anall$nday*365.25*anall$ncity,
  anall$excdeath_low/anall$nday*365.25*anall$ncity,
  anall$excdeath_high/anall$nday*365.25*anall$ncity)

# PUT TOGETHER WITH SO2 LEVELS
tabcityexc <- data.frame(city=cities$cityname, country=cities$countryname,
  so2 = formatC(avgso2, format="f", digits=1),
  ndaywho = paste0(formatC((so2dayhigh/so2day*100), format="f", digits=1), "%"),
  aftot = af[ancity$range=="tot"], 
  antot = an[ancity$range=="tot"],
  af40 = af[ancity$range=="above40"], 
  an40 = an[ancity$range=="above40"]
)

# SAVE
write.csv(tabcityexc, row.names=F, file="tables/tabcityexc.csv")

################################################################################
# EXCESS MORTALITY BY MAJOR CITIES AND TOTAL

# SELECT CITIES (HIGHEST DAILY DEATH COUNTS)
daydeath <- with(subset(ancity, range=="tot"), ndeath/nday)
indmain <- cities$city %in% tapply(seq(daydeath), cities$country, function(x) 
  cities$city[x[which.max(daydeath[x])]])

# LABELS
citycntrylab <- paste0(cities$cityname, " (", cities$countryname, ")")

# SELECT CITIES
tabmainexc <- cbind(city=citycntrylab[indmain], tabcityexc[indmain, -(1:2)])



# ADD TOTAL
tabmainexc <- rbind(tabmainexc, data.frame(city=paste(nrow(cities), "MCC cities"),
  so2=formatC(mean(avgso2), format="f", digits=1),
  ndaywho=paste0(formatC(sum(so2dayhigh)/sum(so2day)*100, format="f",
    digits=1), "%"),
  aftot=afsum[2], antot=ansum[2], af40=afsum[1], an40=ansum[1]))

# SAVE
write.csv(tabmainexc, row.names=F, file="tables/tabmainexc.csv")

################################################################################
# TABLE OF SENSITIVITY ANALYSIS WITH CO-POLLUTANTS

tabcopoll <- data.frame(Pollutant=seqpoll,
  N=copoll[,1],
  without=funformat(exp(copoll[,2]), exp(copoll[,2]-qn*copoll[,3]),
    exp(copoll[,2]+qn*copoll[,3]), digits=4),
  with=funformat(exp(copoll[,4]), exp(copoll[,4]-qn*copoll[,5]),
    exp(copoll[,4]+qn*copoll[,5]), digits=4))

# SAVE
write.csv(tabcopoll, row.names=F, file="tables/tabcopoll.csv")

################################################################################
# RR BY COUNTRY AND OVERALL

# COUNTRY-SPECIFIC ESTIMATES AND LABELS 
effcountry <- rbind(unique(blupcountry), unique(predict(meta, se=T, ci=T)))
labcountry <- c(unique(cities$countryname), "Pooled")

# FORMAT
tabrrcountry <- cbind(labcountry, funformat(exp(effcountry[,1]*10),
  exp(effcountry[,3]*10), exp(effcountry[,4]*10), digits=4))
colnames(tabrrcountry) <- c("", "RR (95%CI")

# SAVE
write.csv(tabrrcountry, row.names=F, file="tables/tabrrcountry.csv")
  