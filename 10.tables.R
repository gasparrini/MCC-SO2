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

# EXTRACT STATS
tabcntrydescr <- data.frame(country = levels(fcntry),
  ncity = tapply(cities$city, fcntry, function(x) length(unique(x))),
  minperiod = tapply(sapply(dlist, function(x) min(year(x$date))), fcntry, min),
  maxperiod = tapply(sapply(dlist, function(x) max(year(x$date))), fcntry, max),
  nday = unique(ancountry[period=="full",c("countryname","nday")])$nday,
  ndeath = unique(ancountry[period=="full",c("countryname","ndeath")])$ndeath,
  meanso2 = tapply(sapply(dlist, function(x) mean(x$so2,na.rm=T)), fcntry, mean),
  per05so2 = tapply(sapply(dlist, function(x) quantile(x$so2,0.05,na.rm=T)), fcntry, mean),
  per95so2 = tapply(sapply(dlist, function(x) quantile(x$so2,0.95,na.rm=T)), fcntry, mean),
  ndaywho = tapply(sapply(dlist, function(x) 
    sum(na.omit(x$so2>40))/length(na.omit(x$so2))*100), fcntry, mean)
)

# ADD TOT
tabcntrydescr <- rbind(tabcntrydescr, data.frame(
  country = paste(length(levels(fcntry)), "MCC countries"),
  ncity = sum(tabcntrydescr$ncity),
  minperiod = min(tabcntrydescr$minperiod), maxperiod = max(tabcntrydescr$maxperiod),
  nday = sum(tabcntrydescr$nday), ndeath = sum(tabcntrydescr$ndeath),
  meanso2 = mean(sapply(dlist, function(x) mean(x$so2,na.rm=T))),
  per05so2 = mean(sapply(dlist, function(x) quantile(x$so2,0.05,na.rm=T))),
  per95so2 = mean(sapply(dlist, function(x) quantile(x$so2,0.95,na.rm=T))),
  ndaywho  = mean(sapply(dlist, function(x) 
    sum(na.omit(x$so2>40))/length(na.omit(x$so2))*100))
))

# FORMAT
tabcntrydescr <- cbind(tabcntrydescr[1:2], 
  period = paste(tabcntrydescr$minperiod, tabcntrydescr$maxperiod, sep="-"),
  nday = formatC(tabcntrydescr$nday, format="f", big.mark=",", digits=0),
  ndeath = formatC(tabcntrydescr$ndeath, format="f", big.mark=",", digits=0),
  so2 = paste0(
    formatC(tabcntrydescr$meanso2, format="f", big.mark=",", digits=1), " (",
    formatC(tabcntrydescr$per05so2, format="f", big.mark=",", digits=1), "-",
    formatC(tabcntrydescr$per95so2, format="f", big.mark=",", digits=1), ")"
  ),
  ndaywho = paste0(formatC(tabcntrydescr$ndaywho, format="f",
    big.mark=",", digits=1), "%")
)

# SAVE
write.csv(tabcntrydescr, row.names=F, file="tables/tabcntrydescr.csv")

################################################################################
# SO2 DISTRIBUTION

# SO2 AVERAGE BY COUNTRY AND PERIOD
perlist <- lapply(0:3, function(x) x*10+1980+0:9)
so2meanper <- sapply(dlist, function(x) {
  sapply(perlist, function(per) {
    mean(na.omit(x$so2[year(x$date)%in%per]))
  })
})
so2meanper <- rbind(apply(so2meanper, 1, tapply, fcntry, mean, na.rm=T),
  MCC=apply(so2meanper, 1, mean, na.rm=T)) |>
  formatC(digits=1, format="f") |>
  gsub("NaN", "-", x=_)

# DAYS ABOVE WHO LIMIT
so2aboveper <- sapply(dlist, function(x) {
  sapply(perlist, function(per) {
    so2 <- na.omit(x$so2[year(x$date)%in%per])
    sum(so2>40)/length(so2)*100
  })
})
so2aboveper <- rbind(apply(so2aboveper, 1, tapply, fcntry, mean, na.rm=T),
  MCC=apply(so2aboveper, 1, mean, na.rm=T)) |>
  formatC(digits=1, format="f") |>
  gsub("NaN", "-", x=_)
so2aboveper <- ifelse(so2aboveper=="-", "-", paste0(so2aboveper, "%"))

# NAMES
colnames(so2meanper) <- colnames(so2aboveper) <- sapply(perlist, function(x)
  paste(range(x), collapse="-"))

# SAVE
write.csv(cbind(so2meanper,so2aboveper), row.names=T,
  file="tables/tabso2period.csv")

################################################################################
# EXCESS MORTALITY BY PERIOD AND COUNTRY

# EXTRACT, ADD ALL MCC
tabcntaf <- ancountry
tabcntaftot <- antot
tabcntaftot$countryname <- "MCC"
tabcntaf <- rbind(tabcntaf, tabcntaftot)
rm(tabcntaftot)

# CREATE AF, FORMAT
tabcntaf[, paste0("af",1:3):=lapply(.SD, function(x) x/ndeath*100), 
  .SDcols=c("an","an_low","an_high")]
tabcntaf$af <- funformat(tabcntaf$af1, tabcntaf$af2, tabcntaf$af3, digits=2)

# RESHAPE, FILL THE MISSING VALUES, REORDER
tabcntaf <- dcast(tabcntaf, countryname+range~period, value.var="af")
for(i in seq(2, ncol(tabcntaf))) 
  tabcntaf[[i]] <- ifelse(is.na(tabcntaf[[i]]), "-", tabcntaf[[i]])

# SAVE
write.csv(tabcntaf[range=="tot", -2], row.names=F, file="tables/tabcntaftot.csv")
write.csv(tabcntaf[range!="tot", -2], row.names=F, file="tables/tabcntafbelow40.csv")

################################################################################
# TABLE OF SENSITIVITY ANALYSIS WITH CO-POLLUTANTS

# EXTRACT ESTIMATES
copollest <- t(sapply(copoll, function(x) as.numeric(x[-1])))

# TABLE WITH ESTIMATES
tabcopoll <- data.frame(Pollutant=seqpoll,
  N=sapply(copoll, function(x) nrow(x$cities)),
  without=funformat(exp(copollest[,1]), exp(copollest[,1]-qn*copollest[,2]),
    exp(copollest[,1]+qn*copollest[,2]), digits=4),
  with=funformat(exp(copollest[,3]), exp(copollest[,3]-qn*copollest[,4]),
    exp(copollest[,3]+qn*copollest[,4]), digits=4))

# TABLE WITH NUMBER OF CONTRIBUTING CITIES
taballpoll <- lapply(seqpoll, function(poll) {
  cit <- as.data.table(copoll[[poll]]$cities)
  cit <- cit[, list(length(city)), by="countryname"]
  names(cit)[2] <- poll
  cit
}) |> Reduce(function(y,z) merge(y,z,all=T), x=_)
taballpoll[is.na(taballpoll)] <- 0
taballpoll <- taballpoll[match(levels(fcntry), countryname),]
taballpoll <- rbind(taballpoll,
  cbind(data.frame(countryname="Tot"), t(apply(taballpoll[,-1], 2, sum))))

# SAVE
write.csv(tabcopoll, row.names=F, file="tables/tabcopoll.csv")
write.csv(taballpoll, row.names=F, file="tables/taballpoll.csv")

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
  