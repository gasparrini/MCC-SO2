library(maps)
library(mapproj)
library(mapdata)
library(maptools)

avgso2<- sapply(dlist,function(x) mean(x$so2,na.rm=T))

countrymeanso2 <-aggregate(avgso2, by=list(cities$countryname),
  FUN=mean, na.rm=TRUE)
names(countrymeanso2)<-c("Country","so2")
print(countrymeanso2)

summary(avgso2)
labels <- paste0(c("0-2","2-6","6-12","12-20","20-60")," microg/m3")
avgso2cat <- cut(avgso2,c(0,2,6,12,20,60),labels=labels)
funcol <- colorRampPalette(c("blue","skyblue","yellow","red"))
col <- funcol(length(labels))[avgso2cat]
size <- rep(1.8,nrow(cities))

pdf("mapso2.pdf",width=9,height=3.7)

map("worldHires",xlim=c(-170,180),ylim=c(-60,80),mar=c(0,0,0,0),
    col=grey(0.5),myborder=0)
symbols(cities$long,cities$lat,circles=size,inches=F,bg=col,add=T)

map.scale(2,-40,ratio=F,cex=0.6,relwidth=0.1)

legend(-125,-20,labels,xjust=0.5,yjust=0.5,pt.cex=1,
       pch=21,pt.bg=funcol(length(labels)),bty="n",cex=0.5,
       title=expression(paste("      so2 (microg/m3)",sep="")))

dev.off()

#



