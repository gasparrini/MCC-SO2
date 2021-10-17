################################################################################
# MCC-POLLUTION PROJECT: PRELIMINARY SO2 ANALYSIS 
################################################################################

################################################################################
# GRAPHS
################################################################################

################################################################################
# MAP

# WORLD MAP, GRID, PROJECTION
wld <- ne_countries(scale = 50, returnclass = "sf")
grid <- st_graticule(lon=seq(-180, 180, length.out=7),
  lat=seq(-90, 90, length.out=5), ndiscr=1000, crs=4326, margin=0.00001) 
prj <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# DEFINE ANNUAL SO2 AVERAGES AND CUT-OFFS
meanso2 <- round(sapply(dlist, function(x) mean(x$so2, na.rm=T)))
percut <- seq(0,100,length=10)
cutso2 <- quantile(meanso2, percut/100)

# MAP
pdf("figures/map.pdf", height=5, width=10)

xlab <- expression(paste("Annual average of ",SO[2]," (",mu,"gr/",m^3,")"))
cbind(cities, meanso2) %>%
    st_as_sf(coords=c("long", "lat"), crs=4326) %>%
  ggplot() +
  geom_sf(data=wld, fill=grey(0.88), colour = "white", size = .2) +
  geom_sf(aes(fill=cut(meanso2, cutso2, inc=T)), size=3, stroke=.3,
    shape=21, alpha=.7, colour=alpha("white", .5)) +
  geom_sf(data=grid, size=.3, linetype="dashed", colour="grey50", alpha=0.5) +
  scale_fill_brewer(palette="YlOrRd", name=xlab) +
  guides(fill=guide_colorsteps(barwidth=18, barheight=0.6, title.position="top",
    title.hjust=0.5)) + 
  coord_sf(crs=prj, ylim=c(-5500000, NA)) +
  theme_void() +   theme(legend.position="bottom")

dev.off()

################################################################################
# COUNTRY-SPECIFIC EFFECTS

# SEQUENCES AND RANGES
yn <- nrow(effcountry)
yseq <- c(rev(seq(yn-1)+1.5), 1)
xrange <- exp(range(c(effcountry[,c(1,3,4)]*10)))

# PLOT
pdf("figures/rrcountry.pdf", height=9, width=6.5)
par(mar=c(4,8.2,1,1)+0.1)

plot(yseq, type="n", xlim=xrange, yaxt="n", xlab="Relative risk", ylab="",
  mgp=c(2,1,0), cex.axis=0.8)
axis(2, at=yseq, las=1, tick=F, labels=labcountry, cex.axis=0.8)
grid(ny=0)
abline(v=1)
abline(v=exp(effcountry[yn,1]*10), lty=2)
arrows(exp(effcountry[,3]*10), yseq, exp(effcountry[,4]*10), yseq, 
  col="lightskyblue", code=3, angle=90, length=0.03, lwd=2)
points(exp(effcountry[-yn,1]*10), yseq[-yn], col=muted("lightskyblue"), pch=19,
  cex=1.2)
points(exp(effcountry[yn,1]*10), 1, col=muted("lightskyblue"), pch=23, bg=2,
  cex=1.4)

dev.off()

################################################################################
# NON-LINEAR EXPOSURE-RESPONSE

# COMPUTE THE POOLED EXPOSURE-RESPONSE
nlcp <- crosspred(do.call(onebasis, c(list(x=0:200), nlargvarso2)),
  coef=coef(nlmeta), vcov=vcov(nlmeta), cen=0, model.link="log")

# PLOT
pdf("figures/expresp.pdf", height=5, width=8)

par(mar=c(4,4,1,0.5))

xlab <- expression(paste(SO[2]," (",mu,"g/",m^3,")"))
plot(nlcp, ylim=c(0.98, 1.08), ylab="RR", xlab=xlab, col=muted("lightskyblue"), 
  ci.arg=list(col=alpha("lightskyblue", 0.2)), mgp=c(2.5,1,0), las=1,
  cex.axis=0.8)
points(linknots, nlcp$allRRfit[nlcp$predvar %in% linknots], pch=19,
  col=muted("lightskyblue"))
lines(0:200, exp(coef(meta)*0:200), lty=2, col=grey(0.5))
legend("topleft", c("Linear", "Non-linear"), lty=2:1, bty="n", cex=0.8,
  inset=0.05, col=c(grey(0.5), muted("lightskyblue")))

dev.off()

################################################################################
# LAG-RESPONSE

# COMPUTE THE POOLED LAG-RESPONSE
clcp <- crosspred(do.call(onebasis, c(list(x=seq(0, cllagso2, by=0.1)), clarglagso2)),
  coef=coef(clmeta), vcov=vcov(clmeta), model.link="log")

# PLOT
pdf("figures/lagresp.pdf", height=5, width=8)

par(mar=c(4,4,1,0.5))

plot(clcp, ylab="RR", xlab="Lag (days)", col=muted("lightskyblue"), mgp=c(2.5,1,0), las=1, cex.axis=0.8,
  ci.arg=list(col=alpha("lightskyblue", 0.2))) 

dev.off()
