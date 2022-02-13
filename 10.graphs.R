################################################################################
# MCC-POLLUTION PROJECT: SO2 ANALYSIS 
################################################################################

################################################################################
# GRAPHS
################################################################################

################################################################################
# MAP OF SO2 AVERAGE CONCENTRATIONS

# WORLD MAP, GRID, PROJECTION
wld <- ne_countries(scale = 50, returnclass = "sf") |>
  subset(subregion != "Antarctica")
grid <- st_graticule() 
prj <- "ESRI:54030"

# DEFINE ANNUAL SO2 AVERAGES AND CUT-OFFS
meanso2 <- round(sapply(dlist, function(x) mean(x$so2, na.rm=T)))
percut <- seq(0,100,length=10)
cutso2 <- quantile(meanso2, percut/100)

# MAP
xlab <- expression(paste("Annual average of ",SO[2]," (",mu,"gr/",m^3,")"))
cbind(cities, meanso2) |>
    st_as_sf(coords=c("long", "lat"), crs=4326) |>
  ggplot() +
  geom_sf(data=wld, fill=grey(0.88), colour = "white", size = .2) +
  geom_sf(aes(fill=cut(meanso2, cutso2, inc=T)), size=3, stroke=.3,
    shape=21, alpha=.7, colour=alpha("white", .5)) +
  geom_sf(data=grid, size=.1, linetype="dashed", colour="grey50", alpha=0.4) +
  scale_fill_brewer(palette="Blues", name=xlab, labels=round) +
  guides(fill=guide_colorsteps(barwidth=18, barheight=0.45, title.position="top",
    title.hjust=0.5)) + 
  coord_sf(xlim=c(-170,180), ylim=c(-55,80), crs=prj, default_crs=4326) +
  theme_void() +   theme(legend.position="bottom")

dev.print(pdf, "figures/mapso2.pdf", height=5, width=10)

################################################################################
# MAP OF EXCESS MORTALITY FRACTION

# DEFINE EXCESS MORTALITY FRACTION AND CUT-OFFS
af <- (ancity$excdeath/ancity$ndeath*100)[ancity$range=="tot"]
cutaf <- round(quantile(af, percut/100), 2)

# MAP
cbind(cities, af) |>
    st_as_sf(coords=c("long", "lat"), crs=4326) |>
  ggplot() +
  geom_sf(data=wld, fill=grey(0.88), colour = "white", size = .2) +
  geom_sf(aes(fill=cut(af, cutaf, inc=T)), size=3, stroke=.3,
    shape=21, alpha=.7, colour=alpha("white", .5)) +
  geom_sf(data=grid, size=.1, linetype="dashed", colour="grey50", alpha=0.4) +
  scale_fill_brewer(palette="YlOrBr", name="Excess mortality fraction (%)") +
  guides(fill=guide_colorsteps(barwidth=18, barheight=0.45, title.position="top",
    title.hjust=0.5)) + 
  coord_sf(xlim=c(-170,180), ylim=c(-55,80), crs=prj, default_crs=4326) +
  theme_void() +   theme(legend.position="bottom")

dev.print(pdf, "figures/mapaf.pdf", height=5, width=10)

################################################################################
# COUNTRY-SPECIFIC EFFECTS

# SEQUENCES AND RANGES
yn <- nrow(effcountry)
yseq <- c(rev(seq(yn-1)+1.5), 1)
xrange <- exp(range(c(effcountry[,c(1,3,4)]*10)))

# PLOT
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

dev.print(pdf, "figures/rrcountry.pdf", height=9, width=6.5)


# GGPLOT VERSION
as.matrix(unique(cities[c("countryname","region")])) |>
  rbind(c("Pooled","")) |>
  cbind(as.data.frame(exp(effcountry[,c(1,3,4)]*10))) |>
  mutate(countryname=factor(countryname, levels=rev(unique(countryname))),
    region=factor(region, levels=unique(region)),
    shape=rep(c(19,18),c(yn-1,1))) |>
  ggplot(aes(blup, countryname)) +
  geom_vline(xintercept=1, size=.4) +
  geom_vline(xintercept=exp(effcountry[yn,1]*10), size=.4, linetype="dotted") +
  geom_errorbar(aes(xmin=pi.lb, xmax=pi.ub), colour="lightskyblue", width=0.4,
    show.legend=FALSE) +
  geom_point(size=2, aes(shape=shape), colour=muted("lightskyblue"),
    show.legend=FALSE) + scale_shape_identity() +
  #scale_y_discrete(labels = function(x) str_remove(x, "Pooled")) +
  facet_grid(region~., space = "free_y", scales = "free_y") +
  labs(y = "", x = "RR") +
  theme_bw() +
  theme(strip.text.y=element_text(angle=0), panel.grid.minor=element_blank(),
    panel.grid.major.y=element_blank())

ggsave("figures/rrcountry2.pdf", height = 8, width = 6.76, unit = "in")

################################################################################
# EXPOSURE-RESPONSE AND LAG-RESPONSE

# COMPUTE THE POOLED EXPOSURE-RESPONSE
nlcp <- crosspred(do.call(onebasis, c(list(x=0:200), nlargvarso2)),
  coef=coef(nlmeta), vcov=vcov(nlmeta), cen=0, model.link="log")

# COMPUTE THE POOLED LAG-RESPONSE
clcp <- crosspred(do.call(onebasis, c(list(x=seq(0, cllagso2, by=0.1)), clarglagso2)),
  coef=coef(clmeta), vcov=vcov(clmeta), model.link="log")

# COMPUTE THE PROPORTION OF STUDIES BY EXPOSURE RANGE
xseq <- 0:199+0.5
proprange <- rowSums(sapply(dlist, function(x) 
  xseq > min(x$so2, na.rm=T) & xseq < max(x$so2, na.rm=T))) / length(dlist)*100

# DEFINE THE COLOURS
fprop <- cut(proprange, 0:100)
col <- c("white", "aliceblue", "lightskyblue", "blue", "blue4")
seqcol <- colorRampPalette(col)(101)

# PLOT
layout(1:2, heights=c(1,0.7))
par(mar=c(4,4,1,0.5))

xlab <- expression(paste(SO[2]," (",mu,"g/",m^3,")"))
plot(nlcp, ylim=c(0.965, 1.08), ylab="RR", xlab=xlab, col=muted("lightskyblue"), 
  ci.arg=list(col=alpha("lightskyblue", 0.2)), mgp=c(2.5,1,0), las=1,
  cex.axis=0.8)
# points(linknots, nlcp$allRRfit[nlcp$predvar %in% linknots], pch=19,
#   col=muted("lightskyblue"))
lines(0:200, exp(coef(meta)*0:200), lty=2, col=grey(0.5))
legend("topleft", c("Linear", "Non-linear"), lty=2:1, bty="n", cex=0.8,
  inset=0.05, col=c(grey(0.5), muted("lightskyblue")))
rect(xseq-0.5, 0.973, xseq+0.5, 0.977, col=seqcol[fprop], border=NA)
rect(0, 0.973, 200, 0.977)
legend(100, 0.968, paste0(0:5*20, "%"), pch=22, pt.bg=seqcol[0:5*20+1], horiz=T, bty="n",
  xjust=0.5, yjust=0.5, pt.cex=1.7, cex=0.8)
text(100, 0.982, "% of contributing locations", cex=0.8)

par(mar=c(4,4,0,0.5))

plot(clcp, ylab="RR", xlab="Lag (days)", col=muted("lightskyblue"), 
  mgp=c(2.5,1,0), las=1, cex.axis=0.8, ci.arg=list(col=alpha("lightskyblue", 0.2))) 

par(mar=c(5,4,4,2)+0.1)
layout(1)

dev.print(pdf, "figures/explagresp.pdf", height=9, width=8)

