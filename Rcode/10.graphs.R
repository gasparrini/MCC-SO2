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

# SET GRAPHICAL PARAMETERS
oldpar <- par(no.readonly=T)
par(las=1, mgp=c(1.5, 0.3, 0), tcl=-0.2, cex.axis=0.7, cex.lab=0.9)

################################################################################
# SO2 DISTRIBUTION BY YEAR

# SO2 AVERAGE BY CITY AND YEAR
so2meanyear <- lapply(dlist, function(x) {
  data.table(x)[, list(so2=mean(so2, na.rm=T)), by=year(date)]
}) |> Reduce(rbind, x=_)

# LAB
lab <- expression(paste("Average ",SO[2]," concentration (",mu,"g/",m^3,")"))

# PLOT (TRICK TO DRAW GRID)
par(mar=c(3,3,0.5,0.5))
plot(pmax(so2,1)~factor(year), so2meanyear, log="y", col="lightskyblue",
  ylab="", xlab="", pch=19, cex=0.4, xaxt="n")
abline(v=0:9*4+1, h=c(1,2,5,10,20,50,100,200), col="lightgray", lty="dotted")
par(new=T) 
plot(pmax(so2,1)~factor(year), so2meanyear, log="y", col="lightskyblue",
  ylab="", xlab="", pch=19, cex=0.4, xaxt="n")
axis(1, at=0:9*4+1, labels=0:9*4+1980)
title(ylab=lab, xlab="Year")
abline(h=40, lty=5)
par(mar=c(5,4,4,2)+0.1)

dev.print(pdf, "figures/boxso2year.pdf", height=5, width=10)

# GGPLOT VERSION
ggplot(na.omit(so2meanyear), aes(factor(year), pmax(so2,1))) +
  geom_boxplot(fill="lightskyblue", outlier.size=1) +
  geom_hline(yintercept=40, size=.4) +
  scale_y_continuous(trans='log', breaks=c(1,2,5,10,20,50,100,200)) +
  scale_x_discrete(breaks=0:9*4+1980) +
  labs(y=lab, x="Year") +
  theme_bw() 

ggsave("figures/boxso2year2.pdf", height=5, width=10, unit="in")


################################################################################
# COUNTRY-SPECIFIC RR

# SEQUENCES AND RANGES
yn <- nrow(effcountry)
yseq <- c(rev(seq(yn-1)+1.5), 1)
xrange <- exp(range(c(effcountry[,c(1,3,4)]*10)))

# LAB
rrlab <- expression(paste("RR for 10 ",mu,"g/",m^3," increase in ",SO[2]))

# PLOT
par(mar=c(3,6.2,0.5,0.5))
plot(yseq, type="n", xlim=xrange, yaxt="n", xlab=rrlab, ylab="")
axis(2, at=yseq, tick=F, labels=labcountry)
grid(ny=0)
abline(v=1)
abline(v=exp(effcountry[yn,1]*10), lty=2)
arrows(exp(effcountry[,3]*10), yseq, exp(effcountry[,4]*10), yseq, 
  col="lightskyblue", code=3, angle=90, length=0.03, lwd=2)
points(exp(effcountry[-yn,1]*10), yseq[-yn], col=muted("lightskyblue"), pch=19,
  cex=1.2)
points(exp(effcountry[yn,1]*10), 1, col=muted("lightskyblue"), pch=23, bg=2,
  cex=1.4)
par(mar=c(5,4,4,2)+0.1)

dev.print(pdf, "figures/rrcountry.pdf", height=9, width=6.5)

# GGPLOT VERSION
as.matrix(unique(cities[c("country")])) |>
  rbind(c("Pooled")) |>
  cbind(as.data.frame(exp(effcountry[,c(1,3,4)]*10))) |>
  mutate(country=factor(country, levels=rev(unique(country))),
    shape=rep(c(19,18),c(yn-1,1))) |>
  ggplot(aes(blup, country)) +
  geom_vline(xintercept=1, size=.4) +
  geom_vline(xintercept=exp(effcountry[yn,1]*10), size=.4, linetype="dotted") +
  geom_errorbar(aes(xmin=pi.lb, xmax=pi.ub), colour="lightskyblue", width=0.4,
    show.legend=FALSE) +
  geom_point(size=2, aes(shape=shape), colour=muted("lightskyblue"),
    show.legend=FALSE) + scale_shape_identity() +
  labs(y = "", x=rrlab) +
  theme_bw() +
  theme(strip.text.y=element_text(angle=0), panel.grid.minor=element_blank(),
    panel.grid.major.y=element_blank())

ggsave("figures/rrcountry2.pdf", height=9, width=5.76, unit="in")

################################################################################
# EXPOSURE-RESPONSE, LAG-RESPONSE, BY PERIOD

# COMPUTE THE POOLED EXPOSURE-RESPONSE
nlcp <- crosspred(do.call(onebasis, c(list(x=0:200), nlargvarso2)),
  coef=coef(nlmeta), vcov=vcov(nlmeta), cen=0, model.link="log")

# COMPUTE THE PROPORTION OF STUDIES BY EXPOSURE RANGE
xseq <- 0:199+0.5
proprange <- rowSums(sapply(dlist, function(x) 
  xseq > min(x$so2, na.rm=T) & xseq < max(x$so2, na.rm=T))) / length(dlist)*100

# DEFINE THE COLOURS
fprop <- cut(proprange, 0:200)
col <- c("white", "aliceblue", "lightskyblue", "blue", "blue4")
seqcol <- colorRampPalette(col)(101)

# COMPUTE THE POOLED LAG-RESPONSE
clcp <- crosspred(do.call(onebasis, c(list(x=seq(0, cllagso2, by=0.1)), clarglagso2)),
  coef=coef(clmeta), vcov=vcov(clmeta), model.link="log")

# COMPUTE THE RISK BY PERIOD
rrperiod <- exp(predict(permeta, newdata=list(midyear=1980:2018), ci=T)*10) |>
  as.data.frame() |> mutate(year=1980:2018)

# RESET SOME GRAPHICAL PARAMETERS
par(mgp=c(1.8, 0.3, 0))

# PLOT
layout(1:3, heights=c(0.9,0.7, 0.7))
par(mar=c(3.5,3.5,0.5,0.5))

lab <- expression(paste(SO[2]," (",mu,"g/",m^3,")"))
plot(nlcp, type="n", ci="n", ylim=c(0.90, 1.30), ylab="RR", xlab=lab, bty="o")
grid()
lines(nlcp, col=muted("lightskyblue"), ci="area", ci.arg=list(col=alpha("lightskyblue", 0.2)))
lines(0:200, exp(coef(meta)*0:200), lty=2, col=grey(0.5))
legend("topleft", c("Linear", "Non-linear"), lty=2:1, bty="n", cex=1,
  inset=0.05, col=c(grey(0.5), muted("lightskyblue")))
rect(xseq-0.5, 0.91, xseq+0.5, 0.925, col=seqcol[fprop], border=NA)
rect(0, 0.91, 200, 0.925)
legend(100, 0.95, paste0(0:5*20, "%"), pch=22, pt.bg=seqcol[0:5*20+1], horiz=T,
  bty="n", xjust=0.5, yjust=0.5, pt.cex=1.7, cex=0.9, x.intersp=1.5, text.width=20)
text(100, 0.98, "% of contributing locations", cex=1)

par(mar=c(3.5,3.5,0,0.5))

plot(clcp, type="n", ci="n", ylab=rrlab, xlab="Lag (days)", bty="o",) 
grid()
lines(clcp, ci="area", col=muted("lightskyblue"), 
  ci.arg=list(col=alpha("lightskyblue", 0.2))) 

plot(fit~year, rrperiod, type="n", ylim=c(0.98, 1.040), xlim=c(1980,2020),
  ylab=rrlab, xlab="Year")
grid()
polygon(x=c(rrperiod$year,rev(rrperiod$year)),
  y=c(rrperiod$ci.lb,rev(rrperiod$ci.ub)), border=NA,
  col=alpha("lightskyblue", 0.2))
lines(fit~year, rrperiod, col=muted("lightskyblue"))
abline(h=1)

par(mar=c(5,4,4,2)+0.1)
layout(1)

dev.print(pdf, "figures/explagper.pdf", height=8, width=4)

# RESET GRAPHICAL PARAMETERS
par(oldpar)

################################################################################
# COMPARISON OF LINEAR AND NON-LINEAR MODELS

# OBTAIN COUNTRY-SPECIFIC BLUPS WITH VCOV FROM BOTH MODELS
blup1 <- unique(blup(meta, vcov=T, level=1))
blup2 <- lapply(blup(nlmeta, vcov=T, format="matrix", level=1), unique)

# CREATE A DATAFRAME WITH THE FITTED RELATIONSHIPS AND CI
cpmod <- lapply(seq(levels(fcntry)), function(i) {
  x <- seq(0, 100, by=1)
  one1 <- onebasis(x, "lin")
  one2 <- do.call(onebasis, c(list(x=x), nlargvarso2))
  cp1 <- crosspred(one1, coef=blup1[i,1], vcov=matrix(blup1[i,2]),
    model.link="log", at=x, cen=0)
  cp2 <- crosspred(one2, coef=blup2[[1]][i,], vcov=xpndMat(blup2[[2]][i,]), 
    model.link="log", at=x, cen=0)
  rbind(
    data.frame(country=levels(fcntry)[i], x=x, mod="Linear", rr=cp1$allRRfit,
      low=cp1$allRRlow, high=cp1$allRRhigh),
    data.frame(country=levels(fcntry)[i], x=x, mod="Non-linear", rr=cp2$allRRfit,
      low=cp2$allRRlow, high=cp2$allRRhigh)
  )
}) |> Reduce(rbind, x=_)
cpmod$country <- factor(cpmod$country, levels=levels(fcntry))

# LABEL
lab <- expression(paste(SO[2]," (",mu,"g/",m^3,")"))

# PLOT
ggplot(cpmod, aes(x=x)) +
  geom_hline(yintercept=1) +
  geom_line(aes(y=rr, col=mod, linetype=mod), size=0.7) +
  geom_ribbon(aes(ymin=low, ymax=high, fill=mod), alpha=0.3, show.legend=F) +
  scale_colour_manual(name="Model", values=c("red", "blue")) +
  scale_linetype(name="Model") +
  labs(y="Relative risk (RR)", x=lab) +
  coord_cartesian(ylim=c(0.95, 1.2)) +
  theme_bw() +
  theme(legend.position="top") +
  facet_wrap(vars(country), ncol=3)

ggsave("figures/modfit.png", height=10, width=12, unit="in")
