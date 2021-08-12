################################################################################
# MCC-POLLUTION PROJECT: PRELIMINARY SO2 ANALYSIS 
################################################################################

################################################################################
# GRAPHS
################################################################################

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
grid()
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
pdf("figures/expresp.pdf", height=6, width=8)
layout(matrix(1:2), heights=c(2,1.5))

par(mar=c(0.5,4,1,0.5))

plot(nlcp, xlab="", ylim=c(0.98, 1.08), ylab="RR", col=muted("lightskyblue"), mgp=c(2.5,1,0),
  axes=F, ci.arg=list(col=alpha("lightskyblue", 0.2)))
axis(2, las=1, cex.axis=0.8)
points(linknots, nlcp$allRRfit[nlcp$predvar %in% linknots], col=muted("lightskyblue"), pch=19)
lines(0:200, exp(coef(meta)*0:200), lty=2, col=grey(0.5))
legend("topleft", c("Linear", "Non-linear"), lty=2:1, bty="n", cex=0.8, inset=0.05,
  col=c(grey(0.5), muted("lightskyblue")))

par(mar=c(4,4,0.5,0.5))

plot(0:200, proprange, type="h", ylim=c(0,100), xlim=par()$usr[1:2], xaxs="i",
  col=alpha("lightskyblue", 0.7), cex.axis=0.8, cex.lab=0.8, las=1,
  mgp=c(2.2,1,0), bty="l", ylab="Percentage\nof studies (%)",
  xlab="Sulfur dioxide (mcgr/m3)")
points(0:200, proprange, cex=0.4, pch=19, col=muted("lightskyblue"))

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
