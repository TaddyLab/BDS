library(MASS)
data(mcycle)
x <- mcycle[,1,drop=FALSE]
y <- mcycle[,2]

library(laGP)
## get parameters 
d <- darg(NULL, x)
g <- garg(list(mle=TRUE), y)
## initialize (dK=TRUE saves info we need in estimation)
gpi <- newGP(x, y, d=d$start, g=g$start, dK=TRUE)


print(jmleGP(gpi, drange=c(d$min, d$max), grange=c(g$min, g$max)))

## plot the resulting predictive surface
N <- 100
XX <- matrix(seq(min(x), max(x), length=N), ncol=1)
p <- predGP(gpi, XX, lite=TRUE)

par(mai=c(.8,.8,.1,.1))
plot(x[,1], y, main="", col=8, xlab="time", ylab="accelleration", bty="n")
lines(XX, p$mean, lwd=2)
lines(XX, p$mean+1.96*sqrt(p$s2*p$df/(p$df-2)), col=2, lty=2)
lines(XX, p$mean-1.96*sqrt(p$s2*p$df/(p$df-2)), col=2, lty=2)

