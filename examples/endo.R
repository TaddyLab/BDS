
yfun <- function(e,p){
	y = 2 + 10*e-3*p + rnorm(length(e),0,.1)
	y[y<0] <- 0
	return(y)
}

e <- rgamma(100,1,1)
z <- rgamma(100,1,1)
pobs <- e + z
pind <- rgamma(100,2,1)
yobs <- yfun(e, pobs)
yind <- yfun(e, pind)

preg <- lm(pobs ~ z)
phat <- predict(preg, data.frame(z=z))
lin2SLS <- lm(yobs ~ phat)

par(mfrow=c(1,2), mai=c(.8,.8,.5,.1))
plot(pobs, yobs, xlim=c(0,6), ylim=c(0,29), pch=21, bg=8,
	xlab="", ylab="", 
	bty="n", main="observed")
abline(lm(yobs ~ pobs), col="orange", lwd=2)
abline(lin2SLS, col="dodgerblue", lwd=2)
legend("topleft", bty="n", legend=c("OLS","2SLS"),
	lwd=2, col=c("orange","dodgerblue"))
plot(pind, yind, xlim=c(0,6), ylim=c(0,29), pch=21, bg=8,
	xlab="", ylab="", 
	bty="n", main="counterfactual")
abline(lm(yind ~ pind), col="orange", lwd=2)
mtext(side=1, "price", outer=TRUE, line=-1)
mtext(side=2, "sales", outer=TRUE, line=-1)
