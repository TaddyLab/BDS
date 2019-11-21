
D <- read.csv("RD.csv")

par(mai=c(.8,.8,.3,.3))
boxplot(score ~ treat, data=D, horizontal=TRUE, 
	xlab="rank score minus reserve",
 	ylab="treatment (Ad in Main)")
abline(v=0, col=8, lty=3)



# a neighborhood
w <- 5
above <- which(D$score > 0 & D$score <w)
below <- which(D$score < 0 & D$score >-w)

# constant model
mua <- mean(D$y[above])
mub <- mean(D$y[below])
(te <- mua - mub)
vara <- var(D$y[above])
varb <- var(D$y[below])
sdte <- sqrt(vara/length(above) + varb/length(below))
te + c(-2,2)*sdte

# local linear regression
fita <- loess(y ~ score, data=D[above,], degree=1)
fitb <- loess(y ~ score, data=D[below,], degree=1)
rr <- seq(0.001,w-0.001,length=100)
preda <- predict(fita,rr) 
predb <- predict(fitb,-rr)

h <- 3
window <- which(D$score > -h & D$score < h)
summary(linfit <- lm(y ~ treat*score, data=D, subset=window))

nr <- 10
rgrid <- seq(0, h, length=nr)
dgrid <- data.frame(score=c(-rev(rgrid),rgrid), 
					treat=rep(c(0,1),each=nr))
linpred <- predict(linfit, newdata=dgrid)


library(AER)
( ate <- coef(linfit)["treat"] )
seate <- sqrt(vcovHC(linfit)["treat","treat"])
ate + c(-2,2)*seate


par(mfrow=c(1,2), omi=c(.5,0,0,0), mai=c(.3,.9,.4,.2))
plot(y ~ score, data=D, subset=sample(c(above,below),10000), 
		cex=.3, col=8, bty="n", xlab="", main="data sample")
plot(rr, preda, xlab="", col="grey50", ylab="y",lwd=2, 
	main="RD analysis",
	ylim=range(c(preda,predb)), xlim=c(-w,w), type="l", bty="n")
legend("right", bty="n", lwd=2, lty=c(1,2,1), col=c("grey50","red","blue"), 
	legend=c("loess","constant","linear"))
lines(-rr, predb, col="grey50", lwd=2)
lines(dgrid$score[1:nr], linpred[1:nr], lwd=1.5, col=4)
lines(dgrid$score[nr+1:nr], linpred[nr+1:nr], lwd=1.5, col=4)
lines(dgrid$score[1:nr], rep(mub,nr), lwd=1.5, lty=2, col=2)
lines(dgrid$score[nr+1:nr], rep(mua,nr), lwd=1.5, lty=2, col=2)
lines(c(0,0),coef(linfit)[1] + c(0,coef(linfit)[2]), lwd=1.5, col=2, lty=3)
mtext(side=1, "rank score", outer=TRUE, line=1)



hh <- seq(.1,5,length=50)
ateh <- seah <- rep(0,length(hh))
for(i in 1:length(hh)){
	print(i)
	fith <- lm(y ~ treat*score, data=D, 
		subset=which(abs(D$score) < hh[i]))
	ateh[i] <- coef(fith)[2]
	seah[i] <- sqrt(vcovHC(fith)[2,2])
}

up <- ateh+2*seah
down <- ateh-2*seah


par(mai=c(.8,.8,.3,.3))

plot(hh, ateh, type="l", ylim=range(c(up,down)), 
	xlab="window size", ylab="ATE estimate", bty="n")
polygon(c(hh,rev(hh)), c(up,rev(down)), col=8, border=FALSE)
lines(hh, ateh, col="blue", lwd=2)

