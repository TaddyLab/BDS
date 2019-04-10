##  browser spending analysis 
browser = read.csv("web-browsers.csv")

# data histogram
par(mai=c(.8,.8,.1,.1))
hist(log(browser$spend), freq=FALSE,
	xaxt="n", main="", xlab="total online spend", col=8, border="grey90")
lgrid = c(1,10,100,1000,10000,100000)
axis(1, at=log(lgrid), labels=sprintf("%.0e",lgrid))

# basic stats
nrow(browser)
mean(browser$spend)
var(browser$spend)/nrow(browser)

xbar <- mean(browser$spend)
xbse <-  sd(browser$spend)/sqrt(nrow(browser))

xx <- seq(1650,2250,length=1000)

par(mai=c(.9,.8,.2,.2))
plot(xx, dnorm(xx, xbar, xbse), type="l", col="royalblue", lwd=1.5,
	xlab="average total online spend", ylab="density")

# nonparametric bootstrap
B <- 10000
mub <- c()
for (b in 1:B){
	samp_b = sample.int(nrow(browser), replace=TRUE)
	mub <- c(mub, mean(browser$spend[samp_b]))
}
sd(mub)

par(mai=c(.8,.8,.2,.2))
hist(mub, main="", xlab="average total online spend", 
	col=8, border="grey90", freq=FALSE)
lines(xx, dnorm(xx, xbar, xbse), col="royalblue", lwd=1.5)

## parametric bootstrap
xbar <- mean(browser$spend)
sig2 <-  var(browser$spend)

B <- 10000
mus <- c()
for(b in 1:B){
  xsamp <- rnorm(1e4, xbar, sqrt(sig2))
  mus <- c(mus, mean(xsamp))
}
sd(mus)
sqrt(sig2/1e4)

## usual estiamtion of variance
smallsamp <- browser$spend[sample.int(nrow(browser),100)]
s <- sd(smallsamp) # sample variance
s
sd(browser$spend)
s/sd(browser$spend)

## CI bootstrap
eb <- c()
for (b in 1:B){
	sb <- sd(smallsamp[sample.int(100, replace=TRUE)]) 
	eb <- c(eb, sb-s)
}
mean(eb)
tvals <- quantile(eb, c(0.05, 0.95))
sd(mub)

## regression analysis
summary( glm( log(spend) ~ broadband + anychildren, data=browser) )

# nonparametric bootstrap
B <- 1000
betas <- c()
for (b in 1:B){
	samp_b = sample.int(nrow(browser), replace=TRUE)
	reg_b <- glm(log(spend) ~ broadband + anychildren, data=browser[samp_b,])
	betas <- rbind(betas, coef(reg_b))
}
head(betas)

cor(betas[,"broadband"], betas[,"anychildren"])

xx <- seq(min(betas[,2]),max(betas[,2]),length=100)

par(mai=c(.8,.8,.2,.2))
hist(betas[,2], main="", xlab="broadband coefficient", 
	col=8, border="grey90", freq=FALSE)
lines(xx, dnorm(xx, 0.55285, 0.04357), col="royalblue", lwd=1.5)

par(mai=c(.8,.8,.2,.2))
hist(exp(betas[,2]), main="", xlab="broadband multiplier", 
	col=8, border="grey90", freq=FALSE)


spendy <- glm( log(spend) ~ .-id, data=browser) 
round(summary(spendy)$coef,2)
pval <- summary(spendy)$coef[-1,"Pr(>|t|)"]

pdf("fig1.9.pdf", width=4, height=4)
par(mai=c(.8,.8,.2,.2))
plot(sort(pval), bty="n", xlab="rank", ylab=expression(italic(p)-values))
abline(a=0, b=.1/9)
points(sort(pval)[1:5], col=2, pch=20)
dev.off()

par(mai=c(.8,.8,.2,.2))
plot(c(-1,0,0,1,1,2), c(0,0,1,1,0,0), ylim=c(0,1.5), xlim=c(-0.1,1.1),
	type="l", bty="n", xlab="U", ylab="probability density", main = "uniform pdf")


par(mai=c(.8,.8,.2,.2))
plot(1:9, (1:9)/10, ylim=c(0,1),
	pch=16, col="black", bty="n", ylab="p-value", 
	xlab="order", main = "p-value order statistics")
points(1:9, sort(pval), pch=17, col=rgb(0,0,1,.5))
legend("topleft", bty="n",
	legend=c("expectation under null","observed"), pch=c(16,17),
	col=c("black",rgb(0,0,1,.5)))

