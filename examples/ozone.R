data(airquality)
fit <- lm(Ozone ~ ., data=airquality)
summary(fit)$coef["Wind",]

library(AER)
bvar <- vcovHC(fit)
round(bvar,1)

sqrt(bvar["Wind","Wind"])

par(mai=c(.9,.9,.2,.1))
plot(na.omit(airquality)$Wind, fit$residuals, pch=20, bg=8, bty="n",
	xlab="Wind", ylab="OLS residuals")

B <- 10000
beta <- vector(length=B)
n <- nrow(airquality)
for(b in 1:B){
  bs = sample.int(n,n,replace=TRUE)
  bsfit <- lm(Ozone ~., data=airquality, subset=bs)
  beta[b] <- coef(bsfit)["Wind"] }
sd(beta)

coef <- summary(fit)$coef["Wind",1:2]
par(mai=c(.9,.9,.1,.1))
hist(beta, col=8, main="", 
  xlab="Coefficient for Ozone on Wind", 
  freq=FALSE,ylim=c(0,0.6),breaks=25)
grid <- seq(-6,5,length=500)
lines(grid, dnorm(grid,coef[1],coef[2]),col=2,lwd=2)
lines(grid, dnorm(grid,coef[1],sqrt(bvar["Wind","Wind"])),col=4,lwd=2)
legend("topleft",col=c(8,4,2),lwd=4, 
  legend=c("Bootstrap",
           "HC",
           "Vanilla"),bty="n")
