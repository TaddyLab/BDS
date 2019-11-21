###################################################
############# Orange Juice Regression #############
###################################################

## read in the data
oj <- read.csv("oj.csv") 
head(oj)
levels(oj$brand)

## create some colors for the brands
brandcol <- c("green","red","gold")
par(mfrow=c(1,2))
plot(log(price) ~ brand, data=oj, col=brandcol)
plot(log(sales) ~ log(price), data=oj, col=brandcol[oj$brand])

## simple regression
reg = glm(log(sales) ~ log(price) + brand, data=oj)

## use the fitted model
summary(reg) ## coef, tests, fit
coef(reg) ## just coefficients


x <- model.matrix( ~ log(price) + brand, data=oj)
x[c(100,200,300),]
oj[c(100,200,300),]

beta <- coef(reg)

plot(log(sales) ~ log(price), data=oj, col=brandcol[oj$brand], 
	cex=.1, pch=20, bty="n")
abline(a=beta[1], b=beta[2], col=brandcol[1], lwd=2)
abline(a=beta[1]+beta[3], b=beta[2], col=brandcol[2], lwd=2)
abline(a=beta[1]+beta[4], b=beta[2], col=brandcol[3], lwd=2)
legend("bottomleft", bty="n", lwd=2, col=brandcol, legend=levels(oj$brand))

## Interactions
## note that `*' also adds the main effects automatically
reg_interact = glm(log(sales) ~ log(price)*brand, data=oj)
coef(reg_interact)
## compare brand-specific log(price) slopes to our earlier elasticity (-3.1)
beta <- coef(reg_interact)

plot(log(sales) ~ log(price), data=oj, col=brandcol[oj$brand], 
	cex=.1, pch=20, bty="n")
abline(a=beta[1], b=beta[2], col=brandcol[1], lwd=2)
abline(a=beta[1]+beta[3], b=beta[2]+beta[5], col=brandcol[2], lwd=2)
abline(a=beta[1]+beta[4], b=beta[2]+beta[6], col=brandcol[3], lwd=2)
legend("bottomleft", bty="n", lwd=2, col=brandcol, legend=levels(oj$brand))

## and finally, consider 3-way interactions
ojreg <- glm(log(sales) ~ log(price)*brand*feat, data=oj)
coef(ojreg)
# log(price)                       -2.77415    0.03883 -71.445  < 2e-16 ***
# feat                              1.09441    0.03810  28.721  < 2e-16 ***
# log(price):brandminute.maid       0.78293    0.06140  12.750  < 2e-16 ***
# log(price):brandtropicana         0.73579    0.05684  12.946  < 2e-16 ***
# log(price):feat                  -0.47055    0.07409  -6.351 2.17e-10 ***
# brandminute.maid:feat             1.17294    0.08196  14.312  < 2e-16 ***
# brandtropicana:feat               0.78525    0.09875   7.952 1.90e-15 ***
# log(price):brandminute.maid:feat -1.10922    0.12225  -9.074  < 2e-16 ***
# log(price):brandtropicana:feat   -0.98614    0.12411  -7.946 2.00e-15 ***



## for the elasticities table
b <- coef(ojreg)
b["log(price)"] 
b["log(price)"] + b["log(price):brandminute.maid"]
b["log(price)"] + b["log(price):brandtropicana"]
b["log(price)"] + b["log(price):feat"] 
b["log(price)"] + b["log(price):brandminute.maid"] + b["log(price):feat"] + b["log(price):brandminute.maid:feat"]
b["log(price)"] + b["log(price):brandtropicana"] + b["log(price):feat"] + b["log(price):brandtropicana:feat"]

## table explaining why ads confounded our brand elasticity estimates
salestable <- tapply(exp(oj$logmove), oj[,c("feat","brand")], sum)
mosaicplot(salestable,col=brandcol)

## fit plots and R^2 
## (the 'bty="n"' option removes boxes around your plot)
plot(ojreg$fitted ~ oj$logmove, col=brandcol[oj$brand], bty="n")
abline(a=0,b=1)#  add a line with slope 1, intercept 0
legend("topleft",legend=levels(oj$brand),fill=brandcol, bty="n")
cor(ojreg$fitted,oj$logmove)^2


## create some data for prediction, using the data.frame function
## note the care in specifying brand factor (levels must match original data)
## we don't need all variables in oj; just those used as covariates in reg.
newdata=data.frame(price=rep(4,3), 
	brand=factor(c("tropicana","minute.maid","dominicks"),levels=levels(oj$brand)))
## predict
predict(reg, newdata=newdata)  ## predicted log units moved
exp(predict(reg, newdata=newdata)) ## predicted # of units moved



## lasso stuff
library(gamlr)
xnaref <- function(x){
	if(is.factor(x))
		if(!is.na(levels(x)[1]))
			x <- factor(x,levels=c(NA,levels(x)),exclude=NULL)
	return(x) }

naref <- function(DF){
	if(is.null(dim(DF))) return(xnaref(DF))
	if(!is.data.frame(DF)) 
		stop("You need to give me a data.frame or a factor")
	DF <- lapply(DF, xnaref)
	return(as.data.frame(DF))
}
# with -1 to drop the intercept
oj$brand <- naref(oj$brand)
xbrand <- sparse.model.matrix( ~ brand, data=oj)
xbrand[c(100,200,300),]
oj$brand[c(100,200,300)]

## demonstrating control
basefit <- lm(log(sales) ~ log(price), data=oj)
coef(basefit)
brandfit <- lm(log(sales) ~ brand + log(price), data=oj)
coef(brandfit)

pricereg <- lm(log(price) ~ brand, data=oj)
phat <- predict(pricereg, newdata=oj)
presid <- log(oj$price)-phat
coef( residfit <- lm( log(sales) ~ presid, data=oj) )

residfit2 <- lm( log(sales) ~ phat + log(price), data=oj)
coef(residfit2)

salesreg <- lm(log(sales) ~ brand, data=oj)
shat <- predict(salesreg, newdata=oj)
sresid <- log(oj$sales) - shat
residfit3 <- lm( sresid ~ presid - 1)
coef(residfit3)
