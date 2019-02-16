#### web-browsing data

library(gamlr)

## Browsing History. 
## The table has three colums: [machine] id, site [id], [# of] visits
web <- read.csv("browser-domains.csv")
## Read in the actual website names and relabel site factor
sitenames <- scan("browser-sites.txt", what="character")
web$site <- factor(web$site, levels=1:length(sitenames), labels=sitenames)
## also factor machine id
web$id <- factor(web$id, levels=1:length(unique(web$id)))

## get total visits per-machine and % of time on each site
## tapply(a,b,c) does c(a) for every level of factor b.
machinetotals <- as.vector(tapply(web$visits,web$id,sum)) 
visitpercent <- 100*web$visits/machinetotals[web$id]

## use this info in a sparse matrix
## this is something you'll be doing a lot; familiarize yourself.
xweb <- sparseMatrix(
	i=as.numeric(web$id), j=as.numeric(web$site), x=visitpercent,
	dims=c(nlevels(web$id),nlevels(web$site)),
	dimnames=list(id=levels(web$id), site=levels(web$site)))

# what sites did household 1 visit?
head(xweb[1, xweb[1,]!=0])

## now read in the spending data 
yspend <- read.csv("browser-totalspend.csv", row.names=1)  # us 1st column as row names
yspend <- as.matrix(yspend) ## good practice to move from dataframe to matrix

## run a lasso path plot
spender <- gamlr(xweb, log(yspend), verb=TRUE)
plot(spender) ## path plot

# run a stepwise regression plot
stepspend <- gamlr(xweb, log(yspend), verb=TRUE, gamma=Inf, lmr=.1)
par(mai=c(.9,.9,.1,.1))
plot(stepspend, df=FALSE, select=FALSE) ## path plot



B <- coef(spender) ## the coefficients selected under AICc
## a few examples
B <- B[-1,] # drop intercept and remove STM formatting
B[which.min(B)] ## low spenders spend a lot of time here
B[which.max(B)] ## big spenders hang out here

coef(spender, select=which.min(BIC(spender))) ## and BIC instead
AIC(spender)
cv.spender <- cv.gamlr(xweb, log(yspend), verb=TRUE)
beta1se <- coef(cv.spender) ## 1se rule; see ?cv.gamlr
betamin <- coef(cv.spender, select="min") ## min cv selection
cbind(beta1se,betamin)[c("tvguide.com","americanexpress.com"),]

## plot them together
par(mfrow=c(1,2))
plot(cv.spender)
plot(cv.spender$gamlr) ## cv.gamlr includes a gamlr object

## log lambdas selected under various criteria
log(spender$lambda[which.min(AICc(spender))])
log(spender$lambda[which.min(AIC(spender))])
log(spender$lambda[which.min(BIC(spender))])
log(cv.spender$lambda.min)
log(cv.spender$lambda.1se)

## plot CV results and the various IC
ll <- log(spender$lambda) ## the sequence of lambdas
par(mfrow=c(1,2))
plot(cv.spender)
plot(ll, AIC(spender)/n, 
	xlab="log lambda", ylab="IC/n", pch=21, bg="orange")
abline(v=ll[which.min(AIC(spender))], col="orange", lty=3)
abline(v=ll[which.min(BIC(spender))], col="green", lty=3)
abline(v=ll[which.min(AICc(spender))], col="black", lty=3)
points(ll, BIC(spender)/n, pch=21, bg="green")
points(ll, AICc(spender)/n, pch=21, bg="black")
legend("topleft", bty="n",
	fill=c("black","orange","green"),legend=c("AICc","AIC","BIC"))


## all metrics, together in a path plot.
plot(spender, col="grey")
abline(v=ll[which.min(AICc(spender))], col="black", lty=2)
abline(v=ll[which.min(AIC(spender))], col="orange", lty=2)
abline(v=ll[which.min(BIC(spender))], col="green", lty=2)
abline(v=log(cv.spender$lambda.min), col="blue", lty=2)
abline(v=log(cv.spender$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, 
	col=c("black","orange","green","blue","purple"),
	legend=c("AICc","AIC","BIC","CV.min","CV.1se"))





