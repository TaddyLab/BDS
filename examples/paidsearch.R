## paidsearch

sem <- read.csv("paidsearch.csv")
sem$dma <- factor(sem$dma)

## quick summary: total revenue by date and treatment/controls
totalrev <- tapply(sem$revenue, sem[,c("date","search.stays.on")], sum)
## for plotting, we'll convert the row `dates' to R Date class
## see http://www.statmethods.net/input/dates.html for format codes.
asdate <- as.Date(rownames(totalrev), format="%d-%b-%y")
## order everything by date
totalrev <- totalrev[order(asdate),]
asdate <- sort(asdate)

## plot the revenues by group
plot(asdate, totalrev[,'0'], type="l", bty="n", col=2,
	ylim=range(totalrev), log="y", xlab="", ylab="revenue")
lines(asdate, totalrev[,'1'], type="l")
legend("right",col=c(1,2), lwd=2, bty="n",
	legend=c("control (search stays on)", "treatment (search goes off)"))
abline(v=as.Date("2012-05-22"), lty=2)
## and the difference between groups
plot(asdate, log(totalrev[,'1'])-log(totalrev[,'0']), 
	type="l", bty="n", col=3,lwd=2, xlab="", 
	ylab="log(rev_control) - log(rev_treat)")
abline(v=as.Date("2012-05-22"), lty=2,lwd=2)

######  Actual Analysis #######
## we'll make use of the data.table package
library(data.table)
sem <- as.data.table(sem)
semavg <- sem[, 
			list(d=mean(1-search.stays.on), y=mean(log(revenue))), 
			by=c("dma","treatment_period")]
setnames(semavg, "treatment_period", "t") # names to match slides
semavg <- as.data.frame(semavg)

library(AER)

semreg <- glm(y ~ d*t, data=semavg)
summary(semreg)
sqrt(vcovCL(semreg, cluster=semavg$dma)['d:t','d:t'])

head(semavg)
dmareg <- glm(y ~ dma + d*t, data=semavg)
summary(dmareg)$coef["d:t",]

## just to confirm: 
##   we get the same thing just viewing this as a sample of n_dma differences.
r <- tapply(semavg$y, semavg$dma, function(y) y[2]-y[1])
d <- semavg[match(names(r),semavg$dma),"d"]
rbar <- tapply(r,d,mean)
rbarvar <- tapply(r, d, function(r) var(r)/length(r))
rbar[2]-rbar[1]
sqrt(sum(rbarvar))



