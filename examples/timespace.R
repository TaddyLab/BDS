
###### Airline Passenger Numbers ######

airline <- read.csv("airline.csv")

## non-essential, but you can tell R you are working with dates
## sprintf is a function for creating strings from numbers.
## the first of every month; there's a whole syntax to this...
dmy <- sprintf("01-%d-19%d", airline$Month,  airline$Year) 
## as.Date creates a Date classed variable
## see help(as.Date) and help(strptime)
## this stuff can quickly get complicated...
date <- as.Date(dmy, format="%d-%m-%Y") 
passengers <- airline$Passengers
plot(date, passengers, 
	ylab="monthly passengers", type="l", col="maroon", lwd=2)

## plot the log version...
plot(date, passengers,  log="y",
	ylab="monthly passengers", type="l", col="navy", lwd=2)

## fit our regression 
## from here it is easier to not work with fancy Date objects
## note these are both 'month' variables; 
## the first is categorical, the second numeric
month <- factor(airline$Month)
time <- (airline$Year-min(airline$Year))*12 + airline$Month

## fit the regression 
## you could get fancier and group months together (summer, holidays),
## but we'll assume you have enough data that this doesn't make a difference.
summary(air <- glm(log(passengers) ~ time + month))

## if you do glmnet it likes the full model
library(glmnet) 
x <- sparse.model.matrix(~ time + month)
plot(airlasso <- cv.glmnet(x=x, y=log(passengers)))

## plot fit vs true
plot(date, log(passengers), 
	ylab="log passengers", type="l", col="navy", lty=2, lwd=2)
lines(date, air$fit, col="maroon", lwd=2)
legend("topleft", legend=c("true","fit"), bty="n", lwd=3, col=c("navy","maroon"))

## plot residuals
plot(time, air$resid, type="l", ylab="residuals", lwd=2, col="grey30")

## calculate and plot the acf
airac <- acf(air$resid, lag.max=20)
plot(log(airline$Passengers), xlab="year", ylab="log monthly passengers", type="l", col=4, lwd=2, xaxt="n")
axis(1, at=(0:12)*12, labels=1949:1961)

## fit the autoregressive model
lag <- head(log(passengers),-1)## see help(head)
passengers <- passengers[-1] 
month <- month[-1]
time <- time[-1]
summary(airAR <- glm(log(passengers) ~ time + month + lag))

## plot it 
par(mfrow=c(1,2))
plot(time, airAR$resid, type="l", ylab="residuals", lwd=2, col="grey30")
print(acf(airAR$resid, lag.max=20))


##########  various AR(1) series

random.walk <- rnorm(1)
for(i in 2:200){ random.walk <- c(random.walk, random.walk[i-1]+rnorm(1)) }
plot(random.walk, pch=20, col=2)
lines(random.walk, col=4)
acf(random.walk, lwd=2, main="")

dja <- read.csv("dja.csv")$DJ
n <- 1979
plot(dja, type="l", col=4, xlab="day", ylab="DJA")
summary(ARdj <- glm(dja[2:n] ~ dja[1:(n-1)]))
returns <- (dja[2:n]-dja[1:(n-1)])/dja[1:(n-1)]
plot(returns, type="l", col=3, xlab="day", ylab="DJA Return")
summary( glm(returns[2:n] ~ returns[1:(n-1)]) )

exploding.series <- rnorm(1)
for(i in 2:200){ exploding.series <- c(exploding.series, 1.02*exploding.series[i-1]+rnorm(1)) }
plot(exploding.series, pch=20, col=2)
lines(exploding.series, col=4)

stationary.series <- rnorm(1)
for(i in 2:200){ stationary.series <- c(stationary.series, 0.8*stationary.series[i-1]+rnorm(1)) }
plot(stationary.series, pch=20, col=2)
lines(stationary.series, col=4)
abline(h=0, lty=2, col=8)
acf(stationary.series, lwd=2)

negcor.series <- rnorm(1)
for(i in 2:100){ negcor.series <- c(negcor.series, -0.8*negcor.series[i-1]+rnorm(1)) }
plot(negcor.series, pch=20, col=2)
lines(negcor.series, col=4)
abline(h=0, lty=2, col=8)


