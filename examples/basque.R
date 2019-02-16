library(Synth)
library(tidyr)
data(basque)

## synthetic controls analysis
y <- basque[,1:4] %>% spread(year, gdpcap)
rownames(y) <- y$regionname
y <- y[c(17,2:16,18), -(1:2)]
y <- y[,1:35]

# untreated years are through 1968
library(gamlr)
synthc <- function(j, tyear=1968, ...){
	y0t <- t(y[,1:(tyear-1954)])
	fit <- gamlr( y0t[,-j], y0t[,j], ...)
	plot(fit)
	y0hat <- predict(fit, t(y[-j,]))[,1]
	return(list(w=coef(fit)[,1], y0hat=y0hat ) )
}

# run the synthetic controls
sc <- synthc(1, lmr=1e-4)
sc$w

# permutation test
library(parallel)
cl <- makeCluster(detectCores())
clusterExport(cl, c("y", "gamlr", "synthc"))

gety0 <- function(j){ synthc(j, lmr=1e-4)$y0hat }
Ysynth <- parSapply(cl, 1:nrow(y), gety0)
diff <- Ysynth - t(y)

# produce the plots
year <- as.numeric(colnames(y))
plot(year, sc$y0hat, type="l", ylab="gdp per capita",
	col=rgb(.1,.5,1,0.8), ylim=range(c(y[1,],sc$y0hat)), bty="n", lwd=2)
abline(v=1968, col=8, lty=2)
lines(year, y[1,], col=rgb(1,.5,0,.8), lwd=2)
legend("topleft", bty="n", legend=c("observed basque","synthetic basque"), 
	lwd=2, col=c(col=rgb(1,.5,0,.8),rgb(.1,.5,1,0.8)) )

matplot(year, diff, type="l", lwd=1.5, 
	xlab="year", ylab="synthetic - observed",
 	col=8, lty=1, bty="n")
lines(year, diff[,1], lwd=1.5, col="red")
lines(year, diff[,14], lwd=1.5, lty=2, col=1)
legend("topleft", bty="n", 
	legend=c("basque", "placebo", "(madrid)"), 
	lty=c(1,1,2), lwd=2, col=c(2,8,1))
