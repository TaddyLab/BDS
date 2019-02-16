
###### *** Gas Octane Data *** ######

gas <- read.csv("gasoline.csv")
octane <- gas[,1]
nir <- as.matrix(gas[,-1])

nm <- sapply(
	strsplit(substring(colnames(nir),2), ".", fixed=TRUE),
	function(v) as.numeric(v[1]))

par(mai=c(.8,.8,0,0))
plot(nm, nir[1,], type='l', ylab='NIR', bty="n", xlab="")
for(i in 2:60) 
	lines(nm, nir[i,], col=rainbow(60)[i])
mtext(side=1, "nanometres", line=2.5)

### marginal regression
phi <- cor(nir, octane)/apply(nir,2,sd) 
v <- nir%*%phi
fwd <- glm(octane ~ v)

par(mai=c(.8,.8,.0,0))
plot(v, octane, pch=21, bg="lightgreen", bty="n", 
	xlab="MR factor v")

### Partial Least Squares
library(textir)
gaspls <- pls(x=nir, y=octane,  K=3)

par(mfrow=c(1,3), mai=c(.7,.7,.1,.1))
plot(gaspls, bty="n", cex.lab=1.4)

foldid <- rep(1:6,each=10)[sample(1:60)]
OOS <- matrix(nrow=6, ncol=10)
for(b in 1:6){
	print(b)
	for(k in 1:10){
		gpls <- pls(x=nir[foldid!=b,], y=octane[foldid!=b], K=k)
		OOS[b,k] <- 
			mean( (octane[foldid==b] - predict(gpls, nir[foldid==b,], K=k))^2 )
	}
}
cvm <- apply(OOS,2,mean)
cvs <- apply(OOS,2,sd)
OOS <- as.data.frame(OOS)
names(OOS) <- 1:10

par(mfrow=c(1,2), mai=c(.9,.8,.8,.1))
boxplot(OOS, ylab="mean squared error", xlab="K", col="purple", log="y", main="", ylim=c(0.01,2))
mtext(side=3, "PLS", line=2)
gasgl <- cv.gamlr(x=nir, y=octane, lmr=1e-4)
plot(gasgl, log="y", main="", ylim=c(0.01,2)) 
mtext(side=3, "lasso", line=2)

### PC Regression for reference
gaspca <- prcomp(nir, scale=TRUE)
gaspc <- predict(gaspca)
summary(glm(gas$octane ~ gaspc[,1:10]))
## some plausible models
BIC(glm(octane ~ gaspc[,1:5]))
BIC(glm(octane ~ gaspc[,1:6]))
BIC(glm(octane ~ gaspc[,1:7]))
BIC(glm(octane ~ gaspc[,1:8]))
## use 6 PC's
summary(glm(octane ~ gaspc[,1:6]), k=log(nrow(gas)))

