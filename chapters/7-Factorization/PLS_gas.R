### Partial Least Squares
library(textir)

#Perform PLS regression
gaspls <- pls(x=nir, y=octane,  K=3)

#Plot the iterations of PLS
plot(gaspls, bty="n", cex.lab=1.4)


##Perform PLS with cross-validation

#Establish cross validation folds (6 folds)
foldid <- rep(1:6,each=10)[sample(1:60)]

#Create a matrix to store OOS values
OOS <- matrix(nrow=6, ncol=10)

#Conduct cross-validtion with PLS
for(b in 1:6){
  #Prit fold iteration
  print(b)
  for(k in 1:10){
    #Perform PLS
    gpls <- pls(x=nir[foldid!=b,], y=octane[foldid!=b], K=k)
    #Collect OOS values
    OOS[b,k] <- 
      mean( (octane[foldid==b] - predict(gpls, nir[foldid==b,], K=k))^2 )
  }
}
#Calculate OOS MSE and standard deviation
cvm <- apply(OOS,2,mean)
cvs <- apply(OOS,2,sd)
OOS <- as.data.frame(OOS)
names(OOS) <- 1:10


par(mfrow=c(1,2))
#Plot the OSS MSE for different K
boxplot(OOS, ylab="mean squared error", xlab="K", col="purple", log="y", main="", ylim=c(0.01,2))
mtext(side=3, "PLS", line=2)
#Compare plot above to a lasso regression as a function of lambda
gasgl <- cv.gamlr(x=nir, y=octane, lmr=1e-4)
plot(gasgl, log="y", main="", ylim=c(0.01,2)) 
mtext(side=3, "lasso", line=2)

### Compare PLS to PC Regression for reference
#get PC's
gaspca <- prcomp(nir, scale=TRUE)
#Transofrm inputs to PC's
gaspc <- predict(gaspca)
#Perform PCR
summary(glm(gas$octane ~ gaspc[,1:10]))
## some plausible models
BIC(glm(octane ~ gaspc[,1:5]))
BIC(glm(octane ~ gaspc[,1:6]))
BIC(glm(octane ~ gaspc[,1:7]))
BIC(glm(octane ~ gaspc[,1:8]))
## Use 6 PC's for PCR
summary(glm(octane ~ gaspc[,1:6]), k=log(nrow(gas)))