
###### *** California Housing Data *** ######
library(tree)
library(gamlr)
library(ranger)

## median home values in various census tracts
## lat/long are centroids of the tract
## response value is log(medianhomeval)
CAhousing <- read.csv("CAhousing.csv")
CAhousing$AveBedrms <- CAhousing$totalBedrooms/CAhousing$households
CAhousing$AveRooms <- CAhousing$totalRooms/CAhousing$households
CAhousing$AveOccupancy <- CAhousing$population/CAhousing$households
logMedVal <- log(CAhousing$medianHouseValue)
CAhousing <- CAhousing[,-c(4,5,9)] # lose medval and the room totals
CAhousing$logMedVal <- logMedVal # attach to the DF

## create a full matrix of interactions (only necessary for linear model)
## do the normalization only for main variables.
XXca <- model.matrix(logMedVal~.*longitude*latitude, data=data.frame(scale(CAhousing)))[,-1]

## what would a lasso linear model fit look like?
## it likes a pretty complicated model
par(mfrow=c(1,2))
plot(capen <- cv.gamlr(x=XXca, y=logMedVal, lmr=1e-6, standardize=FALSE))
plot(capen$gamlr)
round(coef(capen),2)

#### Trees

## First, lets do it with CART
## no need for interactions; the tree finds them automatically
catree <- tree(logMedVal ~ ., data=CAhousing) 
plot(catree, col=8, lwd=2)
text(catree)
## looks like the most complicated tree is best! 
cvca <- cv.tree(catree)
cvca$size[which.min(cvca$dev)]
plot(cvca)

## Next, with random forest (takes some time to run)
## limit the number of trees and the minimum tree size for speed
## add importance=TRUE so that we store the variable importance information
carf <- ranger(logMedVal ~ ., data=CAhousing, 
  write.forest=TRUE, num.tree=200, min.node.size=25, importance="impurity")
## variable importance 
sort(carf$variable.importance, decreasing=TRUE)

##### prediction and plotting fit

## the spatial mapping is totally extra; no need to replicate
par(mfrow=c(1,2))
plot(calasso <- cv.gamlr(x=XXca, y=logMedVal,lmr=1e-5))
plot(calasso$gamlr)
round(coef(calasso),2)

## predicted values
yhatlasso <- predict(calasso, XXca, select="min")
yhattree <- predict(catree, CAhousing)
yhatrf <- predict(carf, CAhousing)$predictions

## plot the predictions by location
## the plotting is a bit complex here
## no need to figure it out if you don't want


rl <- drop(logMedVal - yhatlasso)
rt <- logMedVal - yhattree
rr <- logMedVal - yhatrf

pdf("CAHresiduals.pdf", width=8, height=4)
par(mfrow=c(1,3), mai=c(.1,.1,.1,.1), omi=c(0,0,0,0))
map('state', 'california') 
points(CAhousing[,1:2], col=c("black","gray70")[1 + (rl>0)], cex=abs(rl))
mtext("lasso", line=1)
map('state', 'california') 
points(CAhousing[,1:2], col=c("black","gray70")[1 + (rt>0)], cex=abs(rt))
mtext("tree", line=1)
map('state', 'california') 
points(CAhousing[,1:2], col=c("black","gray70")[1 + (rr>0)], cex=abs(rr))
mtext("forest", line=1)
legend("topright", title="residuals", bty="n", pch=1, pt.cex=c(2,1,1,2), col=c("gray70","gray70","black","black"), legend=c(2,1, -1,-2))
dev.off()

### plot fit and resids
## Out of sample prediction (takes a while since RF is slow)
MSE <- list(LASSO=NULL, CART=NULL, RF=NULL)
for(i in 1:10){
  train <- sample(1:nrow(CAhousing), 5000)
  
  lin <- cv.gamlr(x=XXca[train,], y=logMedVal[train], lmr=1e-4)
  yhat.lin <- drop(predict(lin, XXca[-train,], select="min"))
  MSE$LASSO <- c( MSE$LASSO, var(logMedVal[-train] - yhat.lin))

  rt <- tree(logMedVal ~ ., data=CAhousing[train,])
  yhat.rt <- predict(rt, newdata=CAhousing[-train,])
  MSE$CART <- c( MSE$CART, var(logMedVal[-train] - yhat.rt))

  rf <- ranger(logMedVal ~ ., data=CAhousing[train,], 
          num.tree=200, min.node.size=25, write.forest=TRUE)
  yhat.rf <- predict(rf, data=CAhousing[-train,])$predictions
  MSE$RF <- c( MSE$RF, var(logMedVal[-train] - yhat.rf) )
 
  cat(i)
} 
par(mai=c(.8,.8,.1,.1))
boxplot(log(as.data.frame(MSE)), col="dodgerblue", xlab="model", ylab="log(MSE)")
