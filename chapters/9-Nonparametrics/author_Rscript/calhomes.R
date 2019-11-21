
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
XXca <- model.matrix(~.*longitude*latitude, data=data.frame(scale(CAhousing)))[,-1]

## what would a lasso linear model fit look like?
## it likes a pretty complicated model
par(mfrow=c(1,2))
plot(capen <- cv.gamlr(x=XXca, y=logMedVal, lmr=1e-4))
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
plot(calasso <- cv.gamlr(x=XXca, y=logMedVal))
plot(calasso$gamlr)
round(coef(calasso),2)

## predicted values
yhatlasso <- predict(calasso, XXca, lmr=1e-4)
yhattree <- predict(catree, CAhousing)
yhatrf <- predict(carf, CAhousing)$predictions

## plot the predictions by location
## the plotting is a bit complex here
## no need to figure it out if you don't want

## set up some color maps for fit and residuals
predcol = heat.colors(9)[9:1] ## see help(heat.colors)
residcol = c('red','orange',0,'turquoise','blue')
predbreaks = c(9,10,10.5,11,11.5,12,12.5,13,13.5,14.5) # borders of pred color bins
residbreaks = c(-3,-2,-1,1,2,3) # borders of resid color bins
## simple utility functions
predmap <- function(y){
  return(predcol[cut(drop(y),predbreaks)]) ## cut sorts into bins
}
residmap <- function(e){
  return(residcol[cut(drop(e), residbreaks)]) ## cut sorts into bins
}

### plot fit and resids

## again: this image plotting is all extra!  
## using the maps package to add california outline
library(maps)
par(mfrow=c(2,3))
## preds
map('state', 'california') 
points(CAhousing[,1:2], col=predmap(yhatlasso), pch=20, cex=.5)
mtext("lasso fitted")
legend("topright", title="prediction", bty="n",
       fill=predcol[c(1,4,7,9)], legend=c("20k","100k","400k","1mil"))
map('state', 'california') 
points(CAhousing[,1:2], col=predmap(yhattree), pch=20, cex=.5)
mtext("cart fitted")
map('state', 'california') 
points(CAhousing[,1:2], col=predmap(yhatrf), pch=20, cex=.5)
mtext("rf fitted")
## resids
map('state', 'california') 
points(CAhousing[,1:2], col=residmap(logMedVal - yhatlasso), cex=1.5)
mtext("lasso resid")
legend("topright", title="residuals", bty="n", fill=residcol[-3], legend=c(-2,-1, 1,2))
map('state', 'california') 
points(CAhousing[,1:2], col=residmap(logMedVal - yhattree), cex=1.5)
mtext("cart resid")
map('state', 'california') 
points(CAhousing[,1:2], col=residmap(logMedVal - yhatrf), cex=1.5)
mtext("rf resid")


## Out of sample prediction (takes a while since RF is slow)
MSE <- list(LASSO=NULL, CART=NULL, RF=NULL)
for(i in 1:10){
  train <- sample(1:nrow(CAhousing), 5000)
  
  lin <- cv.gamlr(x=XXca[train,], y=logMedVal[train], lmr=1e-4)
  yhat.lin <- drop(predict(lin, XXca[-train,]))
  MSE$LASSO <- c( MSE$LASSO, var(logMedVal[-train] - yhat.lin))

  rt <- tree(logMedVal ~ ., data=CAhousing[train,])
  yhat.rt <- predict(rt, newdata=CAhousing[-train,])
  MSE$CART <- c( MSE$CART, var(logMedVal[-train] - yhat.rt))

  rf <- ranger(logMedVal ~ ., data=CAhousing[train,], 
          num.tree=200, min.node.size=25, write.forest=TRUE)
  yhat.rf <- predict(rf, data=CAhousing[-train,])
  MSE$RF <- c( MSE$RF, var(logMedVal[-train] - yhat.rf) )
 
  cat(i)
} 
par(mai=c(.8,.8,.1,.1))
boxplot(log(as.data.frame(MSE)), col="dodgerblue", xlab="model", ylab="log(MSE)")


## parallel processing
library(parallel)
cl <- makeCluster(4) # use 4 processors
clusterExport(cl, c("randomForest","CAhousing")) # broadcast the data 
out <- parLapply(cl, rep(50,4), 
  function(ntree) randomForest(logMedVal ~ ., data=CAhousing, ntree=ntree))