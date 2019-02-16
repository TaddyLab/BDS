library(gamlr)
load("dominicks-beer.rda")

# how many upcs?
length( upctab <- table(wber$UPC) )

# check data types
sapply(wber, class)

# create priceperoz
wber$lp <- log(12*wber$PRICE/upc[wber$UPC,"OZ"])

# smallbeer
set.seed(888)
ss <- sample.int(nrow(wber),5e3)

# all together
coef( margfit <- lm(log(MOVE) ~ lp, data=wber[ss,]) )

# numeric matrices for week, store, item
wber$s <- factor(wber$STORE)
wber$u <- factor(wber$UPC)
wber$w <- factor(wber$WEEK)
xs <- sparse.model.matrix( ~ s-1, data=wber)
xu <- sparse.model.matrix( ~ u-1, data=wber)
xw <- sparse.model.matrix( ~ w-1, data=wber)

# parse the item description text as a bag o' words
library(tm)
descr <- Corpus(VectorSource(as.character(upc$DESCRIP)))
descr <- DocumentTermMatrix(descr)
descr <- sparseMatrix(i=descr$i,j=descr$j,x=as.numeric(descr$v>0), # convert from stm to Matrix format
              dims=dim(descr),dimnames=list(rownames(upc),colnames(descr)))

descr[1:5,1:6]
descr[287,descr[287,]!=0]

controls <- cBind(xs, xu, xw, descr[wber$UPC,]) 
dim(controls)


# naive lasso
naivefit <- gamlr(x=cBind(lp=wber$lp,controls)[ss,], y=log(wber$MOVE)[ss], free=1, standardize=FALSE)
print( coef(naivefit)[1:2,] )

# full data mle fit
fullfit <- gamlr(x=cBind(lp=wber$lp,controls), y=log(wber$MOVE), lambda.start=0)
print( coef(fullfit)["lp",] )

# mle
mlefit <- gamlr(x=cBind(lp=wber$lp,controls)[ss,], y=log(wber$MOVE)[ss], lambda.start=0)
print( coef(mlefit)[1:2,] )

# double ML
source("orthoML.R")
dreg <- function(x,d){ 
	gamlr(x, d, standardize=FALSE, lmr=1e-5) }

yreg <- function(x,y){ 
	gamlr(x, y, standardize=FALSE, lmr=1e-5) }

resids <- orthoPLTE( x=controls[ss,], d=wber$lp[ss], y=log(wber$MOVE)[ss], dreg=dreg, yreg=yreg, nfold=5)

##############  heterogeneity

# interact items and text with price
lpxu <- xu*wber$lp
colnames(lpxu) <- paste("lp",colnames(lpxu),sep="")
# create our interaction matrix
xhte <- cBind(BASELINE=1,descr[wber$UPC,])
d <- xhte*wber$lp
colnames(d) <- paste("lp",colnames(d),sep=":")

eachbeer <- xhte[match(rownames(upc),wber$UPC),]
rownames(eachbeer) <- rownames(upc)

# fullhte 
fullhte <- gamlr(x=cBind(d,controls), y=log(wber$MOVE), lambda.start=0)
#gamfull <- coef(fullhte)[2:(ncol(lpxu)+1),]
gamfull <- drop(eachbeer%*%coef(fullhte)[2:(ncol(d)+1),])
hist(gamfull, main="", xlab="elasticity", ,
			 col="darkgrey", freq=FALSE)

# mle with all upcs
mlehte <- gamlr(x=cBind(d,controls)[ss,], 
	y=log(wber$MOVE)[ss], lambda.start=0)
gammle <- drop(eachbeer%*%coef(mlehte)[2:(ncol(d)+1),])
hist(gammle, main="", xlab="elasticity", breaks=200, col="pink", xlim=c(-60,25), freq=FALSE)
sort(gammle)[1:4]

# naive fit
naivehte <- gamlr(x=cBind(d,controls)[ss,], 
				  y=log(wber$MOVE)[ss], 
				  free=1, standardize=FALSE)
gamnaive <- drop(eachbeer%*%coef(naivehte)[2:(ncol(d)+1),])
hist(gamnaive, main="", xlab="elasticity", col="lightyellow", freq=FALSE)

# double ML
dmlhte <- gamlr(x=xhte[ss,]*resids$dtil, y=resids$ytil, free=1, standardize=FALSE)
coef(dmlhte)[1:2]
range( gamdml <- drop(eachbeer%*%coef(dmlhte)[-1,]) )
hist(gamdml, main="", xlab="elasticity", col="lightblue", freq=FALSE)

ylim <- c(-8,1)
par(mai=c(.7,.7,.1,.1), mfrow=c(1,3))
plot(gamfull, gammle, pch=21, bg="pink", xlab="fulldata MLE", ylab="subsample MLE", bty="n", ylim=ylim)
text(x=-6,y=1, sprintf("R2 = %.02f",summary(lm(gamfull~gammle))$r.squared))
plot(gamfull, gamnaive, pch=21, bg="lightyellow", xlab="fulldata MLE", ylab="subsample Naive ML", bty="n", ylim=ylim)
text(x=-6,y=1, sprintf("R2 = %.02f",summary(lm(gamfull~gamnaive))$r.squared))
plot(gamfull, gamdml, pch=21, bg="lightblue", xlab="fulldata MLE", ylab="subsample Orthogonal ML", bty="n", ylim=ylim)
text(x=-6,y=1, sprintf("R2 = %.02f",summary(lm(gamfull~gamdml))$r.squared))

B <- coef(dmlhte)[-(1:2),]
B <- B[B!=0]
head(sort(round(B,2)))
head(sort(round(B,2), decreasing=TRUE))

upc[which(gamfull>0),]