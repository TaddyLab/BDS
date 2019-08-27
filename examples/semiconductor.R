
SC <- read.csv("semiconductor.csv")

## full model
full <- glm(FAIL ~ ., data=SC, family=binomial)
1 - full$deviance/full$null.deviance

## grab p-values
pvals <- summary(full)$coef[-1,4] #-1 to drop the intercept
## plot them: it looks like we have some signal here
hist(pvals, xlab="p-value", main="", col="lightblue")

## At 10% FDR, we get 25 `signif'
fdr_cut <- function(pvals, q=0.1){
  pvals <- sort(pvals[!is.na(pvals)])
  N <- length(pvals)
  k <- rank(pvals, ties.method="min")
  alpha <- max(pvals[ pvals<= (q*k/(N+1)) ])
  
  plot(pvals, log="xy", xlab="order", main=sprintf("FDR of %g",q),
   ylab="p-value", bty="n", col=c(8,2)[(pvals<=alpha) + 1], pch=20)
  lines(1:N, q*(1:N)/(N+1))

  return(alpha)
}

fdr_cut(pvals)

## Re-run a cut regression using only these 25
( signif <- which(pvals <= 0.0122) )
cut <- glm(FAIL ~ ., data=SC[,c("FAIL", names(signif))], family="binomial")
1 - cut$deviance/cut$null.deviance # new in-sample R2

## Out of sample prediction experiment
## first, define the deviance and R2 functions

## pred must be probabilities (0<pred<1) for binomial
deviance <- function(y, pred, family=c("gaussian","binomial")){
	family <- match.arg(family)
	if(family=="gaussian"){
		return( sum( (y-pred)^2 ) )
	}else{
		if(is.factor(y)) y <- as.numeric(y)>1
		return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
	}
}

## get null deviance too, and return R2
R2 <- function(y, pred, family=c("gaussian","binomial")){
	fam <- match.arg(family)
	if(fam=="binomial"){
		if(is.factor(y)){ y <- as.numeric(y)>1 }
	}
	dev <- deviance(y, pred, family=fam)
	dev0 <- deviance(y, mean(y), family=fam)
	return(1-dev/dev0)
}

# setup the experiment
n <- nrow(SC) # the number of observations
K <- 10 # the number of `folds'
# create a vector of fold memberships (random order)
foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)]
# create an empty dataframe of results
OOS <- data.frame(full=rep(NA,K), cut=rep(NA,K)) 
# use a for loop to run the experiment
for(k in 1:K){ 
	train <- which(foldid!=k) # train on all but fold `k'
		
	## fit the two regressions
	rfull <- glm(FAIL~., data=SC, subset=train, family=binomial)
	rcut <- glm(FAIL~., data=SC[,cutvar], subset=train, family=binomial)

	## get predictions: type=response so we have probabilities
	predfull <- predict(rfull, newdata=SC[-train,], type="response")
	predcut <- predict(rcut, newdata=SC[-train,], type="response")

	## calculate and log R2
	OOS$full[k] <- R2(y=SC$FAIL[-train], pred=predfull, family="binomial")
	OOS$cut[k] <- R2(y=SC$FAIL[-train], pred=predcut, family="binomial")

	## print progress
	cat(k, " ")
}
## plot it in plum
par(mai=c(.9,.9,.1,.1))
boxplot(OOS, col="plum", ylab="R2", xlab="model", bty="n")

## what are the average OOS R2?
colMeans(OOS) # WOW!  Full model really sucks.

## A forward stepwise procedure
# null model
null <- glm(FAIL~1, data=SC)
# forward stepwise: it takes a long time!
system.time(fwd <- step(null, scope=formula(full), dir="forward"))
length(coef(fwd)) # chooses around 70 coef

#### lasso (glmnet does L1-L2, gamlr does L0-L1)
library(gamlr) 
# for gamlr, and most other functions, you need to create your own numeric
# design matrix.  We'll do this as a sparse `simple triplet matrix' using 
# the sparse.model.matrix function.
scx <- sparse.model.matrix(FAIL ~ ., data=SC)[,-1] # do -1 to drop intercept!
# here, we could have also just done x <- as.matrix(SC[,-1]).
# but sparse.model.matrix is a good way of doing things if you have factors.
scy <- SC$FAIL # pull out `y' too just for convenience

# fit a single lasso
sclasso <- gamlr(scx, scy, family="binomial")
plot(sclasso) # the ubiquitous path plot

# AICc selected coef
scbeta <- coef(sclasso) 
log(sclasso$lambda[which.min(AICc(sclasso))])
sum(scbeta!=0) # chooses 30 (+intercept) @ log(lambda) = -4.5

# alt: BIC selected coef
BICseg <- which.min(BIC(sclasso))
scb.bic <- coef(sclasso, s=BICseg)
sum(scb.bic!=0) # sets all coef to zero: just the intercept!

## cross validated lasso (verb just prints progress)
sccvl <- cv.gamlr(scx, scy, family="binomial", verb=TRUE)
plot(sccvl, bty="n")

## CV min deviance selection
scb.min <- coef(sccvl, select="min")
log(sccvl$lambda.min)
sum(scb.min!=0) ## around 65-70 with log(lam) -4.8 (its random!)

## CV 1se selection (the default)
scb.1se <- coef(sccvl)
log(sccvl$lambda.1se)
sum(scb.1se!=0) ## usually selects all zeros (just the intercept)

## comparing AICc, BIC, and the CV error
plot(sccvl, bty="n")
lines(log(sclasso$lambda),AICc(sclasso)/n, col="green", lwd=2)
lines(log(sclasso$lambda),BIC(sclasso)/n, col="maroon", lwd=2)
legend("top", fill=c("blue","green","maroon"),
	legend=c("CV","AICc","BIC"), bty="n")

## BIC, AIC, AICc on full vs cut (they all prefer cut)
BIC(full)
BIC(cut)

AIC(full)
AIC(cut)

AICc(full)
AICc(cut)