

#Load gamlr package and data
library(gamlr)

# load the data
data(hockey) 

##Clean data

# Combine the covariates all together
x <- cBind(config,team,player) # cBind binds together two sparse matrices

# build 'y': home vs away, binary response
y <- goal$homegoal

#### Simple Sample Splitting ####

#Create folds
fold <- sample.int(2,nrow(x),replace=TRUE)

#Fit model to half of the data
nhlprereg <- gamlr(x[fold==1,], y[fold==1],
                   #free=1:(ncol(config)+ncol(team)), 
                   family="binomial", standardize=FALSE)

##Determine the non-zero coefficients from the regression above
# the -1 to remove the intercept
selected <- which(coef(nhlprereg)[-1,] != 0)

#Coerce nonzero coefficients to a matrix
xnotzero <- as.data.frame(as.matrix(x[,selected]))

#Fit new regression on other half of data with nonzero coefficients
nhlmle <- glm( y ~ ., data=xnotzero, 
               subset=which(fold==2), family=binomial )

#Suppose we want to predict the probability that the home team scored a given goal.
# Look at first goal in our dataset and apply standard glm prediction procedure.

#Look at first goal in our dataset
x[1,x[1,]!=0]

#Run prediction
predict(nhlmle, xnotzero[1,,drop=FALSE], type="response", se.fit=TRUE)



#### Double ML for PTLE Approach ####

#Recall the basic regression from the hockey dataset
nhlreg <- gamlr(x, y, verb=TRUE,
                free=1:(ncol(config)+ncol(team)), ## free denotes unpenalized columns
                family="binomial", standardize=FALSE)

#Odds for Crosby's team scoring a goal when he is on the ice
exp(coef(nhlreg)["SIDNEY_CROSBY",])

#Use AICc linear lasso for the treatment and AICc logistic lasso for the binary response


WHO <- "SIDNEY_CROSBY"
who <- grep(WHO,colnames(x))
# orthogonal ML
source("orthoML.R")
dreg <- function(x,d){ 
  gamlr(x, d, standardize=FALSE, lmr=1e-5) }

yreg <- function(x,d){ 
  gamlr(x, d, family="binomial", standardize=FALSE, lmr=1e-5) }

resids <- orthoPLTE( x=x[,-who], d=x[,who], y=y, 
                     dreg=dreg, yreg=yreg, nfold=5)
