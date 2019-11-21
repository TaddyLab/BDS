### *** data on tv shows from NBC *** ###

shows <- read.csv("nbc_showdetails.csv", row.names=1) ## show details; ratings and engagement

## take a look at the types of shows
plot(GRP ~ PE, data=shows, bg=c(4,2,3)[shows$Genre], pch=21, log="y")
legend("bottomright", legend=levels(shows$Genre), fill=c(4,2,3), bty="n")

## Now read the pilot focus group survey results
## for each question, 1=strongly disagree, 5=strongly agree.
## 1: 'The show makes me feel ____', 2: 'I found the show ____'
survey <- read.csv("nbc_pilotsurvey.csv", as.is=TRUE) 
survey$Show <- factor(survey$Show, levels=rownames(shows))

## First, aggregate average survey results by show
Xpilot <- aggregate(survey[,-(1:2)],  ## -(1:2) to remove the 'show' and 'viewer'
                by=list(Show=survey$Show), mean)
## aggregate adds the 'by' variable levels back in; 
## we'll strip it (show names) and use them as rownames
rownames(Xpilot) <- Xpilot[,1]
Xpilot <- Xpilot[,-1]
all(rownames(Xpilot)==rownames(shows)) ## sanity check

### Now look at PCA of the (average) survey responses.  
## This is a common way to treat survey data
PCApilot <- prcomp(Xpilot, scale=TRUE)

## screeplot
plot(PCApilot, main="")
mtext(side=1, "Pilot-Survey PCs",  line=1, font=2)

# first few pcs
## first is maybe in 'bad' or 'not drama' direction,  
## second loads high on both boring and comforted...
round(PCApilot$rotation[,1:3],1) 

## calculate pc directions
zpilot <- predict(PCApilot)

## look at a plot of them
par(mai=c(.8,.8,0,.1))
plot(zpilot[,1:2], col=0, bty="n", # col=0 to get an empy plot
   ylim=c(-3,3), xlim=c(-6,6), # hides "monarch cove",living with ed", and "next" but these are all tiny 
    main="") 
text(zpilot[,1:2], labels=rownames(zpilot), 
    col=c("navy","red","green")[shows$Genre], # color by genre
    cex=shows$PE/mean(shows$PE)) # size by show

### Principal components regression
library(gamlr) # to get AICc, plus we'll do lasso below

PE <- shows$PE
## convert to a data frame so glm can keep track of names
zdf <- as.data.frame(zpilot)
## do regression onto the first two
summary(PEglm <- glm(PE ~ ., data=zdf[,1:2]))

## Get glm fits on 1:20 factors
kfits <- lapply(1:20, # do the below for K=1:20
	function(K) glm(PE~., data=zdf[,1:K,drop=FALSE]))
aicc <- sapply(kfits, AICc) # apply AICc to each fit
which.min(aicc) ## it likes 7 factors best
## you could also use BIC
bic <- sapply(kfits, BIC) 
which.min(bic) ## likes 3

## now the lasso
lassoPCR <- gamlr(zpilot, PE)
B <- coef(lassoPCR)[-1,]
B[B!=0]
## nfold=20 for leave-two-out CV... 
cvlassoPCR <- cv.gamlr(x=zpilot, y=PE, nfold=20)
## lasso.1se agrees with IC on first 2, then grabs a couple extra
coef(cvlassoPCR) 

## plot 'em

par(mfrow=c(1,2))
plot(aicc, pch=21, bg="maroon", xlab="K", ylab="AICc")
plot(lassoPCR, col=0, ylim=c(160,215), ylab="AICc")
points(log(lassoPCR$lambda), AICc(lassoPCR), pch=21, bg="navy")

## compare to an un-factorized lasso
cvlasso <- cv.gamlr(x=as.matrix(Xpilot), y=PE, nfold=20)

cvlassoboth <- cv.gamlr(x=as.matrix(cbind(Xpilot,zpilot)), y=PE, nfold=20)
## since you haven't simplified into linear factors 
## the estimation variance overwhelms any signal

par(mfrow=c(1,3), mai=c(.2,.2,.5,.1), omi=c(.5,.5,0,0))
plot(cvlasso, main="Lasso on X", ylim=c(50,200), ylab="", xlab="", df=FALSE, bty="n")
plot(cvlassoPCR, main="Lasso on V (PCR)", ylim=c(50,200), ylab="", xlab="", df=FALSE, bty="n")
plot(cvlassoboth, main="Lasso on X and V", ylim=c(50,200), ylab="", xlab="", df=FALSE, bty="n")
mtext(side=2, "mean squared error", outer=TRUE, line=2)
mtext(side=1, "log lamba", outer=TRUE, line=2)

#### TREES STUFF

library(gamlr)

## new packages
library(tree)
library(randomForest)

## read in the NBC show characteristics
nbc <- read.csv("nbc_showdetails.csv")

## lets look at the show demographics for predicting genre
demos <- read.csv("nbc_demographics.csv", row.names=1)
genre <- nbc$Genre
## tree fit; it knows to fit a classification tree since genre is a factor.
## for two-level factors (e.g. spam) make sure you do factor(spam)
genretree <- tree(genre ~ ., data=demos[,-1], mincut=1)
## tree plot
plot(genretree, col=8, lwd=2)
## print the predictive probabilities
text(genretree, label="yprob")

## example of prediction (type="class"  to get max prob classifications back)
genrepred <- predict(genretree, newdata=demos[,-1], type="class")

## example of random forest for classification
genrerf <- randomForest(genre ~ ., data=demos[,-1], importance=TRUE)
varImpPlot(genrerf,type=1)
## random forest also just gives you the max prob class.
genrerfclass <- predict(genrerf, newdata=demos[,-1])


## create a design matrix ourselves and re-name genre for convenience 
x <- as.data.frame(model.matrix(PE ~ Genre + GRP, data=nbc)[,-1])
names(x) <- c("reality","comedy","GRP")
nbctree <- tree(nbc$PE ~ ., data=x, mincut=1)
## now plot it
par(mfrow=c(1,2))
plot(nbctree, col=8)
text(nbctree, cex=.75, font=2)
## add a look at fit using the predict function
par(mai=c(.8,.8,.2,.2))
plot(PE ~ GRP, data=nbc, col=c(4,2,3)[nbc$Genre], pch=20, ylim=c(45,90))
newgrp <- seq(1,3000,length=1000)
lines(newgrp, predict(nbctree, newdata=data.frame(GRP=newgrp, drama=1, comedy=0, reality=0)), col=4)
lines(newgrp, predict(nbctree, newdata=data.frame(GRP=newgrp, drama=0, comedy=1, reality=0)), col=3)
lines(newgrp, predict(nbctree, newdata=data.frame(GRP=newgrp, drama=0, comedy=0, reality=1)), col=2)

