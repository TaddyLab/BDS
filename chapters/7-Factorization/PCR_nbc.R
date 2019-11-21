### *** data on tv shows from NBC *** ###

#Import data
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

## #Perform PCA on cleansed survey data
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

##Two plots:
# On left (first plot): AICc selection results across K for glm
# On Right (second plot): Lambda for lasso fitted onto all 20 PC's 

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
