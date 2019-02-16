### *** Prostate Cancer Data *** ###

library(tree)

## lcavol: log(cancer volume), the response of interest
## age: age
## lbph: log(benign prostatic hyperplasia amount)
## lcp: log(capsular penetration)
## gleason: Gleason score
## lpsa: log(prostate specific antigen)

prostate <- read.csv("prostate.csv")
pstree <- tree(lcavol ~., data=prostate, mincut=1)
par(mfrow=c(1,1))
plot(pstree, col=8)
text(pstree, digits=2)

## Use cross-validation to prune the tree
cvpst <- cv.tree(pstree, K=10)
par(mai=c(.8,.8,0.1,0.1))
plot(cvpst$size, cvpst$dev, xlab="size", ylab="oos deviance", pch=21, bg="lightblue", bty="n")

par(mfrow=c(1,2))
## note across the top is 'average number of observations per leaf'; 
plot(cvpst, pch=21, bg=8, type="p", cex=1.5, ylim=c(65,100))
pstcut <- prune.tree(pstree, best=3)

par(mai=c(.8,.8,0.2,0.2), mfrow=c(1,2))
plot(pstcut, col=8)
text(pstcut)
plot(prostate[,c("lcp","lpsa")], cex=exp(prostate$lca)*.2)
abline(v=.261624, col=4, lwd=2)
lines(x=c(-2,.261624), y=c(2.30257,2.30267), col=4, lwd=2)
