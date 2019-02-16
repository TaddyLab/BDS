
##### *** rollcall voting *** #####

votes <- read.csv("rollcall-votes.csv")
legis <- read.csv("rollcall-members.csv")

pcavote <- prcomp(votes, scale=TRUE)
plot(pcavote, main="")
mtext(side=1, "Rollcall-Vote Principle Components",  line=1, font=2)

votepc <- predict(pcavote) # scale(votes)%*%pcavote$rotation
plot(votepc[,1:2], pch=21, bg=(4:2)[legis$party], main="")

# big scores on pc1 are left and right ideologically
votepc[order(votepc[,1])[1:5],1]
votepc[order(-votepc[,1])[1:5],1]

# big scores -/+ on pc 2?
votepc[order(votepc[,2])[1:5],2]
votepc[order(-votepc[,2])[1:5],2]

# look at the loadings
loadings <- pcavote$rotation[,1:2]

## the 1st is traditional left-right
hist(loadings[,1], main="", xlab="1st Principle Component Vote-Loadings",
     col=8, border=grey(.9))
abline(v=loadings[884,1], col=2)
text(x=loadings[884,1], y=550, "Afford. Health (amdt.)", xpd=TRUE, col=2, font=3)
abline(v=loadings[25,1], col=4)
text(x=loadings[25,1], y=550, "TARP", xpd=TRUE, col=4, font=3)

## trying to interpret the 2nd factor
loadings[order(abs(loadings[,2]), decreasing=TRUE)[1:5],2]
## attendance!
sort(rowSums(votes==0), decreasing=TRUE)[1:5]





