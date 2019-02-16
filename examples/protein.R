### *** European Protein Consumption, in grams/person-day *** ###
food <- read.csv("protein.csv", row.names=1) # 1st column is country name
## scale the data
xfood <- scale(food) 

## first, 3 means
(grpMeat <- kmeans(xfood,  centers=3, nstart=10))


plot(xfood[,"RedMeat"], xfood[,"WhiteMeat"], xlim=c(-2,2.75), 
    type="n", xlab="Red Meat", ylab="White Meat", bty="n")
text(xfood[,"RedMeat"], xfood[,"WhiteMeat"], labels=rownames(food), 
    col=rainbow(3)[grpMeat$cluster])

## same plot, but now with clustering on all protein groups
grpProtein <- kmeans(xfood, centers=7, nstart=50) ## change the number of centers to see what happens.
grpProtein


par(mai=c(.9,.9,.1,.1))
plot(xfood[,"RedMeat"], xfood[,"WhiteMeat"], xlim=c(-2,2.75), 
    type="n", xlab="Red Meat", ylab="White Meat", bty="n")
text(xfood[,"RedMeat"], xfood[,"WhiteMeat"], labels=rownames(food), 
    col=rainbow(7)[grpProtein$cluster]) ## col is all that differs from first plot



plot(xfood[,"RedMeat"], xfood[,"Fish"], xlim=c(-2,2.75), 
    type="n", xlab="Red Meat", ylab="Fish")
text(xfood[,"RedMeat"], xfood[,"Fish"], labels=rownames(food), 
    col=rainbow(7)[grpProtein$cluster]) ## col is all that differs from first plot



### WEEK 8 FACTOR STUFF

food <- read.csv("protein.csv", row.names=1)
pcfood <- prcomp(food, scale=TRUE)
round(pcfood$rotation, 1)

round( predict(pcfood, newdata=food["France",]),2)
head( round(zfood <- predict(pcfood),1)) 

## predict is just doing the same thing as the below:
z <- scale(food)%*%pcfood$rotation
all(z==zfood)

## implies rotations are on scale of standard deviations if scale=TRUE
## looks like PC1 is an 'average diet', PC2 is iberian
t( round(pcfood$rotation[,1:2],2) )

## do some k-means, for comparison
grpProtein <- kmeans(scale(food), centers=7, nstart=20)

## how do the PCs look?
par(mfrow=c(1,2))
plot(zfood[,1:2], type="n", xlim=c(-4,5))
text(x=zfood[,1], y=zfood[,2], labels=rownames(food), col=rainbow(7)[grpProtein$cluster])
plot(zfood[,3:4], type="n", xlim=c(-3,3))
text(x=zfood[,3], y=zfood[,4], labels=rownames(food), col=rainbow(7)[grpProtein$cluster])

## how many do we need? tough to tell
plot(pcfood, main="")
mtext(side=1, "European Protein Principle Components",  line=1, font=2)

## summary puts these scree plots on a more intuitive scale: 
	## proportion of variation explained.
summary(pcfood)
