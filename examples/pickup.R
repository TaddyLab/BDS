## pickup truck regression
## 
## this data are price, mileage, year, and brand for
## full-size trucks from craigslist in 2008.

## you'll first need to make your 'working directory' to be 
## wherever you've stored the data.  
## You can do this from the menu bar, or with a command like
## setwd("/Users/mtaddy/teaching/datamining/data")

getwd() # this will print your working directory

## read the data
trucks <- read.csv("pickup.csv")
 
xx <- seq(500,30000,length=1000)
plot(xx, dnorm(xx,mean(trucks$price), sd(trucks$price)))

## explore a bit
nrow(trucks) # sample size
trucks[1,] # the first observation
trucks[1:3,] # the first three observations
trucks[1:3,1] # the first variable (year)
trucks$year[1:3] # same thing
trucks[1:3,'year'] # same thing again
summary(trucks) # summary of each variable
trucks[trucks$miles>200000,] # some real clunkers

## the make variable is special
class(trucks$make) # it is a factor (categorical)
levels(trucks$make) # with 3 levels
trucks$make[1:2] # the first two obs are GMC and Dodge
as.numeric(trucks$make[1:2]) # which R calls levels 3 and 1

## plots
hist(trucks$price) ## a histogram
plot(price ~ make, data=trucks) ## a boxplot
plot(price~miles, data=trucks) ## simple scatterplot
plot(price~miles, data=trucks, log="y") ## price on log scale
plot(price~miles, data=trucks, log="y", col=trucks$make) ## in color
## add a legend (colors 1,2,3 are black,red,green)
legend("topright", fill=1:3, legend=levels(trucks$make))

## regressions: write these models out, make sure you understand them!

## main effects, regression of log price on miles, year, make
## `glm' is generalized linear model; this is your least-squares regression
fit_maineffects <- glm(log(price) ~ make + miles + year, data=trucks) # ~. means `on everything'
## summarize the regression model
summary(fit_maineffects) 
## what is the 95% CI for effect of miles on log price?
## why are there only two brand effects?

## interaction: allow mileage depreciation to depend upon make.
## in glm formulas, '+' means 'and', '*' means 'interacted with'
fit_interact <- glm(log(price) ~ year + miles*make, data=trucks) 
summary(fit_interact)
## what is the mileage depreciation rate for each of Dodge, Ford, and GMC?




