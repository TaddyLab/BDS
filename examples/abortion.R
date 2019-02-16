####### donohue and levitt 2001/2008: abortion and crime

## example reading non csv data: this is a dump from STATA
## skip says skip the first line of the file, sep="/t" says 'tab separated'
data <- read.table("abortion.dat", skip=1, sep="\t")
names(data) <- c("state","year","pop","y_viol","y_prop","y_murd",
	"a_murd","a_viol","a_prop",'prison','police',
	'ur','inc','pov','afdc','gun','beer')

## prison: log of lagged prisoners per capita
## police: the log of lagged police per capita
## ur: the unemployment rate
## inc: per-capita income
## pov: the poerty rate
## AFDC: generosity at year t-15
## gun: dummy for concealed weapons law
## beer: beer consumption per capita 

data <- data[!(data$state%in%c(2,9,12)),] # AK, DC, HA are strange places
data <- data[data$year>84 & data$year<98,] # incomplete data outside these years
data$pop <- log(data$pop)
t <- data$year - 85
s <- factor(data$state) ## the states are numbered alphabetically

controls <- data.frame(data[,c(3,10:17)])
## y is de-trended log crime rate, a is as described below
## note we also have violent and property crime versions
y <- data$y_murd
d <- data$a_murd

## The abortion 'a_' variables are weighted average of abortion rates where
## weights are determined by the fraction of the type of crime committed by
## various age groups. For example, if 60% of violent crime were committed by 18
## year olds and 40% were committed by 19 year olds in state i, the abortion rate
## for violent crime at time t in state i would be constructed as .6 times the
## abortion rate in state i at time t − 18 plus .4 times the abortion rate in
## state i at time t − 19. See Donohue and Levitt (2001) for further detail.

## we'll just look at murder
## note for convenience here I've made y,d,t, global: they are not in controls.
summary(orig <- glm(y ~ d + t + s +., data=controls) )$coef['d',]
## this is the levitt analysis: higher abortion leads to lower crime

# That abortion is only one factor inuencing crime in the late 1980s points
# out the caution required in drawing any conclusions regarding an abortion-crime
# link based on time series evidence alone.

## Now the same analysis, but for cellphones rather than abortion
cell <- read.csv("us_cellphone.csv")
# center on 1985 and scale by 1997-1985
cellrate <- 5*cell[,2]/(1000*cell[,3]) 
## what if we're just fitting a quadratic trend?
## there are many things that increased with similar shapes over time
## (cellphone usage, yoga revenues, home prices, ...)

par(mai=c(.9,.9,.1,.1))
plot(1985:1997, tapply(d, t, mean), bty="n", xlab="year", ylab="rate", pch=21, bg=2)
points(1985:1997, cellrate, bg=4, pch=21)
legend("topleft", fill=c(2,4), legend=c("abortions","cellphones"), bty="n")


phone <- cellrate[t+1]
## clearly, cellphones fight crime.
summary(tech <- glm(y ~ phone + s + t+., data=controls))$coef['phone',]
1 - exp(-0.372e-01)

## what is happening here is that murder has been increasing quadratically,
## and we have no other controls that do so.  To be correct, you need
## to allow quadratic trends that could be caused by other confounding variables (e.g. technology)
## we also allow interaction between the controls, and interact the nation-wide phone
## variable with state dummies to allow for state specific tech adoption.
t <- factor(t)
interact <- glm(y ~ d + t + phone*s + .^2, data=controls)
summary(interact)$coef["d",]
## Abortion sign has switched direction (and is insignif)!
dim(model.matrix(formula(interact), data=controls))
## we have very few observations relative to number of parameters.

## so we need a way to select only important controls
## try using a lasso 
library(gamlr)
## refactor state to have NA reference level
sna <- factor(s, levels=c(NA,levels(s)), exclude=NULL)
x = sparse.model.matrix( ~ t + phone*sna + .^2, data=controls)[,-1]
dim(x)

## naive lasso regression
naive <- cv.gamlr(cBind(d,x),y)
coef(naive)["d",] # effect is CV selected <0

## now, what if we explicitly include dhat confounding:
treat <- cv.gamlr(x,d, lmr=1e-3)

# Now, grab the predicted treatment
# type="response" is redundant here (gaussian), 
# but you'd want it if d was binary
dhat <- drop( predict(treat, x, select="min") )

## not much signal in d not predicted by dhat
par(mai=c(.9,.9,.1,.1))
plot(dhat,d,bty="n",pch=21,bg=8, cex=.8, yaxt="n")
axis(2, at=c(0,1,2,3)) 
## that means we have little to resemble an experiment here...

## IS R^2?
cor(drop(dhat),d)^2
## Note: IS R2 is what governs how much independent signal
## you have for estimating 

summary( glm( y ~ I(d-dhat) ) )
summary( glm( y ~ d + dhat) )

# re-run lasso, with this (2nd column) included unpenalized
causal <- cv.gamlr(cBind(d,dhat,x),y,free=2,lmr=1e-3)
coef(causal, select="min")["d",] # AICc says abortion has no causal effect.

#######################
# orthogonal ML
source("orthoML.R")
dreg <- function(x,d){ cv.gamlr(x, d, lmr=1e-5) }

yreg <- function(x,y){ cv.gamlr(x, y, lmr=1e-5) }

resids <- orthoPLTE( x=x, d=d, y=y, 
				dreg=dreg, yreg=yreg, nfold=5)

