
# person_id  is key
# treatment is in Description file, and is random conditional on the numhh_list (number of names in lottery)
# in 2008 new spots opened for medicaid, which was previously closed to new enroll
# we are interested in health insurance effect on increased costs and utilization (on health is longer term)
# admin data is clean, survey data no necessarily balanced due to non-response bias
# admin data has hospital admission (by dept, emerg itself is non-signif)
# we can also look at number of hostpital days or total list cost

library(foreign)

descr <- read.dta("OHIE_Public_Use_Files/OHIE_Data/oregonhie_descriptive_vars.dta")
prgm <- read.dta("OHIE_Public_Use_Files/OHIE_Data/oregonhie_stateprograms_vars.dta")
s12 <- read.dta("OHIE_Public_Use_Files/OHIE_Data/oregonhie_survey12m_vars.dta")

# nicely organized, one row per person
all(s12$person_id == descr$person_id)
all(s12$person_id == prgm$person_id)

P <- descr[,c("person_id","household_id", "numhh_list")]
P$medicaid <- as.numeric(prgm[,"ohp_all_ever_firstn_30sep2009"]=="Enrolled")
P$selected <- as.numeric(descr[,"treatment"]=="Selected")
levels(P$numhh_list) <- c("1","2","3+")

# 12 month is the survey that really matters
# need to control for household size interacted with survey return time
Y <- s12[,c("weight_12m",
	"doc_any_12m","doc_num_mod_12m",
	"er_any_12m","er_num_mod_12m",
	"hosp_any_12m","hosp_num_mod_12m")]
Y$doc_any_12m <- as.numeric(Y$doc_any_12m=="Yes")
Y$er_any_12m <- as.numeric(Y$er_any_12m=="Yes")
Y$hosp_any_12m <- as.numeric(Y$hosp_any_12m=="Yes")

# smk_ever_12m - num19_12m are sources of heterogeneity, plus descr
X <- s12[,121:147]
X$dt_returned <- factor(format(s12$dt_returned_12m, "%Y-%m"))

insurv <- which(s12$sample_12m_resp == "12m mail survey responder")
X <- X[insurv,]
Y <- Y[insurv,]
P <- P[insurv,]

sapply(Y,function(y) sum(is.na(y)))
nomiss <- which( !apply(Y,1, function(y) any(is.na(y))) )
X <- X[nomiss,]
Y <- Y[nomiss,]
P <- P[nomiss,]

# pull out the weights and attach doc_any to P
weights <- Y[,1]
Y <- Y[,-1]

# replace some ridiculous values in survey and drop num19
X$hhsize_12m[X$hhsize_12m>10] <- 10
X$num19_12m <- NULL

# organize to make it pretty for text
P$doc_any_12m <- Y$doc_any_12m # you can explore other responses if you want
P <- P[,c(1,2,6,5,4,3)]
names(P)[6] <- "numhh"

################## basic diffs in mean

head(P)
dim(P)
table(P$selected)

ybar <- tapply(P$doc_any_12m, P$selected, mean)
( ATE = ybar['1'] - ybar['0'] )

nsel <- table(P[,c("selected")])
yvar <- tapply(P$doc_any_12m, P$selected, var)
( seATE = sqrt(sum(yvar/nsel)) )

ATE + c(-2,2)*seATE

nsel_w <- tapply(weights, P$selected, sum)
ybar_w <- tapply(weights*P$doc_any_12m, P$selected, sum)/nsel_w
( ATEweighted <-  ybar_w['1'] - ybar_w['0'] )

## unweighted analysis for all response options
ybar <- apply( Y, 2, function(y) tapply(y, P$selected, mean))
nsel <- table(P[,c("selected")])
yvar <- apply( Y, 2, function(y) tapply(y, P$selected, var) )

print( gam <- ybar[2,]-ybar[1,] )

gsd <- sqrt(colSums(yvar/as.numeric(nsel)))
print( gam/gsd )

# no big difference if you use CLT on number of households r.t. people
nselhh <- tapply(P$household_id, P$selected, function(h) length(unique(h)))
gsdhh <- sqrt(colSums(yvar/as.numeric(nselhh)))
print( gam/gsdhh )

###### consider the imbalance in numhh
table(P$selected)
table(P[,c("selected","numhh")])

###################  reg for doc_any


# original paper also controls for dt returned, 
# but it has no practical effect
lin <- glm(doc_any_12m ~ selected + numhh, data=P)
round( summary(lin)$coef["selected",],4) # 6-7% increase in prob

x <- scale( model.matrix( ~ numhh, data=P)[,-1], scale=FALSE)
colMeans(x)
linadj <- glm(doc_any_12m ~ selected*x, data=P)
summary(linadj)

# build the household effects
yhh <- tapply(P$doc_any_12m, P$household_id, mean)
zebra <- match(names(yhh), P$household_id) # 1st occurence of each HH in P
selectedhh <- P$selected[zebra]
xhh <- x[zebra,]
summary(glm(yhh ~ selectedhh*xhh))

# bootstrap
library(boot)
n <- nrow(P)
hhwho <- split(1:n, P$household_id) # rows grouped by HH
bootfit <- function(hhlist, boothh) {
	bootsamp <- unlist(hhwho[boothh])   # map from HH sample to rows
   	coef(glm(doc_any_12m ~ selected*x, data = P, subset=bootsamp))[2]
}
bs <- boot(names(hhwho), bootfit, 999)
sd(bs$t)
quantile(bs$t, c(.05,.95))

# clustered SEs
library(AER)
sqrt(vcovCL(linadj, cluster = P$household_id)[2,2])

# logistic regression

lgt <- glm(doc_any_12m ~ selected*numhh, data=P, family="binomial")

predlocs <- data.frame(selected=c(1,1,1,0,0,0), 
	numhh=c('1','2','3+','1','2','3+'))
predy <- predict(lgt, newdata=predlocs, type='response')
( pdiff <- predy[1:3] - predy[4:6] )

( mu_numhh <- table(P$numhh)/nrow(P) )
pdiff%*%mu_numhh

# bootstrap 
bootfit_lgt <- function(hhlist, boothh) {
	bootsamp <- unlist(hhwho[boothh])   # map from HH sample to rows
   	lgt <- glm(doc_any_12m ~ selected*numhh, data = P, subset=bootsamp, family="binomial")
   	predy <- predict(lgt, newdata=predlocs, type='response')
	mu_numhh%*%(predy[1:3] - predy[4:6])
}
bslgt <- boot(names(hhwho), bootfit_lgt, 99)
sd(bslgt$t)
quantile(bslgt$t, c(.05,.95))

## two stage least squares (Instrumental Variables)
stage1 <- lm( medicaid ~ selected + numhh, data=P)
phat <- predict(stage1, newdata=P)
stage2 <- lm( doc_any_12m ~ phat + numhh, data=P, x=TRUE)
coef(stage2)

library(Matrix)
resids <- P$doc_any_12m - predict( stage2, 
	newdata=data.frame(numhh=P$numhh, phat=P$medicaid))
meat <- Diagonal(x=resids^2)
bread <- stage2$x%*%solve(t(stage2$x)%*%stage2$x)
sandwich <- t(bread)%*%meat%*%bread
print( segam <- sqrt(sandwich[2,2]) )


coef(stage2)["phat"] + c(-2,2)*segam

# or, just use the packages
library(AER)
aeriv <- ivreg( doc_any_12m  ~ medicaid + numhh | selected + numhh, data=P)

summary(aeriv)
sqrt( (solve(t(stage2$x)%*%stage2$x)*var(resids))[2,2] )

sqrt(vcovHC(aeriv)[2,2])
segam

sqrt(vcovCL(aeriv, cluster = P$household_id)[2,2])

## heterogeneity via causal trees (Nonparametric chapter)
install.packages("devtools")
library(devtools) 
install_github("susanathey/causalTree")

library(causalTree)
selectfit <- glm(selected ~ numhh, data = P, family=binomial)
propensity <- predict(selectfit, type="response")

ybar <- tapply(P$doc_any_12m, P$selected, mean)
ybar['1'] - ybar['0']

# remove the hhinc_cat variable to get a nicer looking tree
ct <- causalTree( P$doc_any_12m ~ ., data=X[,-10], 
	treatment=P$selected,  weights=1/propensity, minsize=2000,
	split.Rule = "CT", cv.option="CT", split.Honest=TRUE)
rpart.plot(ct)

### linear HTE estimation (Controls chapter)
library(gamlr)
source("naref.R")

## dealing with missingness
# make NA the reference level for all categories
X <- naref(X)

# pull out the numeric variables 
xnum <- X[,sapply(X,class)%in%c("numeric","integer")]
xnum[66:70,]
colSums(is.na(xnum))
# flag missing
xnumna <- apply(is.na(xnum), 2, as.numeric)
xnumna[66:70,]
# impute the missing values
mzimpute <- function(v){ 
	if(mean(v==0,na.rm=TRUE) > 0.5) impt <- 0
	else impt <- mean(v, na.rm=TRUE)
	v[is.na(v)] <- impt
	return(v) }
xnum <- apply(xnum, 2,  mzimpute)
xnum[66:70,]

# replace/add the variables in new data frame 
for(v in colnames(xnum)){
	X[,v] <- xnum[,v]
	X[,paste(v,"NA", sep=".")] <- xnumna[,v] }
X[144:147,]

xhte <- sparse.model.matrix(~., data=cbind(numhh=P$numhh, X))[,-1]
xhte[1:2,1:4]
dim(xhte)


dxhte <- P$selected*xhte
colnames(dxhte) <- paste("d",colnames(xhte), sep=".")
htedesign <- cBind(xhte,d=P$selected,dxhte)
# include the numhh controls and baseline treatment without penalty 
htefit <- gamlr(x=htedesign, y=P$doc_any_12m, free=c("numhh2","numhh3+","d"))
gam <- coef(htefit)[-(1:(ncol(xhte)+1)), ]
round(sort(gam)[1:6],4)
round(sort(gam, decreasing=TRUE)[1:6],4)

gam["d"] + colMeans(xhte)%*%gam[grep("d.", names(gam))]