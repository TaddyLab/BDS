######################################
## Logistic regression: Spam data
######################################

email <- read.csv("spam.csv")

## fit the full model
spammy <- glm(spam ~ ., data=email, family='binomial')
## you don't need to worry about this warning.  
## It says that some covariates are nearly perfect predictors.

## the guy is named george and he works in a cs dept
table(email$spam, email$word_george)
table(email$spam, email$word_free)

## the coefficients
b <- coef(spammy)
exp(b["word_george"]) # George => !SPAM
exp(b["word_free"]) # Free => SPAM

# fit plot
plot(spammy$fit~email$spam, 
	xlab="", ylab=c("fitted probability of spam"), 
	col=c("navy","red"))

# predict spam v not for first 2 obsv
predict(spammy, newdata=email[c(1,4000),])
predict(spammy, newdata=email[c(1,4000),], type="response")

# OOS prediction
leaveout <- sample(1:nrow(email), 1000) ## sample 1000 random indices
# train the model WITHOUT these observations (-index removes those obs)
spamtrain <- glm(spam ~ ., data=email[-leaveout,], family='binomial')
# get the predicted probability of spam on the left out data
pspam <- predict(spamtrain, newdata=email[leaveout,], type="response")
# plot the OOS fit
plot(pspam ~ email$spam[leaveout],
	xlab="", ylab=c("predicted probability of spam"), 
	col=c("navy","red"))

## check out the deviance function for calculating
## mse (family="gaussian") and binomial deviance (family="binomial") 
source("deviance.R")
D <- deviance(y=email$spam[leaveout], pred=pspam, family="binomial")
## for null deviance, our pred is ybar: the mean for spam
ybar <- mean(email$spam[leaveout]=="spam") # marginal prob(spam)
D0 <- deviance(y=email$spam[leaveout], pred=ybar, family="binomial")
## OOS R2
1 - D/D0  
## compare to spamtrain
summary(spamtrain) # will usually be a higher in-sample R2


