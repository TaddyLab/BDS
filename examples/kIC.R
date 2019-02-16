
## get AICc and BIC for the output of kmeans
kIC <- function(fit, rule=c("A","B")){
	df <- length(fit$centers) # K*dim
	n <- sum(fit$size)
	D <- fit$tot.withinss
	rule=match.arg(rule)
	if(rule=="A")
		return(D + 2*df*n/max(n-df-1,1))
	else
		return(D + log(n)*df)
}
