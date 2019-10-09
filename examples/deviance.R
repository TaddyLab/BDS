## deviance calculations

## pred must be probabilities (0<pred<1) for binomial
deviance <- function(y, pred, family=c("gaussian","binomial")){
	family <- match.arg(family)
	if(family=="gaussian"){
		return( sum( (y-pred)^2 ) )
	}else{
		if(is.factor(y)) y <- as.numeric(y)>1
		return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
	}
}

## get null devaince too, and return R2
R2 <- function(y, pred, family=c("gaussian","binomial")){
	fam <- match.arg(family)
	if(fam=="binomial"){
		if(is.factor(y)){ y <- as.numeric(y)>1 }
	}
	dev <- deviance(y, pred, family=fam)
	dev0 <- deviance(y, mean(y), family=fam)
	return(1-dev/dev0)
}
