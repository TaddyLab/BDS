## naref: make missing (NA) the reference level of a factor
xnaref <- function(x){
	if(is.factor(x))
		if(!is.na(levels(x)[1]))
			x <- factor(x,levels=c(NA,levels(x)),exclude=NULL)
	return(x) }

naref <- function(DF){
	if(is.null(dim(DF))) return(xnaref(DF))
	if(!is.data.frame(DF)) 
		stop("You need to give me a data.frame or a factor")
	DF <- lapply(DF, xnaref)
	return(as.data.frame(DF))
}


n <- 1000
p <- 3
xvar <- matrix(0.9, nrow=p,ncol=p)
diag(xvar) <- 1
x <- matrix(rnorm(p*n), nrow=n)%*%chol(xvar)
y <- 4 + 3*x[,1] + -1*x[,2] + rnorm(n)

## run models to extra small lambda 1e-3xlambda.start
fitlasso <- gamlr(x, y, gamma=0, lambda.min.ratio=1e-4) # lasso
