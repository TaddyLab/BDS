# orthoML utility function
require(AER)
require(gamlr)

orthoPLTE <- function(x, d, y, dreg, yreg, nfold=2)
{
	# randomly split data into folds
	nobs <- nrow(x)
    foldid <- rep.int(1:nfold, 
    	times = ceiling(nobs/nfold))[sample.int(nobs)]
    I <- split(1:nobs, foldid)
    # create residualized objects to fill
	ytil <- dtil <- rep(NA, nobs)
	# run the OOS orthogonalizations
	cat("fold: ")
	for(b in 1:length(I)){
		dfit <- dreg(x[-I[[b]],], d[-I[[b]]])
		yfit <- yreg(x[-I[[b]],], y[-I[[b]]])
		dhat <- predict(dfit, x[I[[b]],], type="response")
		yhat <- predict(yfit, x[I[[b]],], type="response")
		dtil[I[[b]]] <- drop(d[I[[b]]] - dhat)
		ytil[I[[b]]] <- drop(y[I[[b]]] - yhat)
		cat(b," ")
	}
	rfit <- lm(ytil ~ dtil)
	gam <- coef(rfit)[2]
	se <- sqrt(vcovHC(rfit)[2,2])
	cat(sprintf("\ngamma (se) = %g (%g)\n", gam, se))

	return( list(gam=gam, se=se, dtil=dtil, ytil=ytil) )
}

