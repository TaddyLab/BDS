library(Synth)
library(tidyr)
data(basque)

## synthetic controls analysis
y <- basque[,1:4] %>% spread(year, gdpcap)
rownames(y) <- y$regionname
y <- y[c(17,2:16,18), -(1:2)]
y <- y[,1:35]

# untreated years are through 1968
library(gamlr)

#Create function to perform synthetic controls
synthc <- function(j, tyear=1968, ...){
  #Pull the untreated values into a separate matrix (Step 1)
  y0t <- t(y[,1:(tyear-1954)])
  #Fit a regression for the untreated values (Step 1)
  fit <- gamlr( y0t[,-j], y0t[,j], lambda.min.ratio=1e-4, alpha=1)
  #Create a plot of the coefficient paths
  plot(fit, xvar="lambda")
  
  #Make predictions for untreated values for time periods after T (Step 2)
  y0hat <- predict(fit, t(y[-j,]))[,1]
  
  return(list(w=coef(fit)[,1], y0hat=y0hat ) )
}

# run the synthetic controls
sc <- synthc(1, lmr=1e-4)
sc$w

##Plot the synthetic (no terrorism GDP) versus the observed GDP
year <- as.numeric(colnames(y))

#Plot synthetic GDP
plot(year, sc$y0hat, type="l", ylab="gdp per capita",
     col=rgb(.1,.5,1,0.8), ylim=range(c(y[1,],sc$y0hat)), bty="n", lwd=2)
#Mark on plot when the treatment (terrorism occured)
abline(v=1968, col=8, lty=2)

#Plot the observed GDP
lines(year, y[1,], col=rgb(1,.5,0,.8), lwd=2)

#Add legend to the plot
legend("topleft", bty="n", legend=c("observed basque","synthetic basque"), 
       lwd=2, col=c(col=rgb(1,.5,0,.8),rgb(.1,.5,1,0.8)) )

#### permutation Testing ####

## To test the significance in differences between synthetic and observed
## as viewed in the plot above, we can utilize permutation testing.
## We can compare your estimated tretment effects to results obtained using
## the same methods on placebo units (regions where we know no treatment ocurred).

#Import parallel library
library(parallel)

#detect number of available computer cores
cl <- makeCluster(detectCores())

#Make cluster using the available cores
clusterExport(cl, c("y", "gamlr", "synthc"))

#Define the gety0 as synthc as a function of j, the number of regions
gety0 <- function(j){ synthc(j, lmr=1e-4)$y0hat }

#Generate a synthetic for each observed region in teh basque file
Ysynth <- parSapply(cl, 1:nrow(y), gety0)

#Store the resulting differences between synthetic and observed in a matrix
diff <- Ysynth - t(y)

##Plot differences in synthetic and observed for regions in Spain

#Plot the placebos
matplot(year, diff, type="l", lwd=1.5, 
	xlab="year", ylab="synthetic - observed",
 	col=8, lty=1, bty="n")

#Highlight synthetic for Basque
lines(year, diff[,1], lwd=1.5, col="red")

#Highlight synthetic for Madrid
lines(year, diff[,14], lwd=1.5, lty=2, col=1)

#Add a legend to the plot
legend("topleft", bty="n", 
	legend=c("basque", "placebo", "(madrid)"), 
	lty=c(1,1,2), lwd=2, col=c(2,8,1))
