
## your first non-standard R package!
# data.table is useful for dealing with large datasets
# you can install with install.packages("data.table")
library(data.table)

# fread is faster than read.table
system.time(lipids <-  fread("jointGwasMc_LDL.txt"))

# we'll then convert back to the usual R 'data.frame'.
# data.tables have other nice capabilities that we'll see later in class
lipids <- as.data.frame(lipids)

# pull out p-values and label them
pvals <- lipids[,'P-value']
names(pvals) <- lipids[,'rsid']

# plot the p-value distribution; notice the tiny spike near zero
hist(pvals, main='', xlab='p-values',freq=FALSE) # freq=FALSE gives you a density plot

# top 10 locations to investigate
names(pvals)[order(pvals)[1:10]] 

## function to get significance cut-off alpha from FDR q
# I also put a version of this in the fdr.R script for you
fdr_cut <- function(pvals, q){
  pvals <- pvals[!is.na(pvals)]
  N <- length(pvals)
  
  k <- rank(pvals, ties.method="min")
  alpha <- max(pvals[ pvals<= (q*k/N) ])
  
  return(alpha)
}

## find the cut
# @ 10% FDR
cutoff10 <- fdr_cut(pvals,q=.1)
print(cutoff10)
print(sum(pvals<=cutoff10))
# @ 1% FDR
cutoff1 <- fdr_cut(pvals,q=.01)
print(cutoff1)
print(sum(pvals<=cutoff1))
# @ 1/10% FDR
cutoff01 <- fdr_cut(pvals,q=.001)
print(cutoff01)
print(sum(pvals<=cutoff01))
# so you get 4000 discoveries, only 4-5 of which you expect to be false

## visualize the B+H FDR algorithm 
# warning: the plot can take a bit of time/memory
sig <- factor(pvals<=cutoff01)
o <- order(pvals)
N <- length(pvals)
plot(pvals[o], log="xy", col=c("grey60","red")[sig[o]], pch=20, 
     ylab="p-values", xlab="tests ordered by p-value", main = 'FDR = 0.1%')
lines(1:N, 0.01*(1:N)/N)

