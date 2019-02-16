#### Info retrieval

## the tm library (and related plugins) is R's ecosystem for text mining.
## for an intro see http://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
library(tm) 

## the way file input works with tm is you create a reader function,
## depending on document type.  Each of the reader functions
## have arguments elem, language, id (see ?readPlain,?readPDF,etc)
## I wrap another function around them to specify these arguments.

## for example, a reader to input plain text files 
## (Note: there are many other ways to do this)
readerPlain <- function(fname){
				readPlain(elem=list(content=readLines(fname)), 
							id=fname, language='en') }
## test it on this script
## (the file name will change depending on where you store stuff).
rcode <- readerPlain("text.R")
rcode # this is the tm 'PlainTextDocument'
content(rcode)[1:21] # this is the actual text part

## *** Reading PDFs ***

## from the tm docs: "Note that this PDF reader needs the 
## tool pdftotext installed and accessible on your system,
## available as command line utility in the Poppler PDF
## rendering library (see http://poppler.freedesktop.org/)."
## this appears to be the default on windows

## we'll create a 'reader' function to interpret pdfs, 
## using tm's readPDF (see help(readPDF) examples)

readerPDF <- function(fname){
		txt <- readPDF(control = list(text = "-layout -enc UTF-8"))(elem=list(uri=fname), 
															id=fname, language='en')
		return(txt)
	}

## for the following to work, in your working directory
## you'll need to point to wherever you've stored the lectures

## apply to all the lectures (assuming you are running from MBAmaterials/examples)
files <- Sys.glob("../lectures/*.pdf") 
# Sys.glob just expands file names from 'wildcards'
## takes time!  this would be easy to do 
## distributed via clusterApply or MapReduce
notes <- lapply(files, readerPDF) 
## some string manipulation to get nice names
names(notes) = sub('.pdf', '', substring(files,first=37))
names(notes)
writeLines(content(notes[[1]])[1]) # the cover slide

for(i in 1:11) 
 content(notes[[i]]) <- 
 	iconv(content(notes[[i]]), from="UTF-8", to="ASCII", sub="")

## once you have a bunch of docs in a vector, you 
## create a text mining 'corpus' with: 
docs <- Corpus(VectorSource(notes))

names(docs) <- names(notes) # no idea why this doesn't just happen
## you can then do some cleaning here
## tm_map just maps some function to every document in the corpus
docs <- tm_map(docs, content_transformer(tolower)) ## make everything lowercase
docs <- tm_map(docs, content_transformer(removeNumbers)) ## remove numbers
docs <- tm_map(docs, content_transformer(removePunctuation)) ## remove punctuation
## remove stopword.  be careful with this: one's stopwords are anothers keywords.
docs <- tm_map(docs, content_transformer(removeWords), stopwords("SMART"))
# you could also do stemming; I don't bother here.
docs <- tm_map(docs, content_transformer(stripWhitespace)) ## remove excess white-space

## create a doc-term-matrix
dtm <- DocumentTermMatrix(docs)
dtm # 11 documents, > 4K terms

## Finally, drop those terms that only occur in one or two lectures
## This is a common step: you the noise of rare terms to overwhelm things,
##					and there is nothing to learn if a term occured once.
## Below removes those terms that have count 0 in >75% of docs.  
## this is way more harsh than you'd usually do (but we only have 11 docs here)
## .75*11 is 8.25, so this will remove those with zeros in 9+ docs.
## ie, it removes anything that doesn't occur in at least 3 docs
dtm <- removeSparseTerms(dtm, 0.75)
dtm # now near 700 terms


## These are special sparse matrices.  
class(dtm)
## You can inspect them:
inspect(dtm[1:5,1:8])
## find words with greater than a min count
findFreqTerms(dtm,100)
## or grab words whose count correlates with given words
findAssocs(dtm, "lasso", .9) 

## consider of PCA on term frequencies.
## note that converting to a dense matrix would be infeasible for big corpora
## see the 'irlba' package for PCA on the sparse Matrices we've used with glmnet.
X <- as.matrix(dtm)
F <- X/rowSums(X) ## divide by row (doc totals)
classpca <- prcomp(F, scale=TRUE)
plot(classpca) 

## look at the big rotations... all rare words linked to our examples
classpca$rotation[order(abs(classpca$rotation[,1]),decreasing=TRUE),1][1:10]
classpca$rotation[order(abs(classpca$rotation[,2]),decreasing=TRUE),2][1:10]

## Plot the first two PCs..
plot(classpca$x[,1:2], col=0, xlab="PCA 1 direction", ylab="PCA 2 direction", bty="n")
text(x=classpca$x[,1], y=classpca$x[,2], labels=rownames(dtm))
## PC1 is low for core material, high for special topics
## PC2 looks like supervised vs unsupervised?

## **** a quick topic-modelling example **** ##
library(maptpx) ## you can give topics a few K, and it chooses the best by BIC
tpc <- topics(dtm, K=2:10) # it chooses 2 topics only!  this is simple class ;-)
# If you follow through with the 2 topic model below, it'll tell you that 
# class one was a single topic and every other class was another.
# I think a 3 topic model is more interesting here.
tpc <- topics(dtm,K=3)

## summary prints terms by 'lift': p(term|topic)/p(term)
summary(tpc, 10) #10 is number of top terms to print
# not very informative here; 
# looks like every rare term has high lift in one of only 2 topics.

## the topic-term probabilities ('theta'); each column is a topic
## we can use these to rank terms by probability within topics
rownames(tpc$theta)[order(tpc$theta[,1], decreasing=TRUE)[1:10]]
rownames(tpc$theta)[order(tpc$theta[,2], decreasing=TRUE)[1:10]]
## in this case, the probs rather than words seem more informative
## topic 1 looks like regression modelling, topic 2 is all else on data.

## plot the lectures another way (do them in order)
whichtopic <- 1 # change this to see which classes are in each
par(srt=-30, xpd=NA) ## rotate stings, and allow words outside plot
plot(tpc$omega[,whichtopic], type="l", col=8, xlab="", xlim=c(0.5,12),
	xaxt="n", ylab=sprintf("topic %d weight",whichtopic), bty="n")
text(x=1:nrow(tpc$omega), y=tpc$omega[,whichtopic], labels=rownames(dtm))

## congress 109 counts
library(textir)
data(congress109)

f <- congress109Counts#t( t(congress109Counts)/rowSums(congress109Counts) )
y <- congress109Ideology$repshare
slant <- pls(f, y, K=3)

# pictures
for(k in 1:3)
	plot(slant$y, slant$fitted[,k], ylim=c(-.05,.85), xlab="", ylab="",
	 main=sprintf("PLS(%d)", k), 
   		pch=20, col=c(4,3,2)[congress109Ideology$party], bty="n")
mtext(side=1, "repshare", outer=TRUE, line=-1.25)
mtext(side=2, "fitted", outer=TRUE, line=-1.25)

# OOS experiment
foldid <- sample.int(5, nrow(f), replace=TRUE)
OOS <- matrix(nrow=5, ncol=10)
for(b in 1:5){
	print(b)
	fb <- f[,colSums(f[foldid!=b,])!=0]
	for(k in 1:10){
		gpls <- pls(x=fb[foldid!=b,], y=y[foldid!=b], K=k)
		OOS[b,k] <- 
			mean( (y[foldid==b] - predict(gpls, fb[foldid==b,], K=k))^2 )
	}
}
cvm <- apply(OOS,2,mean)
cvs <- apply(OOS,2,sd)
OOS <- as.data.frame(OOS)
names(OOS) <- 1:10

# lasso instead of PLS
lassoslant <- cv.gamlr(congress109Counts>0, y)
B <- coef(lassoslant$gamlr)[-1,]
sort(round(B[B!=0],4))

# compare
boxplot(OOS, ylab="mean squared error", xlab="K", col="red", log="y", main="", 
	ylim=c(0.01,2))
mtext(side=3, "PLS", line=2)
plot(lassoslant, log="y", main="", ylim=c(0.01,2)) 
mtext(side=3, "lasso", line=2)

#### we8there
library(textir)
data(we8there)
x <- we8thereCounts
pca <- prcomp(x, scale=TRUE) # can take a long time
v <- predict(pca)[,1:4]

par(mai=c(.8,.8,.1,.1))
boxplot(v[,1] ~ we8thereRatings$Overall, xlab="overall rating", ylab="PC1 score")

library(maptpx) # for the topics function


## you need to convert from a Matrix to a `slam' simple_triplet_matrix
## luckily, this is easy.
x <- as.simple_triplet_matrix(we8thereCounts)

# to fit, just give it the counts, number of `topics' K, and any other args
tpc <- topics(x,K=10) 

## choosing the number of topics
## If you supply a vector of topic sizes, it uses a Bayes factor to choose
## (BF is like exp(-BIC), so you choose the bigggest BF)
## the algo stops if BF drops twice in a row
tpcs <- topics(x,K=5*(1:5), verb=1) # it chooses 10 topics 

## interpretation
# summary prints the top `n' words for each topic,
# under ordering by `topic over aggregate' lift:
#    the topic word prob over marginal word prob.
summary(tpcs, n=10) 
# this will promote rare words that with high in-topic prob

# alternatively, you can look at words ordered by simple in-topic prob
## the topic-term probability matrix is called 'theta', 
## and each column is a topic
## we can use these to rank terms by probability within topics
rownames(tpcs$theta)[order(tpcs$theta[,1], decreasing=TRUE)[1:10]]
rownames(tpcs$theta)[order(tpcs$theta[,2], decreasing=TRUE)[1:10]]

boxplot(tpcs$omega[,1] ~ we8thereRatings$Overall, col="gold", xlab="overall rating", ylab="topic 1 score")
boxplot(tpcs$omega[,2] ~ we8thereRatings$Overall, col="pink", xlab="overall rating", ylab="topic 2 score")

## interpret the relationship between topics and overall rating
library(gamlr)
## omega is the n x K matrix of document topic weights
## i.e., how much of each doc is from each topic
## we'll regress overall rating onto it
stars <- we8thereRatings[,"Overall"]
tpcreg <- gamlr(tpcs$omega, stars, lmr=1e-3)
# number of stars up or down for moving up 10\% weight in that topic
drop(coef(tpcreg))*0.1

regtopics.cv <- cv.gamlr(tpcs$omega, stars, lmr=1e-3)
regwords.cv <- cv.gamlr(we8thereCounts, stars)


par(mfrow=c(1,2), mai=c(.3,.6,.7,.1), omi=c(.5,.2,0,0))
plot(regtopics.cv, ylim=c(1,2), xlab="", ylab="")
mtext("topic regression", font=2, line=2)
plot(regwords.cv, ylim=c(1,2), xlab="", ylab="")
mtext("token regression", font=2, line=2)
mtext(side=2, "mean squared error", outer=TRUE, line=0)
mtext(side=1, "log lamba", outer=TRUE, line=1)



# max OOS R^2s
max(1-regtopics.cv$cvm/regtopics.cv$cvm[1])
max(1-regwords.cv$cvm/regwords.cv$cvm[1])


## Multinomial text Regression 

## cl=NULL instead implies a serial run. 
cl <- makeCluster(detectCores())
## small nlambda for a fast example
fits <- dmr(cl, we8thereRatings, 
			we8thereCounts, bins=5,nlambda=10, lmr=1e-3)
stopCluster(cl) # usually a good idea

## plot fits for a few individual terms
terms <- c("first date","chicken wing",
			"ate here", "good food",
			"food fabul","terribl servic")
par(mfrow=c(2,3))
for(j in terms)
{ 	plot(fits[[j]]); mtext(j,font=2,line=2) }
 
## extract coefficients
B <- coef(fits)
mean(B[-1,]==0) # sparsity in loadings
## some big loadings on `overall'
B[2,order(B[2,])[1:10]]
B[2,order(-B[2,])[1:10]]

## do MNIR projection onto factors
z <- srproj(B,we8thereCounts) 

## fit a fwd model to the factors
summary(fwd <- lm(we8thereRatings$Overall ~ z)) 

## truncate the fwd predictions to our known range
fwd$fitted[fwd$fitted<1] <- 1
fwd$fitted[fwd$fitted>5] <- 5
## plot the fitted rating by true rating
par(mfrow=c(1,1))
plot(fwd$fitted ~ factor(we8thereRatings$Overall), 
	varwidth=TRUE, col="lightslategrey")










