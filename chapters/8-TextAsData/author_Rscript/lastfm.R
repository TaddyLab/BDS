####################  Association Rules #####################

### *** LastFM play counts *** ###
lastfm <- read.csv("lastfm.csv")
library(maptpx)
lastfm$user <- factor(lastfm$user)

x <- simple_triplet_matrix(i=as.numeric(lastfm$user), 
		j=as.numeric(lastfm$artist), v=rep(1,nrow(lastfm)), 
		nrow = nlevels(lastfm$user), ncol = nlevels(lastfm$artist),
        dimnames = list(levels(lastfm$user), levels(lastfm$artist)))

tpcs <- topics(x, K=5*(1:5), verb=1)

## Use the a-rules package for association rules
library(arules)
## there is an entire ecosystem of packages around arules
## you need to first create a list of baskets: vectors of items by consumer

## Here's how we do the formatting here:
## split data into a list of artists for each user
playlists <- split(x=lastfm$artist, f=lastfm$user)
## re-move artist repetition in these lists
playlists <- lapply(playlists, unique)
## tell R to treat this as a special arules `transactions' class.
playtrans <- as(playlists, "transactions")

## now apply the actual 'apriori' algorithm
# you can add a list of arguments called 'parameter'.  Here, we look at
# only rules with support > .01 & confidence >.5 & length (# artists) <= 3
musicrules <- apriori(playtrans, 
	parameter=list(support=.01, confidence=.5, maxlen=3))
                         
## take a look
inspect(musicrules)
## Choose any subset you want. 
inspect(subset(musicrules, subset=lift > 5))
inspect(subset(musicrules, subset=confidence > 0.6))
inspect(subset(musicrules, subset=support > .02 & confidence > 0.6))
inspect(subset(musicrules, subset=lhs%in%"t.i."))
