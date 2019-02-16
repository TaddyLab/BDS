
library(tidyr)
library(dplyr)

stocks <- read.csv("stocks.csv")
stocks$RET <- as.numeric(as.character(stocks$RET))
stocks$date <- as.Date(as.character(stocks$date), format="%Y%m%d")
stocks <- stocks %>% filter(TICKER!="" & RET!="")
dups <- which(duplicated(stocks[,c("TICKER","date")]))
stocks <- stocks[-dups,]

stocks$month <- paste(format(stocks$date, "%Y-%m"),"-01",sep="")
stocks$month <- as.Date(stocks$month)

agg <- function(r) prod(1+r, na.rm=TRUE) - 1
mnthly <- stocks %>%
  group_by(TICKER, month) %>%
  summarize(RET = agg(RET), SNP = agg(sprtrn))

RET <- as.data.frame(mnthly[,-4]) %>% spread(TICKER, RET)
SNP <- as.data.frame(mnthly[,c("month","SNP")])
SNP <- SNP[match(unique(SNP$month),SNP$month),]
dgrid <- as.Date(c("2010-01-01", "2011-01-01","2012-01-01",
	"2013-01-01", "2014-01-01","2015-01-01","2016-01-01","2017-01-01"))
RET <- RET %>% select(-MPET)

tbills <- read.csv("tbills.csv")
tbills$date <- as.Date(tbills$date)


par(mai=c(.6,.8,.1,.1))
matplot(RET[,1], RET[,-1], xlab="", ylab="Return", bty="n",
	type="l", col=heat.colors(24), lty=2, xaxt="n", yaxt="n")
axis(1, at=as.numeric(dgrid), labels=2010:2017)
axis(2, at=c(-.5,0,.5))
lines(tbills, lwd=2, col=8)
lines(SNP, lwd=2)

legend("top", lwd=2, legend="S&P500", bty="n")



####################################################
bigs <- read.csv("bigstocks.csv", header=FALSE,as.is=TRUE)
exr <- (as.matrix(RET[,bigs[,1]]) - tbills[,2])
mkt <- (SNP[,2] - tbills[,2])

capm <- lm( exr ~ mkt)
(ab <- t(coef(capm))[,2:1])

ab <- ab[-9,]

par(mai=c(.8,.8,0,0), xpd=FALSE)
plot(ab, type="n", bty="n", xlab="beta", ylab="alpha")
abline(v=1, lty=2, col=8)
abline(h=0, lty=2, col=8)
text(ab, labels=rownames(ab), cex=bigs[,2]/350, col="navy") 

