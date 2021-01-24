## data from kilts website, 
## use sas system viewer to convert from sd2 to tsv
library(gamlr)
library(data.table)
library(bit64)

## basic transaction data
wber <-fread("wber.tsv")
wber <- wber[PRICE>0 & OK==1,]
wber$PRICE <- wber$PRICE/wber$QTY

wber <- wber[,c("STORE","UPC","WEEK","PRICE","MOVE"),with=FALSE]

setkey(wber,UPC,STORE,WEEK)

## limit to not super-rare UPCs or stores with few sales
storetab <- table(wber$STORE)
common <- names(storetab[storetab>10000])
wber <- wber[wber$STORE %in% common,]
wber[,STORE:=factor(STORE)]
upctab <- table(wber$UPC)
common <- as.integer64(names(upctab))[upctab>1000]
wber <- wber[wber$UPC %in% common,]


# demographic data
library(foreign)
demo <- read.dta("demo.dta")
demo <- demo[demo$store%in%levels(wber$STORE),]
demo$gini <- NULL # empty field
demo$zone <- factor(demo$zone, 
	levels=c(NA,sort(unique(demo$zone))), exclude=NULL)
demo$scluster <- factor(demo$scluster)
table(demo$scluster)
demo$mmid <- NULL
rownames(demo) <- NULL
demo[,c("name","store")] 
# note 133, 136, 139 are unnamed; they are new and have no demo info
demo <- demo[demo$name!="", ]
rownames(demo) <- as.character(demo$store)
demo <- demo[, c(6,10:54)]

# match up overlapping stores in UPCs in Demo
wber <- wber[ wber$STORE%in%rownames(demo) ]
wber[,STORE:=factor(STORE)]
wber[,WEEK:=factor(WEEK)]
wber[,UPC:=factor(UPC)]
nupc <- nlevels(wber$UPC)
nweek <- nlevels(wber$WEEK)
nstore <- nlevels(wber$STORE)

## finally, the UPC data
upc <- as.data.frame(fread("upcber.tsv"))
upc$CASE <- upc$NITEM <- NULL
rownames(upc) <- as.character(upc$UPC)
upc$UPC <- NULL
zebra <- match(levels(wber$UPC),rownames(upc))
upc <- upc[na.omit(zebra),]
all(levels(wber$UPC)==rownames(upc))

## filter on sizes and count ounces
table(upc$SIZE)
upc <- upc[upc$SIZE %in% c("12/12O","24/12O","6/12 O","6/12OZ","32 OZ","30/12O"),]

upc$OZ <- 72
upc$OZ[upc$SIZE == "12/12O"] <- 144
upc$OZ[upc$SIZE == "24/12O"] <- 288
upc$OZ[upc$SIZE == "30/12O"] <- 360
upc$OZ[upc$SIZE == "32 OZ"] <- 32

upc$SIZE <- gsub(" ", "", upc$SIZE, fixed=TRUE)
upc$SIZE <- gsub("/12OZ", "/12O", upc$SIZE, fixed=TRUE)
upc$SIZE <- gsub("/12O", "pk", upc$SIZE, fixed=TRUE)

upc$DESCRIP <- paste(upc$DESCRIP, upc$SIZE)
upc$SIZE <- upc$COM_CODE <- NULL

wber <- wber[ wber$UPC%in%rownames(upc) ]
wber[,UPC:=as.character(UPC)]
wber[,STORE:=as.character(STORE)]
wber <- as.data.frame(wber)

save(wber, upc, demo, file="dominicks-beer.rda")

load("../../BDS/examples/dominicks-beer.rda")
wber$STORE <- factor(as.numeric(wber$STORE))

sales <- naref(wber)
names(sales) <- c("store","upc","week","price","units")
sales<-sales[order(sales$store,sales$upc,sales$week),] 
names(upc) <- c("title","oz")

save(sales, upc, file="Beer.rda", compress=TRUE)

