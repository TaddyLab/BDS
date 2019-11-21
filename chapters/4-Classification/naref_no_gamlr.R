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