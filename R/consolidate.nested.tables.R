consolidate.nested.tables <- function(tl,name=NULL,dir="vertical",sep=": ",
                                      nblanks=1){
  if(is.list(tl)){
    # return a single matrix with all the nested tables contained in it
    if(is.null(names(tl))) names(tl) <- 1:length(tl)
    padded <- sapply(names(tl),function(n)
      consolidate.nested.tables(tl[[n]],name=c(n,name),dir=dir,sep=sep,
                                nblanks=nblanks),
      simplify = FALSE)
    if(dir=="vertical"){
      width <- max(sapply(padded,ncol))
      # pad narrower tables with blanks so they're all the same width
      padded <- sapply(padded,function(t)
        if(ncol(t)<width) cbind(t,matrix("",nrow(t),width-ncol(t))) else t,
        simplify = FALSE)
      do.call(rbind,padded)
    } else {
      height <- max(sapply(padded,nrow))
      # pad shorter tables so they are all the same height
      padded <- sapply(padded, function(t)
        if(nrow(t)<height) rbind(t,matrix("",ncol(t),height-nrow(t))) else t,
        simplify = FALSE)
      do.call(cbind,padded)
    }
  } else {
    if(!is.matrix(tl))
      stop(sprintf("endpoint %s is of class %s but should be a matrix",
                   paste(name,collapse=sep)),class(tl))
    tl <- matrix(as.character(tl),nrow(tl),ncol(tl))
    # browser()
    cbind(
      # add a title line at the top and "nblanks" rows at the bottom
      rbind(matrix(c(paste(name,collapse=sep),rep("",ncol(tl)-1)),1,ncol(tl)),
            tl,
            matrix("",nblanks,ncol(tl))
      ),
      # add nblanks colums to the right
      matrix("",nrow(tl)+nblanks+1,nblanks)
    )
  }
}

nest2df <- consolidate.nested.tables



