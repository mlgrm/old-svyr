as.data.frame.svy <- function(s,remove.attributes=TRUE,...){
  as.data.frame(lapply(s,function(q){
    q <- unAsIs(q)
    class(q) <- class(q)[class(q)!="svq"]
    if(remove.attributes) 
      attributes(q) <- attributes(q)[names(attributes(q)) %in% 
                                     c("class", "dim", "dimnames")]
    q
  }),...)
}

as.data.frame.svq <- function(x,remove.attributes=TRUE,...){
  x <- unAsIs(x)
  class(x) <- class(q)[class(q)!="svq"]
  if(remove.attributes) 
    attributes(x) <- attributes(x)[names(attributes(x)) %in% 
                                     c("class", "dim", "dimnames")]
  as.data.frame(x,...)
}

unAsIs <- function(x){
  class(x) <- class(x)[class(x)!="AsIs"]
  x
}