library(foreign)
# library(miceadds)
write.spss <- function(x,...) UseMethod("write.spss")

write.spss.svy <- function(x,dat,sps=sub("\\.[A-z0-9]+$",".sps",dat),
                                         restrict.type=FALSE){
  df <- as.data.frame(lapply(x,function(c){
    c <- structure(c,class=class(c)[!class(c)%in%c("AsIs","svq")])
    if(restrict.type){
      if(!(is.numeric(c) | is.character(c) | is.factor(c) | is.logical(c)))
        return(as.character(c))
    }
    c
    }), stringsAsFactors=FALSE)
  write.foreign(df,dat,sps,"SPSS")
  invisible(df)
}
