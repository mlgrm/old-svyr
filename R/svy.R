#' create a survey object
svy <- function(x,def=NULL,type="json"){
  if(is.null(def)){
    s <- list()
    class(s) <- "svy"
    s$data <- lapply(x,svq)
    s$def <- def
  } else {
    if(type=="json"){
      df <- extract(def,x)
      s <- lapply(df,svq.old)
    }
  }
}

#' create a svq (survey question) object from the old-style attribute based
#' svq

svq.old <- function(x){
  q <- list()
  class(q) <- "svq"
  svq$data <- x
  svq$type

}
