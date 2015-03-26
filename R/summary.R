tables.svy <- function(dat,...){
  l <- lapply(dat,table.svy,...)
}

table.svy <- function(c,by,strat=NULL,N=NULL,...){
  if(is.null(attributes(c)$type)) return(NULL)

  fun <- switch(attributes(c)$type,
                "select one"=gpoll,
                "select all that apply"={
                  class(c) <- "matrix"
                  gmulti
                },
                "date"={
                  c <- factor(c,ordered=TRUE)
                  gdate
                },
                integer=gmeans,
                numeric=gmeans,
                decimal=gmeans,
                return(NULL)
  )
#   browser(expr=class(c)=="AsIs")
  title <- getlabels(c)
  drill(c,by,fun,title,strat,N,...)
}

