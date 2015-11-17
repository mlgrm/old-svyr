library(jsonlite)
#' load odk data from files
#'
#' \code{load.svy} takes a data file and a metadata file and returns a svy
#' object.
#'
#' The data file is read into a data frame. Then the optional update.fun is run
#' on the data.frame to incorporate post-collection corrections or updates. This
#' function should not add choices or make any other changes to the meta data or
#' the attributes of the data.frame or its columns.  It should simply take the
#' data.frame as its only argument and return the updated data.frame in the same
#' format, as if it had just been read from file.
#'
#' The metadata file is parsed into a list using \code{fromJSON} and
#' the questions are extracted recursively using \code{extract} on the top level
#' \code{children} element.  For each question, data is retrieved from the
#' corresponding column(s) of the data.frame and appended to the survey, and
#' the metadata is appended to the data's attributes (see \code{\link{svq}}).
#' The data is either a vector or a matrix, depending on the question
#' type (see \code{\link{extract}})
#'
#' Finally the elements of metadata list (except \code{children}) are appended
#' to the attributes of the resultant data.frame
#'
#' @section Note:
#' The default parameters of this function are meant to function with formhub
#' file naming conventions.
#'
#' @examples
#' \dontrun{
#' load.svy("mysurvey_2015_05_01_05_31_27.csv")
#' }
#' @param data a filename containing the survey data in csv format
#' @param form a filename containing the survey metadata in json format
#' @param update.fun an optional function to modify the raw data
#' @return an object of class svy inheriting from data.frame, where each column
#' is an object of class svq of a type appropriate to the question type
#'
load.svy <- function(data,form=sub("_[0-9_]+.csv",".json", data),
                     update.fun=identity, check.names=TRUE,
                     include.repeats=FALSE){
  form <- fromJSON(form)
  dat <- cleandat(read.csv(data,na.strings=c("n/a","NA","","skip"),
                           check.names=check.names),optional=!check.names)
#   browser()
  dat <- update.fun(dat)
  dat <- cleandat(dat,optional=!check.names)
  names1 <- names(dat)
  dat <- extract(form$children,dat)
#   if(!check.names) names(dat) <- names1
  dat <- as.data.frame(dat, stringsAsFactors=FALSE,optional=!check.names)
#   colnames(dat) <- sapply(dat,function(c){
#     lbl <- attributes(c)$label
#     if(is.null(lbl)) attributes(c)$name else getlabels(c)
#   })
  class(dat) <- c("svy",class(dat))
  attributes(dat) <- c(attributes(dat),form[names(form)!="children"])
  if(include.repeats){
    dat <- list(main=dat)
    repeats <- find.repeats(form$children)
    dat <- c(dat,repeats)
}
  dat
}

loadsvy <- load.svy

#' extract form data
#'
#' \code{extract} takes a list of (nested) questions in odk json format and
#' extracts the corresponding data from an appropriately formatted data.frame
#'
#' This function is primarily used by \code{\link{load.svy}}, but can be used on
#' its own to extract \code{svq} objects from an odk survey.  Each row in the
#' data.frame corresponds to a question or question group.  If it is a group,
#' its \code{name} is appended to the group vector and \code{extract} is called
#' recursively on its \code{children} element.
#'
#' For each question, a variable of the appropriate type is extracted from
#' \code{dat} using the \code{name} element, the \code{group} vector to select
#' the appropriate column(s).  "select one" questions remain factors with
#' levels converted to their labels (in English) and stored in the
#' \code{choices} attribute of the variable.  "select
#' all that apply" questions are converted into logical matrices with one column
#' for each choice (and wrapped in \code{I()} to prevent them from being broken
#' by \code{as.data.frame()}) and the (English) labels for the choices stored in
#' the \code{choices} attribute of the matrix.  "decimal" and "integer" types
#' are converted, if necessary, to numeric vectors, "date" and "today" types are
#' converted to \code{Date}, "time" types to \code{POSIXct}, and all others to
#' character vectors. Then \code{group} and all the remaining elements of the
#' question are appended to the attributes of the variable.
#'
#' @param df a dataframe of questions created by \code{fromJSON}
#' @param dat a dataframe containing all the question data for the questions
#' @param group the current groups (\code{NULL} for the top level)
#'
#' @return
#' a (flattened) list of objects of type \code{svq}, one for each question in
#' df.
extract <- function(df,dat,group=NULL,make.names=TRUE,unlist=TRUE){
  nm <- factor(df$name,levels=df$name)
  l <- by(df,nm,function(r){
    name <- r$name
    # browser(expr=r$type=="repeat")
    if(r$type=="group")
      return(extract(r$children[[1]],dat,group=c(group,r$name)))
    if(r$type=="repeat"){
      search <- sprintf("(^%s\\.[0-9]+\\.)\\..+$",paste(c(group,r$name),collapse="\\."))
      cn <- grep(search, names(dat),value = TRUE)
      rs <- unique(gsub(search,"\\1",cn))
      cols <- sapply(rs,function(n)extract(r$children[[1]],dat,
                                           group=c(group,n)),
                     simplify = FALSE)
      if(unlist) cols <- do.call(c,cols)
      return(cols)
    }
    fn <- make.names(paste("extract",r$type))
    if(exists(fn, mode="function"))
      f <- match.fun(fn) else f <- extract.unknown
    c <- f(r,dat,group=group)
#     browser(expr=r$type=="text")
    attributes(c) <- c(attributes(c),r,list(group=group))
    class(c) <- c("svq",class(c))
    list(c)
  },
  simplify=FALSE)
#   browser(expr=is.null(group))
  l <- do.call(c, l)
#   name <- function(l,prefix)if(is.list(l))sapply(names(l))
#   names(l1) <-
  l
}

getcol <- function(name,dat,group=NULL){
  cn <- paste(c(group,name), collapse=getOption("odksvy.separator","."))
  if(cn %in% colnames(dat)) dat[,cn] else rep(NA,nrow(dat))
}

getlabels <- function(l,lang=getOption("odksvy.default.lang","English")){
  if(is.null(l)) return("")
  if(is.character(l) && length(l)==1) return(l)
  if(is.data.frame(l)) return(l[[lang]])
  if(is.list(l)) return(sapply(l,function(e) if(is.list(e)) e[[lang]] else e))
  getlabels(attributes(l)$label)
}

getlabel <- function(...) getlabels(...)[1]

label <- function(x) UseMethod("label", x)
label.svq <- function(x) getlabel(attr(x, "label"))
label.default <- getlabel

extract.unknown <- function(r,dat,group=NULL){
  cn <- paste(c(group,r$name),collapse=getOption("odksvy.separator","."))
  if(cn %in% colnames(dat)) dat[,cn] else rep(NA,nrow(dat))
}

extract.text <- function(r,dat,group=NULL){
  as.character(getcol(r$name,dat,group))
}

extract.date <- function(r,dat,group=NULL){
  col <- getcol(r$name,dat,group)
  if(is.factor(col)) as.Date(levels(col)[col]) else
    as.Date(col)
}

extract.time <- function(r,dat,group=NULL){
  col <- getcol(r$name,dat,group)
  if(is.factor(col)) col <- levels(col)[col]
  col <- sub("\\.[0-9]{3}","",sub(":([0-9]{2})$","\\1",col))
  col <- sub(":([0-9]{2})$","\\1",col)
  col <- sub("\\+([0-9]{2})$","+\\1\\00",col)
  #browser()
  as.POSIXct(col,format="%Y-%m-%dT%H:%M:%S%z")
}

extract.integer <- function(r,dat,group=NULL){
  col <- getcol(r$name,dat,group)
  if(is.factor(col)) as.integer(levels(col)[col]) else
    as.integer(col)
}

extract.numeric <- function(r,dat,group=NULL){
  col <- getcol(r$name,dat,group)
  if(is.factor(col)) as.numeric(levels(col)[col]) else
    as.numeric(col)
}


extract.select.one <- function(r,dat,group=NULL,
                               lang=getOption("odksvy.default.lang","English")){
  col <- getcol(r$name,dat,group)
  lbl <- getlabels(r$children[[1]][["label"]],lang)
  r <- factor(col, levels=r$children[[1]]$name, labels=lbl)
  attr(r,"choices") <- lbl
  r
}

extract.select.all.that.apply <-
  function(r,dat,group=NULL,
           lang=getOption("odksvy.default.lang","English")){
  nm <- r$children[[1]]$name
  cn <- paste(paste(c(group,r$name),collapse=getOption("odksvy.separator",".")),
              nm, sep=getOption("odksvy.separator","."))
  # mat <- as.matrix(dat[,cn])
  # browser()
  mat <- as.matrix(as.data.frame(lapply(cn,getcol,dat)))
  colnames(mat) <- NULL
  attr(mat,"choices") <- getlabels(r$children[[1]]$label,lang)
  I(mat)
}

extract.geopoint <- function(r,dat,group=NULL){
  mat <- do.call(rbind,strsplit(as.character(getcol(r$name,dat,group))," "))
  colnames(mat) <- c("latitude","longitude","altitude","accuracy")
  I(mat)
}

extract.today <- extract.date
extract.start <- extract.time
extract.end <- extract.time
extract.deviceid <- extract.text
extract.imei <- extract.text
extract.note <- extract.text
extract.calculate <- extract.text
extract.decimal <- extract.numeric

#' get repeat table
extract.repeat <- function(group,dat,make.names=TRUE,id=row.names(dat),
                           id.field="id",ind.field="index"){
  l <- extract(group,dat,unlist=FALSE)
  l <- sapply(1:length(l),function(k){
    # browser()
    tbl <- as.data.frame(l[[k]])
    tbl <- tbl[!by(tbl,rownames(tbl),function(r)all(is.na(r))),]
    tbl[[id.field]] <- as.integer(rownames(tbl))
    tbl[[ind.field]] <- k
    tbl
  },simplify = FALSE)
  df <- do.call(rbind,l)
  df <- df[order(df[[id.field]]),]
  df
}

extend.repeat <- function(rep,main,id.field="id")
  cbind(rep,main[rep[[id.field]],])

find.repeats <- function(df){
  traverse <- function(df, group=NULL){
    by(df,df$name,function(r){
      if(r$type=="group") return(traverse(r$children[[1]],c(group,r$name)))
      if(r$type=="repeat") repeats <<- c(repeats,list(r))
    },simplify = FALSE)
    #   l <- do.call(c,l)
    #   return(l[!is.null(l)])
  }
  repeats <- list()
  # groups <- list()
  traverse(df)
  names(repeats) <- sapply(repeats,function(r)r$name)
  repeats
}



tree <- function(l,prefix=NULL){
  if(class(l)[1]=="list")
    invisible(mapply(tree,l,lapply(names(l),function(n)c(prefix,n)))) else
      cat(paste(prefix,collapse=":"),class(l),"\n")
}

summary <- function(x) UseMethod("summary", x)

summary.svy <- function(dat,lang=getOption("odksvy.default.lang","English")){
  attr.str <- function(obj,a){
    r <- attr(obj,a)
    str_or_empty(r)
  }
  str_or_empty <- function(r)if(is.null(r)) "" else r[1]

  s <- list()
  s$short <- sub("^.*\\.([^\\.]+)$","\\1",colnames(dat))
  s$label <- sapply(dat,getlabel)
  s$groups <- sapply(dat,function(c)paste(attr(c,"group"),collapse=", "))
  s$name <- sapply(dat,attr.str,"name")
  s$type <- sapply(dat,attr.str,"type")
  s$class <- sapply(dat,attr.str,"class")
  s$summary <- sapply(dat,function(c){
    s <- summary(c)
    paste(names(s),s,sep=": ",collapse="; ")
  })
  s$colname <- colnames(dat)

  as.data.frame(s,stringsAsFactors = FALSE, row.names=1:ncol(dat))
}

attributes.svy <- function(s)lapply(s,attributes)
apply.attr.svy <- function(a,s){
  a <- lapply(a,function(a1){if(!is.null(a1$dim))a1$dim[1] <- nrow(s);a1})
  as.data.frame(mapply(function(a1,s1){
    browser(expr=(("factor"%in%a1$class)&& !("factor"%in%class(s1))))
    attributes(s1) <- a1
    s1
    },a,s), stringsAsFactors=FALSE)
}

apply.attr.svyq <- function(a,q){
  attributes(q) <- sapply(names(a),
                            function(n){
                              if(is.null(attributes(q)[[n]])) a[[n]] else
                                attributes(q)[[n]]
                            },
                            simplify=FALSE, USE.NAMES=TRUE)
  q
}

pres.svy <- function(s,f,...){
  a <- attributes.svy(s)
  s <- f(s,...)
  apply.attr.svy(a,s)
}

pres.svyq <- function(q,f,...){
  a <- attributes(q)
  res <- f(q,...)
  attributes(res) <- sapply(names(a),
                            function(n)
                              if(is.null(attributes(res)[n])) a[[n]] else
                                attributes(res)[[n]],
                            simplify=FALSE, USE.NAMES=TRUE)
  res
}

as.svy <- function(dat,tmp){
  att <- attributes.svy(tmp)
  as.data.frame(mapply(apply.attr.svyq,att,dat))
}

as.svyq <- function(col, label=attributes(col)$label, ...) switch(
  class(col)[1],
  integer=as.svyq.integer(col,label,...),
  numeric=as.svyq.numeric(col,label,...),
  factor=as.svyq.factor(col,label,...),
  ordered=as.svyq.factor(col,label,ordered=TRUE,...),
  stop("unrecognized vector class")
  )

as.svyq.factor <- function(col, label, ordered=FALSE,
                           choices=levels(col)){
  factor(col,levels=choices)
  attributes(col)$label <- label
  attributes(col)$type <- "select one"
  col
}

as.svyq.numeric <- function(col, label){
  attributes(col)$label <- label
  attributes(col)$type <- "decimal"
  col
}

as.svyq.integer <- function(col, label){
  attributes(col)$label <- label
  attributes(col)$type <- "integer"
  col
}

split.svyq <- function(x, f, ...){
  lapply(split(x,f,...),as.svyq,x)
}

split.svy <- function(x, f, ...){
  lapply(split(x,f,...),as.svy,x)
}

as.data.frame.svy <- function(x){
  l <- mapply(function(c,n){
    if(is.matrix(c)){
      class(c) <- "matrix"
      df <- as.data.frame(c)
      colnames(df) <- attributes(c)$choices
      df
    } else {
      df <- data.frame(c)
      colnames(df) <- n
      df
    }
  },x,colnames(x),SIMPLIFY=FALSE)
  do.call(cbind,l)
}

choices.svyq <- function(x){
  switch(attributes(x)$type,
         "select one"=levels(x),
         "select all that apply"=attributes(x)$choices,
         NULL
  )
}
# consolidate choices into a smaller number of choices
cons.choices <- function(q,l){
  choicenames <- ifelse(names(l)!="",names(l),
                        ifelse(sapply(l,function(e)length(e)==1),
                               sapply(l,function(e)attributes(q)$choices[e]),
                               NA))
  if(any(is.na(choicenames))) stop("consolidated choices must be named")
  if(is.matrix(q)){
    r <- sapply(l,function(cs){
      if(length(cs)==1) q[,cs] else
      rowSums(q[,cs])>0
    })
    r <- apply.attr.svyq(attributes(q),r)
    attributes(r)$choices <- choicenames
    r
  } else {
    lvl <- rep(NA,length(levels(q)))
    for(i in 1:length(l))
      if(names(l)[i]=="") lvl[l[[i]]] <- levels(q)[l[[i]]] else
        lvl[l[[i]]] <- names(l)[i]
    levels(q) <- lvl
    attributes(q)$choices <- levels(q)
    q
  }
}



clean.svy <- function(svy)apply.attr.svy(attributes.svy(svy),cleandat(svy))

