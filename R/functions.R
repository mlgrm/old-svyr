library(Hmisc)
library(plyr)

#' summarize the columns in a data frame
datsum <- function(dat,ql=NULL){
  data.frame(
    name=colnames(dat),
    #   long.name=ifelse(colnames(dat) %in% ql[,2],
    #                    as.character(ql[,1])[match(colnames(dat),ql[,2])],""),
    text=ifelse(colnames(dat) %in% ql$name,
                strtrim(
                  as.character(ql$text)[match(colnames(dat),ql$name)],80
                ),""),
    class=vapply(dat,function(c)paste0(class(c),collapse=" "),character(1)),
    type=mapply(function(c,n)switch(class(c)[1],
                                    numeric="numeric",
                                    integer="integer",
                                    factor={
                                      if(grepl("^Q[0-9]+R[0-9]+$",n)){
                                        "multiple response"
                                      }else if(grepl("^Q.+X.*$",n) ||
                                                 length(levels(c))>0.25*length(c)){
                                        "freeform"
                                      }else "multiple choice"},
                                    logical={
                                      if(grepl("^Q[0-9]+R[0-9]+$",n)){
                                        "multiple response"
                                      }else "other"
                                    },
                                    "other"
    ),dat,colnames(dat)),
    choices=sapply(dat,function(c)
      switch(class(c)[1],
             factor=paste0("\"",levels(c),"\"",collapse = ", "), "")),
    `missing vals`=sapply(dat,function(c)sum(is.na(c)/nrow(dat))),

    summary=vapply(dat,function(c){
      s <- summary(c)
      s <- paste(names(s),s,sep=": ",collapse=", ")
    }, character(1)),
    NAs=vapply(dat,function(c)sum(is.na(c)),integer(1)),
    row.names=NULL
  )
}

Sys.setlocale('LC_ALL','C')
#' clean up a data frame
cleandat <- function(dat,optional=FALSE){
  as.data.frame(lapply(dat,function(c){
#      browser(expr=(class(c)=="integer"))
    if(is.integer(c)) c <- pres.svyq(c,as.numeric)
    switch(class(c)[1],
           factor={
             levels(c) <- sub("\\s+$","",levels(c))
             levels(c) <- gsub("\xa0","",levels(c))
             levels(c) <- gsub("\xc2","",levels(c))
             levels(c) <- gsub("\x80","",levels(c))
             levels(c) <- gsub("\x99","",levels(c))
             levels(c) <- gsub("\240","",levels(c))
             levels(c) <- gsub("\302","",levels(c))
             levels(c) <- gsub("\x92","'",levels(c))
             levels(c) <- gsub("\xe2","'",levels(c))
             levels(c) <- gsub("\x96","-",levels(c))
             levels(c) <- gsub("\xa6","...",levels(c))
             levels(c) <- gsub("\x93","-",levels(c))
             levels(c) <- gsub("\U3e32393c","-",levels(c))
             c[c=="Skip"] <- NA
             c[c=="Interviewer Mistake"] <- NA
             c[c==""] <- NA
             c[c=="0"] <- NA
             c[c=="98"] <- NA
             c[c=="n/a"] <- NA
             c <- droplevels(c)
             if(all.is.numeric(levels(c))) c <- as.numeric(levels(c))[c]
             if(all(grepl("^(true|false)$",levels(c),ignore.case = T)) ||
                  all(grepl("^(T|F)$",levels(c)))) c <- as.logical(c)
             c
           },
           numeric={
             c[c<0] <- NA
             c[c==98] <- NA # this is royally stupid
             c
           },
           {
             c
           }
    )
  }),stringsAsFactors=FALSE,optional=optional)
}

#' extract repeats from survey data frame
#'
#' \code{cols2minisurvey} takes a data.frame and a list of vectors of column
#' names or indices and return a minisurvey dataframe.
#'
cols2minisurvey <- function(dat,colsets,id="QID"){
  #   cns <- colnames(dat[,colsets[[1]]])
  dat1 <- ldply(colsets,function(cs){
    if(all(is.na(dat[,cs]))) return(NULL)
    dat[,colsets[[1]]] <- dat[,cs]
    dat <- dat[rowSums(!is.na(dat[,cs]))>0,]
    dat[,unlist(colsets[-1])] <- NA
    dat
  })
  dat1 <- dat1[,-unlist(colsets[-1])]
  dat1
}

clean.multi <- function(df,true="Yes"){
  as.data.frame(lapply(df,function(c)(!is.na(c) & c==true)))
}

clean.multi2 <- function(df,cols,true="Yes"){
  df[,cols]  <- clean.multi(df[,cols],true=true)
  df
}

clean.mrq <- function(nms,qs=qs){
  qs[nms,"text"]  <- sub("^.*\\?\\s*","",qs[nms,"text"])
  qs
}

# multiple response with limited number of responses in columns
lr2mr <- function(dat,cols){
  vals <- unique(unlist(lapply(dat[,cols],levels)))
  names(vals) <- vals
  as.data.frame(lapply(
    vals,
    function(cn){
      v <- (!is.na(dat[,cols[1]])) & (as.character(dat[,cols[1]])==cn)
      for(c in cols[-1]) v <- v | ((!is.na(dat[,c])) & (dat[,c]==cn))
      v
    }
  ))
}

t.table.list <- function(l){
  if(is.list(l)) lapply(l, t.table.list) else t(l)
}

get.mr <- function(dat,qn,tbl=qs,
                     fmt=getOption("svy.mr.fmt",
                                   default="^Q\1R([1-9]|[1-4][0-9]|77)$")){
  if(is.character(qn)) qn <- sub("[^0-9]([0-9]+).*","\\1",qn)
  mr <- as.matrix(dat[,grep(sub("\1",qn,fmt),colnames(dat))])
  colnames(mr) <- lookup(colnames(mr),tbl)
  mr
}

lookup <- function(cn,tbl=qs,name="name",label="text"){
  m <- as.character(tbl[match(cn,tbl[,name]),label])
  m[is.na(m)] <- cn[is.na(m)]
  m
}

moe <- function(p,n,doc=NULL, N=Inf)
  sqrt(p*(1-p)/n*
         if(is.infinite(N)) 1 else (N-n)/(N-1))*
  if(is.null(doc)) 1 else qnorm(1-(1-doc)/2)

# functions for regression
fwd.diff <- function(c){
  m <- t(sapply(c,function(a)1:(length(levels(a))-1)<as.integer(a)))
  browser(expr=("Female"%in%levels(c)[-1]))
  attributes(m)$choices <- levels(c)[-1]
  attributes(m)$label <- attributes(c)$label
  I(m)
}

# '%*%' <- function(f,g) UseMethod('%*%')

compose <- function(f,g) function(...)f(g(...))


