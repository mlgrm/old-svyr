library(xlsx)
# library(RGoogleDocs)
library(plyr)

#' convert a csv "simple survey" file into an xlsForm
csv2form <- function(f,...)
  xlsx2form(f,csv=TRUE,...)

#' convert an xlsx file into an xlsForm
xlsx2form <- function(f,csv=FALSE,path=NULL,title=NULL,lang="English",...){
  if(csv) wb <- createWorkbook() else wb <- loadWorkbook(f)
  if(csv) df <- read.csv(f,header=FALSE,stringsAsFactors=FALSE) else
    df <- read.xlsx(f,sheetName="survey",header=FALSE,stringsAsFactors=FALSE)
  colnames(df) <- df[1,]
  df <- df[-1,!is.na(df[1,])]
  #browser()
  l <- makeXLSForm(df,...)
  shnames <- names(getSheets(wb))
  lapply(shnames[shnames %in% names(l)],function(n)removeSheet(wb,n))
  lapply(names(l),function(n){
    sh <- createSheet(wb,n)
    addDataFrame(l[[n]],sh,row.names = FALSE)
  })
  if(is.null(path)){
    f <- gsub("[^A-z0-9./_-]+","_",f)
    if(csv) f <- sub("csv$","xlsx",f)
  } else f <- path
  saveWorkbook(wb,f)
}

#' create an xlsform from a simplified form specification
#'
#' \code{complete.xlsform} takes a data.frame in a special human readable
#' format and converts it to a legal xlsform, creating the choice sheet in the
#' process.
#'
#' @param df a data.frame or \code{GoogleWorksheetRef} object
#' @param to a file to write the resulting form to in xlsx format
#' @return a list of two data frames with the contents of the survey and choices
#' sheets in an xlsform
makeXLSForm <- function(df, to=NULL, title=NULL, lang="English", uniqtag=FALSE,
                        persistent.names=FALSE){
  if(is(df,"GoogleWorksheetRef")) return(
    makeXLSForm(as.data.frame(df,header=TRUE,trim=FALSE,
                              stringsAsFactors = FALSE)))

  # factors to strings
  fs <- sapply(df, is.factor)
  df[fs] <- lapply(df[fs], as.character)

  # treat empty cells as NA
  df[df==""] <- NA

  # remove names from a previous iteration
  df[,c("name","cN","type")] <- NA

  # these types are considered questions
  qtypes <- c(
    "select_one",
    "select_multiple",
    "text",
    "integer",
    "decimal",
    "date",
    "time",
    "dateTime"
  )
  # entry rows are those with a non-empty "question type"
  erows <- which(!is.na(df$`question type`))

  # question rows are those with non-empty values in the first "label*" column
  # and valid question types in the "question type" column
  #   browser()
  qrows <- which(!is.na(df[,grep("^label(::[A-z0-9]+|)$",colnames(df))[1]]) &
                   (df$`question type` %in% qtypes))
  df$type <- as.character(df$type)
  df$name <- as.character(df$name)
  df$cN <- as.character(df$cN)

  # questions are named Q{1-N} unless we're using UniqTag
  #     if(uniqtag && require(uniqtag)){
  #       df$name[qrows] <- uniqtag(
  #         sub("^[^a-z]+","x",
  #             gsub("[^a-z0-9_-]+","-",
  #                  tolower(
  #                    df[qrows,grep("^label(::[A-z0-9]+|)$",colnames(df))[1]])
  #             )
  #         ),5)
  # } else {
  #   }

  df$type <- as.character(df$`question type`)

  # non question or group types are named X{1-M}
  xrows <- erows[!(erows %in% qrows)]
  xrows <- xrows[grep("^(begin|end)\\s",df$`question type`[xrows],invert=TRUE)]

  # extract choices for each "select_*" question
  chgrps <- mapply(function(r,l)
    if(grepl("^select_", df$type[r])) .getgrp(df,r) else NULL,
    qrows)
  for(i in 1:length(qrows)){
    if(is.null(chgrps[[i]])) next
    df$cN[(qrows[i]+1):(qrows[i]+nrow(chgrps[[i]]))] <- chgrps[[i]]$name
  }
  names(chgrps) <- paste0("s",1:length(chgrps))
  df$type[qrows] <- sapply(1:length(qrows),function(i){
    t <- df$type[qrows[i]]
    o <- df$"include other"[qrows[i]]
    if(grepl("^select_",t)){
      t <- paste(t,names(chgrps)[i])
      if(!is.na(o)) t <- paste(t,o)
    }
    t
  })
  grows <- which(!is.na(df$type) & grepl("^begin\\s+(group|repeat)\\s*$",
                                         df$type))

  # name questions, groups and internals
  if(persistent.names){
    ncur <- sum(!is.na(df$name))
    df$name[is.element(seq(nrow(df)),erows) & is.na(df$name)] <-
      babble(length(erows))[ncur+1:length(erows)]
  } else{
    df$name[qrows] <- paste0("Q",1:length(qrows))
    df$name[xrows] <- paste0("X",1:length(xrows))
    df$name[grows] <- paste0("G",1:length(grows))
  }


  chgrps <- mapply(
    function(g,n)
      if(!is.null(g)) g <- cbind("list name"=rep(n,nrow(g)),g,
                                 stringsAsFactors=FALSE),
    chgrps,names(chgrps)
  )
  choices <- do.call(rbind,chgrps)
  l <- list(survey=df,choices=choices,pretty=prettify.xlsform(df,qrows))
  #browser()
  if(!is.null(title))
    l$settings <- data.frame(`form_title`=title,
                             `form_id`=sub("(^[0-9])","form_\\1",
                                           gsub("[^A-z0-9_-]+","_",title)),
                             `default_language`=lang)
  if(!is.null(to))dfs2xls(l,to)
  l
}

#' save a list of data.frames as an xlsx file
dfs2xls <- function(l,filename=NULL){
  wb <- createWorkbook()
  lapply(names(l),createSheet,wb=wb)
  mapply(addDataFrame,l,getSheets(wb),MoreArgs=list(row.names=FALSE))
  if(!is.null(filename))saveWorkbook(wb,filename)
  wb
}


#' make an simple xlsform readable
prettify.xlsform <- function(df,qrows){
  #qrows <- grep("^Q[1-9][0-9]*$",df$name)
  df1 <- ldply(qrows,function(i){

    n <- paste0(gsub("[^0-9]","",df$name[i]),".")
    question <- as.character(df[i,grep("^label(::.+|)$",colnames(df))[1]])
    if(df[i,"question type"]=="select_one")
      question <- paste0(question,"\nSELECT ONE")
    if(df[i,"question type"]=="select_multiple")
      question <- paste0(question,"\nSELECT ALL THAT APPLY")
    answers <-
      if(df$`question type`[i] %in% c("select_one","select_multiple")){
        a <- .getgrp(df,i,skips=TRUE)
        a <- ifelse(is.na(a[,"skip"]),a[,2],paste(a[,2],"-> SKIP TO",a[,"skip"]))
        if(!is.na(df$`include other`) && df$`include other`=="or_other")
          a <- c(a,"other: ____________")
        paste(letters[1:length(a)],a,sep=". ",collapse="\n")
      } else "____________"

    c(n,question,answers)
  })
  colnames(df1) <- c("","Question","Answers")
  df1
}


.getgrp <- function(df,r,skips=FALSE){
  # the length is the difference between the row numbers of successive entries
  # (minus 1).  the length of the last entry is the number of remaining rows
  l <- diff(which(!is.na(df$`question type`[r:nrow(df)])))[1]-1
  if(length(l)==0) l <- nrow(df)-r
  chcols <- grep("^choices",colnames(df),value=TRUE)
  anstxt <- df[(r+1):(r+l),c("name",chcols)]
  if(skips) anstxt$skip <- df[(r+1):(r+l),"skip to"]

  #remove empties (first choice column)
  anstxt <- anstxt[!is.na(anstxt[[chcols[1]]]),]
  colnames(anstxt) <- sub("^choices","label",colnames(anstxt))
  anstxt$name <- paste0("c",1:nrow(anstxt))
  anstxt
}

#' turn arbitrary strings into legal xlsForm names.
.str2name <- function(str)gsub("[^A-z0-9]+","_",sub("^([^A-z].*$)","X\\1",str))
# .str2name("b1&forty five-*...5.6.7...")
# "b1_forty_five_5_6_7_
