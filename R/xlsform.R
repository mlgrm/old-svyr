library(xlsx)
library(RGoogleDocs)


#' create an xlsform from a simplified form specification
#'
#' \code{complete.xlsform} takes a data.frame in a special human readable
#' format and converts it to a legal xlsform, creating the choice sheet in the
#' process.
#'
#' @param df a data.frame or \code{GoogleWorksheetRef} object
#' @return a list of two data frames with the contents of the survey and choices
#' sheets in an xlsform
makeXLSForm <- function(df, to=NULL){
  if(is(df,"GoogleWorksheetRef")) return(
    makeXLSForm(as.data.frame(df,header=TRUE,trim=FALSE,
                              stringsAsFactors = FALSE)))

  # factors to strings
  fs <- sapply(df, is.factor)
  df[fs] <- lapply(df[fs], as.character)
  # these types are considered questions
  qtypes <- c(
    "select_one",
    "select_multi",
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
  qrows <- which(!is.na(df[,grep("^label(::[A-z0-9]+|)$",colnames(df))[1]]) &
                   (df$`question type` %in% qtypes))
  df$type <- as.character(df$type)
  df$name <- as.character(df$name)
  df$cN <- as.character(df$cN)

  # questions are named Q{1-N}
  df$name[qrows] <- paste0("Q",1:length(qrows))
  df$type <- as.character(df$`question type`)

  # non question or group types are named X{1-M}
  xrows <- erows[!(erows %in% qrows)]
  xrows <- xrows[grep("^(begin|end)\\s",df$`question type`[xrows],invert=TRUE)]
  df$name[xrows] <- paste0("X",1:length(xrows))

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
  df$name[grows] <- paste0("G",1:length(grows))
  chgrps <- mapply(
    function(g,n)
      if(!is.null(g)) g <- cbind("list name"=rep(n,nrow(g)),g,
                                 stringsAsFactors=FALSE),
    chgrps,names(chgrps)
  )
  choices <- do.call(rbind,chgrps)
  list(survey=df,choices=choices)
}

dfs2xls <- function(l,filename){
  wb <- createWorkbook()
  lapply(names(l),createSheet,wb=wb)
  mapply(addDataFrame,l,getSheets(wb),MoreArgs=list(row.names=FALSE))
  saveWorkbook(wb,filename)
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
