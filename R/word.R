library(plyr)

#' make an simple xlsform readable
format.xlsform <- function(df){
  qrows <- grep("^Q[1-9][0-9]*$",df$name)
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
