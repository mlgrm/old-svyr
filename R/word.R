library(plyr)

format.xlsform <- function(df){
  qrows <- grep("^Q[1-9][0-9]*$",df$name)
  df1 <- ldply(qrows,function(i){

    n <- paste0(gsub("[^0-9]","",df$name[i]),".")
    question <- as.character(df[i,grep("^label(::.+|)$",colnames(df))[1]])
    answers <-
      if(df$`question type`[i] %in% c("select_one","select_multiple")){
        a <- .getgrp(df,i)[,2]
        if(!is.na(df$`include other`) && df$`include other`=="or_other")
           a <- c(a,"other: ____________")
        paste(letters[1:length(a)],a,sep=". ",collapse="\n")
      } else "____________"

  c(n,question,answers)
  })
  colnames(df1) <- c("","Question","Answers")
  df1
}
