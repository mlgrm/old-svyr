library(xlsx)

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

complete.xlsform <- function(df){
  qrows <- which(!is.na(df[,grep("^label",colnames(df))[1]]) & 
                   (df$"question type" %in% qtypes))
  df$type <- as.character(df$type)
  df$name <- as.character(df$name)
  df$name[qrows] <- paste0("Q",1:length(qrows))
  df$type <- as.character(df$"question type")
  chgrps <- mapply(function(r,l)
    if(grepl("^select_", df$type[r])) .getgrp(df,r+1,l-1) else NULL,
    qrows,c(diff(qrows),nrow(df)-qrows[length(qrows)]))
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
      if(!is.null(g)) g <- cbind("list name"=rep(n,nrow(g)),g),
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

.getgrp <- function(df,r,l){
  chcols <- grep("^choices",colnames(df),value=TRUE)
  anstxt <- cbind(data.frame(name=1:l),as.data.frame(df[r:(r+l-1),chcols]))
  #remove empties
  anstxt <- anstxt[by(anstxt,rownames(anstxt),
                      function(r)any(!is.na(r[-1]))),]
  colnames(anstxt)[-1] <- sub("choices","label",chcols)
  anstxt$name <- paste0("c",1:nrow(anstxt))
  anstxt
}

#' turn arbitrary strings into legal xlsForm names.
.str2name <- function(str)gsub("[^A-z0-9]+","_",sub("^([^A-z].*$)","X\\1",str))
# .str2name("b1&forty five-*...5.6.7...")
