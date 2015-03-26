#options(svy.entry.template=paste(readLines(file("~/svy/data/entry_template.xml")),
#                                 collapse="\n"))

#options(svy.feed.template=paste(readLines(file("~/svy/data/feed_template.xml")),
#                                collapse="\n"))

updateCells <- function(sheet, vals) {
  h <- basicTextGatherer()
  req <- sprintf(
    "?min-row=%d&max-row=%d&min-col=%d&max-col=%d&return-empty=true",
    min(vals$r),max(vals$r),min(vals$c),max(vals$c)
  )
  x <- xmlRoot(xmlParse(
    getURI(paste0(sheet@cellsfeed,req), curl=sheet@connection, 
           .opts=list(httpheader=c(Authorization=paste0("GoogleLogin auth=",
                                                        sh@connection@auth))))
  ))
  
  vals$entries <- getEntries(x,vals)
  
  curlPerform(
    url=paste0(sheet@cellsfeed,"/batch"),
    postfields=newFeed(sheet,vals),
    writefunction=h$update,
    customrequest="PUT",
    httpheader=c(
      Authorization=paste0("GoogleLogin auth=", sheet@connection@auth),
      "Content-Type"="application/atom+xml",
      curl=sheet@connection,
      verbose=TRUE
    )
  )
  h$value()
}

getEntries <- function(x,vals){
  ns <- sapply(xmlNamespaceDefinitions(x),function(d)d$uri)
  names(ns) <- ifelse(names(ns)=="","x",names(ns))
  entries <- getNodeSet(x,"//x:entry",ns)
  rc <- sapply(getNodeSet(x,"//gs:cell",ns),function(cell){
    a <- xmlAttrs(cell)
    c(a[["row"]],a[["col"]],val=a[["inputValue"]])
  })
  data.frame(r=as.numeric(rc[1,]),c=as.numeric(rc[2,]),val=rc[3,])
  inds <- match(with(vals,paste(r,c)),with(vals,paste(r,c)))
  if(any(is.na(inds))) stop(sprintf("cell not in entries"))
  entries[inds]
}

newEntry <- function(sheet,r,c,val,id){
  fmt <- getOption(
    "svy.entry.template",
    paste(readLines(file("data/entry_template.xml")),collapse="\n"))
  cellurl <- sprintf("%s/R%dC%d",sheet@cellsfeed,r,c)
  sprintf(fmt,id,cellurl,cellurl,r,c,val)
}

newFeed <- function(sheet,vals,ids=rownames(vals)){
  fmt <- getOption(
    "svy.feed.template",
    paste(readLines(file("data/feed_template.xml")),collapse="\n"))
  entries <- mapply(newEntry,r=vals$r,c=vals$c,val=vals$val,id=ids,
                    MoreArgs = list(sheet=sheet))
  sprintf(fmt,sheet@cellsfeed,paste(entries,collapse=""))
}