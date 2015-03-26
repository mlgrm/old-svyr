# take a list of lists of input data (vectors or data.frames for MRs) and 
# return a nested list of result tables
table.list <- function(q.list,g.list,f.list){
  mapply(drill,q.list,g.lists,f.list)
}

make.table.list <- function(dat,instr){
  instr <- instr[!(!is.na(instr$ignore) & instr$ignore),]
  instr <- as.matrix(instr)
  rownames(instr) <- instr[,"title"]
  apply(instr,1,function(r){
    qstr <- unlist(strsplit(r["question"],",\\s*"))
    if(r["type"]=="gmulti") q.list <- lapply(qstr,function(s)
      get.mr(dat,sub("[^0-9]*([0-9]+).*","\\1",s))) else
        q.list <- dat[,qstr]
#     browser(expr = r["type"]=="gmulti")
    if(!is.list(q.list)) q.list <- list(q.list)
    g.list <- dat[,unlist(strsplit(r["group"],",\\s*"))]
    if(!is.list(g.list)) g.list <- list(g.list)
    f.list <- eval(as.name(r["type"]))
    drill(q.list,g.list,f.list)
  })
}

get.panel.size <- function(p){
  if(is.list(p)){
    print(class(p))
    tbls <- sapply(p,get.panel.size)
#     if(is.null(dim(tbls))) return(tbls)
    return(c(length=sum(tbls["length",],width=max(tbls["width",]))))
  }
  print(class(p))
  print(p)
  sz <- c(length=nrow(p),width=ncol(p))
  sz <- ifelse(is.na(sz),1,sz)
  sz
}

get.subsections <- function(dat,instr){
  instr
  if(instr[1,"restrictions"]!=""){
    reststr <- paste(strsplit(as.character(instr[1,"restrictions"]),
                              ",\\s*")[[1]], collapse=" & ")
    dat <- with(dat, dat[eval(parse(text=reststr)),])
  }
  c(
    total=list(make.table.list(dat,instr)),
    lapply(strsplit(as.character(instr[1,"subsections"]),",\\s*")[[1]],
         function(s){
           lapply(with(dat,split(dat,eval(as.name(s)))),make.table.list,instr)
         })
  )
}

# dlply(instr,"title",print)
