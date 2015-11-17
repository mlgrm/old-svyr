#' drill down (cross tabulate a svq)
#'
#' \code{drill.svq} is the drill implementation for class \code{svq}
#'
#' The svq is split by the factor \code{by} and cross sectional measurements
#' are made for for each subgroup.  The measurement made is determined by the
#' type of the svq.  "select one" questions generate cross tabulations of
#' responses in each subgroup and overall.  One table will be generated for the
#' counts, a second for an estimate of the proportion of each subgroup
#' responding with each choice, and a third for the standard error of each
#' estimate.  "select all that apply" (select_multi) questions are similar
#' but the proportions will not, in general, add up to one.  "decimal" and
#' "integer" questions will generate a table with the mean, the standard
#' deviation, the standard error (others to be added).  The last column in each
#' table will give the totals.  "date" questions will
#' generate a table of frequencies.
#'
#' Optionally, \code{q} can be a list of \code{svq} objects of the same length
#' and question type, and \code{drill.svq} will return a list of results.
#' \code{by} can also be a list of factors, with a similar result, but the
#' implementation of this is a little rough.
#'
#' @section Stratified samples:
#'
#' If the data was collected using stratified sampling, estimates and standard
#' errors can be calculated accordingly by setting \code{strat} and \code{N}.
#' \code{strat} should be a factor of the same length as q that can be used to
#' split the data into the strata.  N is a vector of length
#' \code{length(levels(strat))} with the population size corresponding to each
#' stratum.  If \code{N} is specified as an integer and strat is \code{NULL},
#' the finite population correction will be applied to the error estimates.
#'
#' @param q a svq object
#' @param by a factor to split \code{q} by (usually a "select one" svq)
#' @param strat an optional factor of strata for stratified samples
#' @param N the vector of population values for each stratum
#' @return
#' a list of class \code{svqdrill} with the following elements:
#' \itemize{
#'  \item{type}{the question type}
#' }
#' for "select" types:
#' \itemize{
#'  \item{\code{counts}}
#'  \item{\code{proportion}}
#'  \item{\code{margin of error}}
#' }
#' for "integer" and "decimal" types:
#' \itemize{\item{\code{means}}}
#' for "date" types:
#' \itemize{\item{\code{counts}}}
#'
#'
# take a list of vectors or data frames, a list of grouping factors, and
# a list of functions and apply each function to each grouping of each data
# element
drill.svq <- function(q,by,strat=NULL,N=NULL,...){
  if(is.null(attributes(q)$type)) return(NULL)
  #       attr(c,"type") <- switch(class(c),
  #                                factor="select one",
  #                                matrix="select all that apply")

  fun <- switch(attributes(q)$type,
                "select one"=gpoll,
                "select all that apply"={
                  class(q) <- "matrix"
                  gmulti
                },
                "date"={
                  c <- factor(q,ordered=TRUE)
                  gdate
                },
                integer=gmeans,
                numeric=gmeans,
                decimal=gmeans,
                return(NULL)
  )
  #   browser(expr=class(c)=="AsIs")
  title <- getlabel(q)
  l <- drill(q,by,fun,title,strat,N,...)
  simplify <- function(l){
    if(is.list(l) && length(l)==1) simplify(l[[1]]) else
      if(is.list(l)) lapply(l,simplify) else
        l
  }
  res <- list()
  class(res) <- "svqdrill"
  res$type <- attr(c,"type")
  res <- c(res,simplify(l))
  res
}


DOC=pnorm(1)
options(xtable.type="html")
options(xtable.html.table.attributes=c(
  "cellpadding=5, border=1"
))

# take a vector of scalars and return the mean, standard dev, and moe
gmeans.fn <- function(v,strat=NULL, N=NULL){
  if(!is.null(strat)){
    sN <- sum(N)
    n <- sapply(split(v[!is.na(v)],strat[!is.na(v)]),length)
    mean <- sum(N*sapply(split(v,strat),mean,na.rm=TRUE))/sN
    sdev <- sqrt(sum(N*sapply(split(v,strat),var,na.rm=TRUE))/sN)
    se <- sqrt(sum(N*sapply(split(v,strat),var,na.rm=TRUE)*(N-n)/n))/sN
    moe <- qnorm(DOC)*se
  } else {
    n <- length(v[!is.na(v)])
    mean <- mean(v,na.rm=TRUE)
    sdev <- sd(v,na.rm=TRUE)
    se <- sd(v,na.rm=TRUE)/sqrt(length(v[!is.na(v)]))
    #     browser()
    if(!is.null(N)) se <- se*sqrt((N-n)/(N-1))
    moe <- qnorm(DOC)*se
  }
  c(
    mean=mean,
    sdev=sdev,
    stderr=se,
    moe=moe
  )
}

gmeans <- list(means=gmeans.fn)


gpoll.counts <- function(v,strat=NULL,N=NULL) table(v)

gpoll.frac <- function(v,strat=NULL,N=NULL){
  if(!is.null(strat))
    (sapply(split(v,strat),gpoll.frac)%*%N)[,1]/sum(N) else
      table(v)/length(v[!is.na(v)])
}

.prop.moe <- function(v,f,strat,N){
  var <- function(v){
    p <- f(v)
    p*(1-p)
  }
  if(!is.null(strat)){
    vl <- split(v,strat)
    n <- sapply(vl,function(v)
      if(is.matrix(v) || is.data.frame(v))
        n <- nrow(v[!is.na(v[,1]),]) else
          n <- length(v[!is.na(v)]))
    se <- sqrt((sapply(vl,var)%*%(N*(N-n)/(n-1)))[,1])/sum(N)
  }else{
    if(is.matrix(v) || is.data.frame(v)) n <- nrow(v[!is.na(v[,1]),]) else
      n <- length(v[!is.na(v)])
    se <- sqrt(var(v)/n)
    if(!is.null(N)) se <- se*sqrt((N-n)/(N-1))
  }
  qnorm(DOC)*se
}

gpoll.moe <- function(v,strat=NULL,N=NULL) .prop.moe(v,gpoll.frac,strat,N)

gpoll <- list(
  counts=gpoll.counts,
  proportion=gpoll.frac,
  "margin of error"=gpoll.moe
)


gmulti.counts <- function(m,strat=NULL,N=NULL, choices=attributes(m)$choices,
                          metadata=getOption("svy.metadata",NULL)){
  if(is.null(choices)){
    matchnames <- as.character(metadata$text[match(colnames(m),metadata$name)])
    colnames(m) <- ifelse(is.na(matchnames),colnames(m),matchnames)
  } else colnames(m) <- choices
  #   browser()
  colSums(m,na.rm=TRUE)
}
# take a matrix of multiple responses and return a table with the fraction of
# respondents saying yes to each, plus a table of error margins
gmulti.frac <- function(m,strat=NULL,N=NULL, choices=attributes(m)$choices,
                        metadata=getOption("svy.metadata",NULL)){
  if(is.null(choices)){
    browser(expr=is(tryCatch(is.data.frame(m),error=identity),"error"))
    matchnames <- as.character(metadata$text[match(colnames(m),metadata$name)])
    colnames(m) <- ifelse(is.na(matchnames),colnames(m),matchnames)
  } else colnames(m) <- choices
  if(!is.null(strat))
    (sapply(split(as.data.frame(m),strat),gmulti.frac)%*%N)[,1]/sum(N) else
      sapply(m,function(c)sum(c,na.rm=TRUE)/length(c[!is.na(c)]))
}

gmulti.moe  <- function(m,strat=NULL,N=NULL, choices=attributes(m)$choices,
                        metadata=getOption("svy.metadata",NULL)){
  moe <- .prop.moe(m,gmulti.frac,strat,N)
  if(!is.null(choices)) names(moe) <- choices
  moe
}

gmulti <- list(
  counts=gmulti.counts,
  proportion=gmulti.frac,
  "margin of error"=gmulti.moe
)

gdate <- list(counts=function(c,...)table(c))

# take a list of vectors or data frames, a list of grouping factors, and
# a list of functions and apply each function to each grouping of each data
# element
drill <- function(dat,grpgs,f=gpoll,title=NULL,strat=NULL,N=NULL,...){
  if(!is.list(dat) || is.data.frame(dat)) dat <- list(dat)
  if(!is.list(grpgs)) grpgs <- list(grpgs)
  if(!is.list(f)) f <- list(f)
  l <- lapply(dat,function(v){
    #overall=lapply(f,function(f1)as.table(f1(v))),
    if(is.factor(v)||is.data.frame(v)) v <- droplevels(v)
    if(is.matrix(v)){
      choices <- attributes(v)$choices
      v <- as.data.frame(v)
      attributes(v)$choices <- choices
    }
    list(by={
      lapply(grpgs,function(g){
        if(is.factor(g)) g <- droplevels(g)
        #         browser(expr=is.matrix(g))
        vl <- c(split(v,g),overall=list(v))
        if(!is.null(strat)){
          stratl <- c(split(strat,g,drop=TRUE),overall=list(strat))
          stratl <- lapply(stratl,droplevels)
          Nl <- lapply(stratl,function(f)N[levels(f)])
          stratl <- lapply(stratl,function(s)
            if(length(levels(s))==1) NULL else s)
        }else{
          stratl <- c(lapply(vl,function(a)NULL))
          if(is.null(N)) Nl=stratl else Nl=list(N)
        }
        lapply(f, function(f1){
          mapply(f1,vl,stratl,Nl)
        })
      })
    })
  })
  attr(l,"title") <- title
  l
}

drillplex <- function(dat)

  moe.mean <- function(v,c=DOC)qnorm(c)*sd(v)/sqrt(length(v))

# apply a function recursively to splits of the first column by each successive
# column
recapply <- function(l,f,...){
  if(length(l)==2) return(sapply(split(l[[1]],l[[2]]),f))
  sapply(split(as.data.frame(l[-2]),l[[2]]),recapply,function(v)f(v,...))
}

prntbl <- function(title,name="",t,
                   sheet=getOption("table.display.sheet",NULL),
                   currentRow=getOption("table.display.currentRow",1),
                   prn=TRUE){
  if(prn) print(t)
  print_nested_tables(t,title,name,sheet=sheet,currentRow=currentRow)
}

# dim.tblset

# take a logical column and break out the trues by several factors
facetl <- function(lc,fs){
  count <- recapply(c(list(lc),fs),sum)
  num <- recapply(c(list(lc),fs),length)
  p <- count/num
  moe <- qnorm(DOC)*sqrt(p*(1-p)/num)
  list(count=count,proportion=p,"margin of error"=moe)
}

setSheet.default <- function(type,name,
                             wb=getOption("table.display.default.workbook")){
  if(type !="xlsx") return(NULL)
  if(getOption("table.display.single.sheet",default=FALSE)){
    sheet <- getOption("table.display.sheet")
    return(sheet)
  }

  currentRow <<- 1
  createSheet(wb,name)
}

print.title <- function(title,type,sheet=NULL,
                        currentRow=getOption("table.display.currentRow",1)){
  switch(type,
         xlsx={
           r <- createRow(sheet,rowIndex = currentRow:(currentRow+2))
           c <- createCell(r, colIndex = 1)
           setCellValue(c[1][[1]],paste0(rep("#",nchar(title)),collapse=""))
           setCellValue(c[2][[1]],title)
           setCellValue(c[3][[1]],paste0(rep("#",nchar(title)),collapse=""))
           currentRow  <- currentRow + 4
         },
         print=cat(title,"\n"),
         stop("unrecognized type")
  )
  currentRow
}

#i <-
print_nested_tables <-
  function(l,
           title=NULL,
           name=NULL,
           type=getOption("table.display.type"),
           sheet=setSheet.default(type,name),
           currentRow=getOption("table.display.currentRow",1),
           rowPrint=FALSE){
    if(!is.null(title)) currentRow <- print.title(title,type,sheet,currentRow)
    if(is.list(l) && !all(sapply(l,is.list)) && name!="by")
      currentRow  <- print.title(name,type,sheet, currentRow)
    #       switch(type,
    #              xlsx={
    #                r <- createRow(sheet,rowIndex = currentRow:(currentRow+2))
    #                c <- createCell(r, colIndex = 1)
    #                setCellValue(c[1][[1]],paste0(rep("#",nchar(title)),collapse=""))
    #                setCellValue(c[2][[1]],title)
    #                setCellValue(c[3][[1]],paste0(rep("#",nchar(title)),collapse=""))
    #                currentRow <<- currentRow + 4
    #              }
    #              )
    if(is.list(l)){
      # try to print multitables row-wise
      if(type=="xlsx" && rowPrint &&
           all(sapply(l,is.matrix)) &&
           length(l)==3 &&
           all(names(l)==c("counts","proportion","margin of error"))
      ){
        for(n in names(l)){
          rn <- currentRow
          r <- createRow(sheet,rowIndex = currentRow)
          c <- createCell(r, colIndex = 1)
          setCellValue(c[1][[1]],name)
          currentRow <- currentRow + 1
          addDataFrame(l,sheet,startRow = currentRow)
          currentRow <- currentRow + ifelse(is.null(l),1,nrow(l)) + 1
          r <- createRow(sheet,rowIndex = currentRow)
          currentRow <- currentRow + 1
        }
      } else
        for(i in 1:length(l))
          currentRow <- print_nested_tables(l[[i]],
                                            name=paste(name,names(l)[[i]]),
                                            type=type,
                                            sheet=sheet,
                                            currentRow=currentRow)
    } else {
      i <<-i+1
      switch(type,
             markdown={
               cat("#### ",name,"\n")
               print(xtable(l,caption="", digits=6))
               cat("\n")
             },
             view={
               View(l,name)
             },
             View={
               View(l,name)
             },
             print={
               cat(name,":\n")
               print(l)
               cat("\n")
             },
             xlsx={
               r <- createRow(sheet,rowIndex = currentRow)
               c <- createCell(r, colIndex = 1)
               setCellValue(c[1][[1]],name)
               currentRow <- currentRow + 1
               addDataFrame(l,sheet,startRow = currentRow)
               # browser(expr=(!length(nrow(l))))
               currentRow <- currentRow +
                 ifelse(is.null(l) || is.vector(l),1,nrow(l)) + 1
               r <- createRow(sheet,rowIndex = currentRow)
               currentRow <- currentRow + 1
             },
             stop("unknown display type")
      )
    }
    currentRow
  }

print_nested_tables_row <- function(l,name=""){
  if(is.list(l)){
    #     cat("#### ",name,"\n")
    invisible(mapply(print_nested_tables,l,paste(name,names(l))))
  } else {
    i <<-i+1
    cat("<td>",name,"\n")
    print(xtable(l,caption="", digits=6))
    cat("</td>\n")
  }
}

# get_nested_tables <- function(l1,name=""){
#   if(is.list(l1)){
#     #     cat("#### ",name,"\n")
#     lapply(
#       mapply(get_nested_tables,l1,name=paste(name,names(l1))),
#       function(e)if(is.list(e))unlist(e,recursive=FALSE))
#   } else {
#     i <<-i+1
#     list(name=name, table=xtable(l1,digits=6))
#   }
# }

tree <- function(l,name=""){
  if(is.list(l)){
    #     cat(name,"\n")
    mapply(tree,l,paste(name,names(l)))
    invisible()
  } else cat(name,": ",class(l),"\n")
}

#' write tables to xlsx
#'
#' \code{tbls2xlsx} takes a nested list of tables and writes them into a sheet
#' in an xlsx workbook object, optionally saving it to file.
#'
#' Originally meant to save drill output to xlsx for making charts, this
#' function traverses a (tree) list of tables and writes them, in the order
#' encountered, into a single column on a sheet object from package \code{xlsx}.
#' Each table must be a matrix and is written with a title formed from its
#' position in the list, or if the attribute \code{title} is set, that is used.
#' If \code{sheet} is not specified, a new one is created; if wb is not
#' specified, a new one is created.  If \code{filename} is specified, the
#' resultant workbook is written to file.
#'
#' @param tbls a (nested) list of tables
#' @param filename path to write the results to
#' @param sheet the sheet object to write the tables to
#' @param wb the workbook in which to create or find the sheet
#' @return the resultant \code{workbook} object
#'
tbls2xls <- function(tbls, filename=NULL,
                     sheet=createSheet(wb,sheetName = "tables"),
                     wb=createWorkbook()){
  #   browser()
  s <- NULL
  if(is.character(sheet)){
    s <- getSheets(wb)[[sheet]]
    if(is.null(s)) sheet <- createSheet(wb,sheet) else sheet <- s
  }
  options(table.display.type="xlsx")
  options(table.display.single.sheet=TRUE)
  options(table.display.default.workbook=wb)
  currentRow <- 1
  for(i in 1:length(tbls)){
    title <- attr(tbls[[i]],"title")
    if(is.null(title)) title <- names(tbls)[i]
    currentRow <- prntbl(title,t=tbls[[i]],sheet=sheet,currentRow=currentRow,
                         prn=FALSE)
  }
  if(!is.null(filename)) saveWorkbook(wb,filename)
  invisible(wb)
}
