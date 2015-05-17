#options(svy.entry.template=paste(readLines(file("~/svy/data/entry_template.xml")),
#                                 collapse="\n"))
library(RCurl)
library(XML)

modifySheet <- function(sh,mod,...) UseMethod("modifySheet")

#options(svy.feed.template=paste(readLines(file("~/svy/data/feed_template.xml")),
#                                collapse="\n"))
#' modify the contents of a sheet
#'
#' takes a sheet object and a function and changes the contents of the sheet
#' using the funtion
#'
#' @param sheet a sheet object
#' @param mod a function that takes a data frame and returns another, or another
#' data.frame
#' @param ... additional parameters to the function
#' @return invisibly, the server's response to the put request
#'
#' The sheet contents are fetched and coerced into a data.frame, the function
#' called on the data.frame, and the results posted to the sheet, replacing the
#' original contents.  If mod is a data.frame, the contents of the sheet are
#' simply overwritten
#'
modifySheet <- function(mod,sheet,...) UseMethod("modifySheet")

modifySheet.function <- function(mod,sheet,...){
  df <- as.data.frame(sheet)
  df <- mod(df,...)
  req <- createUpdateRequest(df,sheet)
  invisible(putRequest(sheet,req))
}

modifySheet.data.frame <- function(mod,sheet){
  invisible(putRequest(sheet,createUpdateRequest(mod,sheet)))
}

as.data.frame <- function(x,...) UseMethod("as.data.frame")

#' coerce a worksheet object into a data.frame
#'
#' this is just a wrapper for \code{RGoogleDocs}'s \code{sheetAsMatrix}
#' function with the exception that the error caused by an empty sheet
#' in that function results in an empty data.frame here
#'
as.data.frame.GoogleWorksheetRef <- function(x,header=TRUE,trim=FALSE,...){
  r <- suppressWarnings(
    tryCatch(sheetAsMatrix(sh,header=header,trim=trim),error=identity))
  if(is(r,"error") && e$message=="invalid 'times' argument")
    data.frame() else r
}

pushSheet <- function(df,sheet) putRequest(sheet,createUpdateRequest(df,sheet))

getSheet <- function(wbname,shname,conn=getGoogleDocsConnection(auth),auth){
  wb <- getDocs(conn)[[wbname]]
  if(is.null(wb)) stop("workbook not found")
  sh <- getWorksheets(wb,conn)[[shname]]
  if(is.null(sh)) stop("sheet not found in workbook")
  sh
}

#' header for a GoogleSheets request
#'
#' use the information in a
header.sheet <- function(sheet) c(
  Authorization=paste0("GoogleLogin auth=", sheet@connection@auth),
  "Content-Type"="application/atom+xml",
  curl=sheet@connection,
  verbose=TRUE
)



#' retrieve cells doc
#'
#' gets an xml document from Google containing all the specified entries
#' in a sheet
#'
#' @param sheet a \code{GoogleWorksheetRef} object
#' @param extrema a vector containing the minimum row and column and the
#' maximum row and column, in that order.  NA values indicate no minimum
#' or maximum
#' @param get.empty whether to request empty cells (see note below)
#' @return an \code{XMLInternalDocument} containing the feed and its attached
#' entries
#'
#' @section Note:
#' it may be necessary to retrieve empty cells so they can be updated using
#' the version code in the \code{rel='edit'} link of the entries for those cells
#'
getCellsFeed <- function(sheet,extrema=c(NA,NA,c(dim(sheet))),
                         get.empty=any(!is.na(extrema))){
  #   extrema <- c(minr,maxr,minc,maxc)
  nextrema <- c("min-row","min-col","max-row","max-col")
  req <- paste0(sheet@cellsfeed,"?",
                paste(c(paste(nextrema,extrema,sep="=")[!is.na(extrema)],
{if(get.empty)"return-empty=true"}),
collapse="&"))
h <- basicTextGatherer()
curlPerform(url=req,
            httpheader=header.sheet(sheet),
            writefunction=h$update
)
(xmlParse(h$value()))
}

#' extract cell values
#'
#' pull a dataframe of rows, columns and values from a cells feed
#'
#' @param x an xml doc containing the entries in a cells feed
#' @return a dataframe with \code{row}, \code{col}, and \code{InputValue}
#' columns
#' @section Note:
#' the rows in the dataframe will coincide with the order of the entries in the
#' doc and thus with the order of elements in a nodeset created using
#' \code{getNodeset(x,"//def:entry",fixns(x))}
#'
getCellVals <- function(x){
  nodes <- getNodeSet(x,"//def:entry",fixns(x))
  ldply(nodes,function(n){
    xmlAttrs(getNodeSet(n,"gs:cell",fixns(x))[[1]])
  })
}

getEditLinks <- function(x) getNodeSet(x,"//def:entry/def:link[@rel='edit']",
                                       fixns(x))

#' create an update request to modify a sheet
#'
#' retrieves a cells feed document for the sheet, compares the cells to the
#' elements of the data.frame
createUpdateRequest <- function(df,sheet){
  df <- rbind(colnames(df),df)
  df[is.na(df)] <- ""
  x <- getCellsFeed(sheet,extrema=c(NA,NA,pmax(dim(sheet),dim(df))))
  mapply(function(entry,cell){
    cont <- xmlAttrs(cell)
    cont <- list(
      row=as.numeric(cont["row"]),
      col=as.numeric(cont["col"]),
      inputValue=cont["inputValue"]
    )
    browser(expr=!is(entry,"XMLInternalNode"))
    # if the entry is in the bounds of the data.frame and has the same value,
    # delete it from the list of updates
    if(cont$row<=nrow(df) &
         cont$col<=ncol(df) &
         cont$inputValue==df[cont$row,cont$col])
      removeNodes(entry) else{
        if(cont$row>nrow(df) | cont$col>ncol(df))
          # if the entry is outside the data.frame, clear it
          xmlAttrs(cell)["inputValue"] <- "" else
            # otherwise, update it to the value in the data frame
            xmlAttrs(cell)["inputValue"] <- df[cont$row,cont$col]
        addChildren(entry,
                    # make the batch:id the same as the title
                    newXMLNode("id",
                               newXMLTextNode(getChildrenStrings(
                                 searchXML(entry,"def:title",x)[[1]])),
                               namespace = "batch"
                    ),
                    newXMLNode("operation",
                               attrs=list(type="update"),
                               namespace="batch"
                    )
        )

      }
    invisible()
  },
  searchXML(x,"/def:feed/def:entry"),
  searchXML(x,"/def:feed/def:entry/gs:cell")
  )
  x
}

putRequest <- function(sheet,request){
  h <- basicTextGatherer()
  curlPerform(
    url=paste0(sheet@cellsfeed,"/batch"),
    postfields=saveXML(request),
    writefunction=h$update,
    customrequest="POST",
    httpheader=header.sheet(sheet)
  )
  h$value()
}

fixns <- function(x,defstr="def"){
  ns <- sapply(xmlNamespaceDefinitions(x),function(d)d$uri)
  names(ns) <- ifelse(names(ns)=="",defstr,names(ns))
  ns
}

searchXML <- function(x,str,doc=x,...)
  getNodeSet(x,str,namespaces = fixns(doc),...)

dim <- function(x) UseMethod("dim")
dim.GoogleWorksheetRef <- function(sheet) dim(as.data.frame(sheet))

# getCellFeed(sheetId){
#   url=paste0()
# }

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

#' get specified entry nodes from an xml node by row/col pairs
#'
#' blah
getEntries <- function(x,vals){
  # x is an xml node
  # ns is the vector of namespaces
  ns <- sapply(xmlNamespaceDefinitions(x),function(d)d$uri)
  names(ns) <- ifelse(names(ns)=="","x",names(ns))

  # use the x:entry xpath to get all the "entry" nodes
  entries <- getNodeSet(x,"//x:entry",ns)

  # get the "gs:cell" children, and put them into a row-col-val matrix
  rc <- sapply(getNodeSet(x,"//gs:cell",ns),function(cell){
    a <- xmlAttrs(cell)
    c(a[["row"]],a[["col"]],val=a[["inputValue"]])
  })

  # convert to data frame with numeric rows and columns
  ents <- data.frame(r=as.numeric(rc[1,]),c=as.numeric(rc[2,]),val=rc[3,])

  # look up the row/col pairs specified in vals
  inds <- match(with(vals,paste(r,c)),with(ents,paste(r,c)))
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
