

getSheet <- function(wb, name=NULL, as.data.frame=TRUE, header=TRUE,
                     con=getOption("GoogleDocsConnection",
                                   default = .googlecon())){
  l <- getWorksheets(wb, con)
  if(!is.null(name) && name %in% names(l)){
    if(as.data.frame) 
      sheetAsMatrix(l[[name]],header=header,con=con,trim=FALSE,
                    stringsAsFactors=FALSE) else 
      l[[name]]
  } else stop("no such sheet")
}

getWorkbook <- function(
  name=NULL,
  con=getOption("GoogleDocsConnection",default = .googlecon())
){
  l <- getDocs(con,auth=getOption("GoogleAuth", default=.googleauth()),
               service=getOption("defaultGoogleService",default="wise"))
  if(!is.na(name) && name %in% names(l)) l[[name]] else stop("not here.")
}

.googlecon <- function()
  getGoogleDocsConnection(auth=getOption("GoogleAuth", default=.googleauth()))

.googleauth <- function()
  getGoogleAuth(getOption("GoogleLogin", default=.prompt("google login")),
                password=getOption("GooglePassword",
                                   default=.prompt("google password",
                                                   masked=TRUE)),
                service=getOption("defaultGoogleService",default="wise"))


.prompt <- function(text,masked=FALSE){
  readline(prompt=text)
}