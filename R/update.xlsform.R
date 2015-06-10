load("data/auth.Rdata")
update.xlsform <- function(sheetid){
  # get a token for the wise service (sheets) from google
  auth <- getGoogleAuth(uname,pwd,"wise")

  # use the token to get a (ephemeral) connection to the service
  conn <- getGoogleDocsConnection(auth)

  # get a list of all our docs
  docs <- getDocs(conn)

  wb <- docs[[sheetid %in% sapply(docs,function(d).getid(d)==sheetid)]]
}

.getid <- function(sh) gsub(".*/","",sh@id)
