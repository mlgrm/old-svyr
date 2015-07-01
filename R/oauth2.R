library(httr)

get.token <- function(app=getOption("svyr_app"),use_oob=TRUE)
  oauth2.0_token(oauth_endpoints("google"),app,
                 scope="https://spreadsheets.google.com/feeds",
                 use_oob=use_oob)

get.sheetList <- function(key,token=getOption("svyr_token")){
  req <- GET(paste0("https://spreadsheets.google.com/feeds/worksheets/",
                    sheetid,"/private/full"))
}
