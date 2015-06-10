# get login info from file: uname, pwd
load("data/auth.Rdata")

# get a token for the wise service (sheets) from google
auth <- getGoogleAuth(uname,pwd,"wise")

# use the token to get a (ephemeral) connection to the service
conn <- getGoogleDocsConnection(auth)

# get a list of all our docs
docs <- getDocs(conn)

# get the survey template workbook
wb <- docs[["svy template"]]

# get all the sheets in the workbook
sheets <- getWorksheets(wb,conn)

# convert "survey" sheet to a dataframe
survey <- as.data.frame(sheets[["survey"]],stringsAsFactors=FALSE)

# fill in missing fields for an xls form
xlsform <- makeXLSForm(survey)

# create the pretty form
xlsform[["pretty"]] <- format.xlsform(xlsform[["survey"]])

# update the svy template
modifySheet(xlsform[["survey"]],sheets[["survey"]])
modifySheet(xlsform[["choices"]],sheets[["choices"]])
modifySheet(xlsform[["pretty"]],sheets[["pretty"]])

# put it back the way it was
modifySheet(survey,sheets[["survey"]])
modifySheet(data.frame(),sheets[["choices"]])
modifySheet(data.frame(),sheets[["pretty"]])

