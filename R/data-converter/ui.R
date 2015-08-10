txt <- p(
  'this is a tool for converting data sets produced by formhub using an xlsform
  created using the',
  a(href='../simple-form-ui/',"simple survey tool"),
  'into an format that\'s a little  easier to read.  To use it, download your
  data in csv format from the formhub server as well as the JSONform
  specification.  You can find this by clicking on the name of your survey,
  and then, under "XLSform", clicking on "JSONForm".  The data set and the
  JSONForm must come from the exact same version of the xlsform'
  )
shinyUI(fluidPage(
  titlePanel("data converter for the simple survey tool"),
  verticalLayout(
    mainPanel(
      txt,
      fileInput('data.set', 'Choose a data file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'
                )
              ),
      fileInput("json.form", "Choose a json form to upload",
                accept = '.json'),
#       checkboxInput("questionsAsHeaders", "Would you like the questions to
#                     appear in the column headers?"),
      tags$hr(),
      downloadButton('data', 'Download converted data set')
    )
  )
))
