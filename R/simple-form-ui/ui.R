shinyUI(fluidPage(
  titlePanel("simple survey to xlsform converter"),
  verticalLayout(
    mainPanel(
      p('This is an interface for converting simple survey specifications',
        '(see the',
        a(href=paste0('https://sites.google.com/a/samuelhall.org/survey-tools/simplified-survey-tool'),
          "simple survey guide"), ')',
        'into xlsform files appropriate for uploading to the formhub server.',
        'The input file should be in csv format, created from excel\'s export',
        'function or google sheets\' "file -> download as -> comma separated',
        'value" command.  Once you have created your csv file, select it with',
        'choose file, and then press the download button below to save your',
        'completed xlsform file'),
      fileInput('simple.form', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'
                )
      ),
      tags$hr(),
      downloadButton('xlsform', 'Download xlsform')
    )
  )
))
