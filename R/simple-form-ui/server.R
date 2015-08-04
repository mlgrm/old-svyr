library(svyr)

shinyServer(function(input, output) {
#   browser()
#   output$filename <- sub("csv$","xlsx",input$simple.form$name)
  output$xlsform <- downloadHandler(
    filename=function()sub("csv$","xlsx",input$simple.form$name),
    content=function(file){
      if(is.null(input$simple.form))return(NULL)
      csv2form(input$simple.form$datapath,path=file)
    }
#   output$contents <- renderTable({
#     # input$file1 will be NULL initially. After the user selects
#     # and uploads a file, it will be a data frame with 'name',
#     # 'size', 'type', and 'datapath' columns. The 'datapath'
#     # column will contain the local filenames where the data can
#     # be found.
#
#     inFile <- input$simple.form
#
#    read.csv(inFile$datapath, header = input$header,
#            sep = input$sep, quote = input$quote)
  )
})
