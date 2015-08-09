library(svyr)

shinyServer(function(input,output){
  output$data <- downloadHandler(
    filename=function()sub("\\.csv$","\\.xlsx", input$data.set$name),
    content=function(file){
      if(is.null(input$data.set) || is.null(input$json.form))return(NULL)
      #browser()
      s <- load.svy(input$data.set$datapath,input$json.form$datapath)
      write.xlsx(as.data.frame.data.frame(s),file,showNA=FALSE)
    }
  )
})
