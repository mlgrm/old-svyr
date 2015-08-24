library(svyr)

shinyServer(function(input,output){
  output$data <- downloadHandler(
    filename=function()sub("\\.csv$","\\.xlsx", input$data.set$name),
    content=function(file){
      if(is.null(input$data.set) || is.null(input$json.form))return(NULL)
      #browser()
      wb <- svy2wb(input$data.set$datapath,input$json.form$datapath)
      write.xlsx(as.data.frame.data.frame(s),file,showNA=FALSE)
    }
  )
})

svy2df <- function(dat,json,questionsAsHeaders=FALSE){
  s <- load.svy(dat,json)
  if(questionsAsHeaders)
}
