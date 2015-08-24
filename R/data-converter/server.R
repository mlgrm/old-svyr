library(svyr)

shinyServer(function(input,output){
  output$data <- downloadHandler(
    filename=function()sub("\\.csv$","-converted.csv", input$data.set$name),
    content=function(file){
      if(is.null(input$data.set) || is.null(input$json.form))return(NULL)
      #browser()
      df <- svy2df(input$data.set$datapath,input$json.form$datapath)
      write.csv(df,file,na="")
    }
  )
})

svy2df <- function(dat,json,questionsAsHeaders=FALSE){
  s <- load.svy(dat,json)
  if(questionsAsHeaders){}
  colnames(s) <- gsub("G[0-9]+\\.","",colnames(s))
  s <- as.data.frame(lapply(s,function(c){
    if(is(c,"AsIs")){
      class(c) <- "matrix"
      colnames(c) <- attr(c,"choices")
      #       if(is.data.frame(attributes(c)$children[[1]]$label))
      #         colnames(c) <- attributes(c)$children[[1]]$label$English else
      #           colnames(c) <- sapply(attributes(c)$children[[1]]$label,
      #                                 function(e)if(is.list(e)) e$English else e)
    }
    #browser(expr=is.matrix(c))
    c
  }
  ))#,optional=TRUE)
  #browser()
  s
}
