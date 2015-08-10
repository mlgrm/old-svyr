library(svyr)

shinyServer(function(input,output){
  output$data <- downloadHandler(
    filename=function()sub("\\.csv$","\\.xlsx", input$data.set$name),
    content=function(file){
      if(is.null(input$data.set) || is.null(input$json.form))return(NULL)
      #browser()
      s <- load.svy(input$data.set$datapath,input$json.form$datapath)
      s <- as.data.frame(lapply(s,function(c){
        if(is(c,"AsIs")){
          class(c) <- "matrix"
          colnames(c) <- sapply(attributes(c)$children[[1]]$label,
                                function(e)if(is.list(e)) e$English else e)
        }
        #browser(expr=is.matrix(c))
        c
      }
      ),optional=TRUE)
      write.xlsx(as.data.frame.data.frame(s),file,showNA=FALSE)
    }
  )
})
