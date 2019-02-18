library(shiny)

shinyServer(
  function(input,output,session){
    ####
    data <- reactive({
      file1 <- input$file
      if(is.null(file1)){return()}
      read.table(file=file1$datapath,
                 sep=input$sep,
                 dec=input$dec,
                 header = input$header,
                 stringsAsFactors = input$stringAsFactors)
    })
    ####
    output$filedf <- renderTable({
      if(is.null(data())){return()}
      input$file
    })
    ####
    output$sum <- renderPrint({
      if(is.null(data())){return()}
      summary(data())
    })
    ####
    output$table <- renderTable({
      if(is.null(data())){return()}
      data()
    })
    ####
    output$selectx <-renderUI({
      selectInput("x",label = "Axe x",choices = names(data()))
    })
    ####
    output$selecty<-renderUI({
      selectInput("y",label = "Axe y",choices = names(data()))
    })
    ####
    #output$plot = renderPlot(plot(data()[,input$x],data()[,input$y],xlab = input$x,ylab = input$y))
    output$plot = renderPlot(boxplot(data()[,input$x]~data()[,input$y],xlab=input$x,ylab=input$y))
    #condition à mettre!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ####
    output$tb <- renderUI({
      if(is.null(data())){
        h5("Powered by",tags$img(src='RStudio-Ball.png',height=200,width=200))
      }
      else
        tabsetPanel(
          tabPanel("A propos",tableOutput("filedf")),
          tabPanel("Les données",tableOutput("table")),
          tabPanel("Résumé",verbatimTextOutput("sum")),
          tabPanel("Graph",plotOutput("plot"))
          )
    })
    ####
    
  }
)