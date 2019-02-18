library(shiny)
library (sp)
library (gstat)



if (interactive()) {
  
  
  # Define UI for application that draws a histogram
  ui <- navbarPage("Test 1",
                   navbarMenu("Selectioner votre fichier",
                              tabPanel("Telechargement du fichier",
                                        sidebarLayout(sidebarPanel(
                                        tags$h5("Selectioner votre fichier"),
                                        fileInput("file1", "Choose CSV or text File",
                                           accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                        ".csv")),
                                        tags$h5("Option"),
                                        checkboxInput("header", "Header", TRUE),
                                        radioButtons("fileformat", "Extension du fichier",
                                           c(".txt" = "txt",
                                             ".csv" = "csv")),
                                        radioButtons("sep", "choix du separateur",
                                           c(";" = "pv",
                                             "," = "v"))
                                       ),
                                       mainPanel(verbatimTextOutput("head"))
                                       )),      
                                       
                                       
                              tabPanel("Visualisation complete",
                                        tableOutput("contents")
                                       )),
                   
                   tabPanel("Summary",verbatimTextOutput("summary")),
                   
                   tabPanel("Analyse multivariee",
                              sidebarLayout(sidebarPanel( tags$h4("Plot option"),
                              checkboxInput("scale", "Scale", TRUE),
                              selectInput('Cols', 'Selection de la colonne', ""),
                              radioButtons("plot", "Choix du type de plot",
                                  c("plot" = "plt",
                                  "Comparaison deux a deux" = "aplot",
                                  "density plot" = "dplot",
                                  "boxplot" = "bplot"))),

                              mainPanel(
                              plotOutput("multinorm"),
                              plotOutput("distPlot")))
                   ),
                   navbarMenu("Interpolation spatiale",
                        tabPanel("Histogramme de frequence",
                            sidebarLayout(sidebarPanel(
                              tags$h5("Spatial option"),
                              selectInput('spr', 'Selection de Species richness', "")
                              ),
                            mainPanel(tags$h3("histogramme de frequence"),plotOutput("histospa"))
                            )
                        ),
                        tabPanel("graphique Spatial(part 1)",
                                 sidebarLayout(sidebarPanel(
                                   selectInput('Xx', 'Selection de X', ""),
                                   selectInput('Yy', 'Selection de Y', ""),
                                   radioButtons("spaceplot", "Choix du type de plot",
                                                c("plot" = "plt",
                                                  "bubble" = "bub",
                                                  "spplot" = "spplt"))  
                                   
                                 ),
                                 mainPanel(tags$h3("graph spatial"),plotOutput("firstspplot"))
                                 )),
                        tabPanel("graphique Spatial(part 2)",
                                 sidebarLayout(sidebarPanel(
                                   sliderInput("grx",
                                               "cellsize X:",
                                               min = 1,
                                               max = 1000,
                                               value = 10),
                                   sliderInput("gry",
                                               "cellsize y:",
                                                min = 1,
                                                max = 1000,
                                                value = 10),
                                   sliderInput("order",
                                               "TSA order:",
                                               min = 1,
                                               max = 3,
                                               value = 1)
                                 ),
                                 mainPanel(tags$h3("graph species richness (TSA)"),plotOutput("tsa"))
                                 )
                        
                        
                        
                        )
                        )
                   
                   )
                             
                   
  

    



  
  # Define server logic required to draw a histogram
  server <- function(input, output,session) {
    
    
    
    #####################################input CSV or txt######################################       
    sepa<- reactive({
      switch(input$sep,
          pv =  ";",
          v =  ",")
      
    })
    
    
    inputcontents <- reactive({
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      separateur <- sepa()
      switch(input$fileformat,
             txt = read.table(inFile$datapath, header = input$header)->matable,
             csv = read.table(inFile$datapath, header = input$header, sep = separateur)#read.csv(inFile$datapath, header = input$header ,col)
      )})
    
    ####################################Affichage de mon fichier###############################    
    
    output$contents <- renderTable({
      inputcontents()
    })
    
    output$head <- renderPrint({
      if (is.null(inputcontents()))
        return(NULL)
      head(inputcontents())
    })
    
    
    ###################################Summary#################################################    
    
    output$summary <- renderPrint({
      if (is.null(inputcontents()))
        return(NULL)
      summary(inputcontents())
      })
    
    ##################################choix optimiser pour le dataset##########################    
    
    choixreact <-reactive({
      data<-inputcontents()
      updateSelectInput(session, "Cols",choices = colnames( data)) })
    

    
    
    #################################Density plot##############################################    
    
    output$multinorm <- renderPlot({
      
        if (is.null(inputcontents()))
          return(NULL)
        data<- inputcontents()
        choixreact()
        choix<- input$Cols
        choix2 <- as.character(choix)
        scaletest<-input$scale
        if(isTRUE(scaletest)){plotdata<-scale(as.numeric(data[, grep(choix2, colnames(data))] ))}
        else{plotdata <-as.numeric(data[, grep(choix2, colnames(data))] )}
        plot1<-density(plotdata,na.rm=TRUE )
        plot(plot1,main = choix)
        switch(input$plot,
               plt = plot(plotdata,main = choix),
               aplot = plot(data),
               dplot = plot(plot1,main = choix),
               bplot = boxplot(data)  )                
      })
    

    choixreactSpR <-reactive({
      data<-inputcontents()
      updateSelectInput(session, "spr",choices = colnames( data)) })
    
    

    
    
    
    output$histospa <- renderPlot({
      if (is.null(inputcontents()))
        return(NULL)
      data<- inputcontents()
      print(data)
      choixreactSpR()
      choix<- input$spr
      choixfinal<-data[, grep(choix, colnames(data))]
      plot(hist(choixfinal),main = choix)
      
    })
    
    
    choixreactX <-reactive({
      data<-inputcontents()
      updateSelectInput(session, "Xx",choices = colnames(data)) })

    choixreactY <-reactive({
      data<-inputcontents()
      updateSelectInput(session, "Yy",choices = colnames(data)) })


    # 
     CoordonneX <- function(){
       if (is.null(inputcontents()))
         return(NULL)
       data<- inputcontents()
       choixreactX()
      choixX<- input$Xx
       coordX<- data[, grep(choixX, colnames(data))]
       return(coordX)
       }
     
     CoordonneY <- function(){
       if (is.null(inputcontents()))
         return(NULL)
      data<- inputcontents()
       choixreactY()
       choixY<- input$Yy
       coordY<-data[, grep(choixY, colnames(data))]
       return(coordY)
      }
    
    
    
     coordinatesfinal<-function(){
       if (is.null(inputcontents()))
         return(NULL)
       data<- inputcontents() 
       data2<-data
       coordX <-CoordonneX()
       choixX<- input$Xx
      coordY <-CoordonneY()
      choixY<- input$Yy
      coordinates(data2) = (data[, c(grep(choixX, colnames(data)),grep(choixY, colnames(data)))])
       print(class(test))
       return(test)
     }
    
    
    
    
    
    output$firstspplot <-renderPlot({
      if (is.null(inputcontents()))
        return(NULL)
      data<- inputcontents()  
      data2<-data
      choixreactX()
      choixX<- input$Xx
      print(choixX)
      choixreactY()
      choixY<- input$Yy
      coordinates(data2) = (data[, c(grep(choixX, colnames(data)),grep(choixY, colnames(data)))])
      switch(input$spaceplot,
             plt =  plot (data2),
             bub = bubble (data2, "SpRichness", col = c("#00ff0088", "#00ff0088"), main = "Species richness"),
             spplt = spplot (data2, "SpRichness", main = "Species richness")) 
    })
    
    znsppol <- function(){
      if (is.null(inputcontents()))
        return(NULL)
      data<- inputcontents()
      choixreactX()
      choixX<- input$Xx
      choixreactY()
      choixY<- input$Yy
      XY<-(data[, c(grep(choixX, colnames(data)),grep(choixY, colnames(data)))])
      Zp <- Polygon (as.matrix (XY), hole = TRUE)
      Zps <- Polygons (list (Zp), ID = "Polyg")
      Zsp <- SpatialPolygons (list (Zps))
      return(Zsp)
    }   
   
    SpRichnesstsa <- function(){
      data<- inputcontents()
      Zsp <- znsppol()
      gx<- input$grx
      gy<- input$gry
      Zone.grid <- spsample (Zsp, cellsize = c(gx,gy), type = "regular", offset = c(0, 0))
      orders <-  input$order
      donnees<- coordinatesfinal()
      SpRichness<- data[, grep(input$spr, colnames(data))]
      SpRichn<-krige (SpRichness , donnees, Zone.grid, degree = orders)
      return(SpRichn)
      }
    
    output$tsa <- renderPlot({
      SpRichn<-SpRichnesstsa()
      spplot (SpRichn, main = "Species richness (order TSA interpolation)")
      
    })
   
    ######################################################################    
    
    #  output$distPlot <- renderPlot({
    #   # generate bins based on input$bins from ui.R
    #    x    <- faithful[, 2] 
    #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    #  })
    
    #######################################################################    
    
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  
}
