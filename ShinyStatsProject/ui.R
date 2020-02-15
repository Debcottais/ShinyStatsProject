library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(
    skin = "black",
    dashboardHeader(title="Stats-perf"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Acceuil",icon=icon("home",lib = "font-awesome"),tabName = "Acceuil-fileinput"),
          menuSubItem("Statistiques inférentielles",tabName = "inferentiel"),
          menuSubItem("Titre 1.2"),
        menuItem("Titre 2"),
        menuItem("A propos",icon=icon("info-circle",lib = "font-awesome"),tabName = "About")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "Acceuil-fileinput",
                sidebarLayout(
                  sidebarPanel(
                    fileInput("file","Upload the file"),
                    helpText("Taille maximale = 5Mb"),
                    tags$hr(),
                    h5(helpText("Sélectionnez les paramètres :")),
                    checkboxInput(inputId = 'header',label = 'En-tête',value = FALSE),
                    checkboxInput(inputId = 'stringAsFactors',label = 'StringsAsFactors',value = FALSE),
                    br(),
                    radioButtons(inputId = 'sep',label='Séparateur',choices = c(Virgule=',',Point_virgule=';',Tab='\t',Espace=' '),selected = ','),
                    radioButtons(inputId = 'dec',label='Décimale', choices = c(Virgule=',',Point='.'),selected = '.' ),
                    helpText("Choisissez les variables pour avoir un aperçu graphique."),
                    uiOutput("selectx"),
                    uiOutput("selecty"),
                    uiOutput("graphtype")
                  ),
                  mainPanel(
                    uiOutput("tb")
                  )
                )
                ),
        tabItem(tabName = "About",
                p("Cet outil statistique a été développé par des étudiants de master 2 Bio-informatique de Bordeaux en 2018."),
                p("Il a été réalisé pour l'UE Statistiques perfectionnement de l'université de Bordeaux. C'est notamment grâce au package",em("'Shiny'")," dans R.")
                )
      )
    )
  )
)
