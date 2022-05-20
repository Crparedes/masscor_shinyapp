rm(list = ls())   # Delete all objects in workspace
gc()            # Garbage collector

library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2) #Grammar of graphics
library(ggfortify)
library(rhandsontable)
# library(data.table)
library(masscor)
library(png)
# icon("flask")

modules <- with(list(pt = 'Modules/'), paste0(pt, list.files(path = pt)))
sapply(c(modules), source)

ui <- fluidPage(
  withMathJax(),
  div(class = "navbar2", 
      navbarPage(windowTitle = 'masscor Graphical User Interface', title = Information, position = 'fixed-bottom', theme = shinytheme("flatly"))),
  navbarPage(
    title = title, windowTitle = 'masscor Graphical User Interface', id = 'MainNavTabs',# selected = 'Home',
    theme = shinytheme("flatly"), position = 'fixed-top', collapsible = TRUE, lang = 'en',
    tabPanel(title = HTML('Home<br>&nbsp;'), icon = icon('compass'), tags$hr(), homeMasscor.UI(id = 'homeMasscor'), 
             actionButton(inputId = 'brwz1', label = tags$b('Browser()')) #Eliminar esta linea
             ),
    tabPanel(title = HTML('Create/upload <br>', spcs(7), 'NAWI DCC'), icon = icon('certificate'), tags$hr(), manageDCC.UI(id = 'manageDCC'), 
             actionButton(inputId = 'brwz2', label = tags$b('Browser()')) #Eliminar esta linea
             ),
    tabPanel(title = HTML('Conventional <br>', spcs(7), 'mass correction'), icon = icon('ethernet'), tags$hr(), conventionalMass.UI(id = 'conventionalMass')),
    tabPanel(title = HTML('Air buoyancy <br>', spcs(6), 'correction factors'), icon = icon('leaf'), tags$hr(), conventionalMass.UI(id = 'buoyancy')),
    headTags1, headTags2, headTags3 # mainly css code
  )
)

server <- function(input, output, session, devMode = TRUE) {

  
  fecha <- reactive(input$Fecha)
  
  observeEvent(input$brwz1 | input$brwz2, browser(), ignoreInit = TRUE)
  
  IDUsuario  <- reactive(c(input$nombre, input$correo))
  observeEvent(input$Start1, updateTabItems(inputId = 'tabs', selected = 'MRC_DisTab'))
  
  
  callModule(module = manageDCC.Server, id = 'manageDCC')
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
