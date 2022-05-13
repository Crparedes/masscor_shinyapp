rm(list=ls())
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(ggplot2) #Grammar of graphics
library(ggfortify)
library(rhandsontable)
library(masscor)
library(png)
# icon("flask")

modules <- with(list(pt = 'Modules/'), paste0(pt, list.files(path = pt)))
sapply(c(modules), source)

ui <- function(request) {
  withMathJax()
  navbarPage(title = title, windowTitle = 'masscor Graphical User Interface', id = 'MainNavTabs', selected = 'Home', 
             theme = shinytheme("flatly"), position = 'fixed-top', collapsible = TRUE, lang = 'en',
             tabPanel(title = 'Home', icon = icon('compass'), tags$hr(), homeMasscor.UI(id = 'homeMasscor')),
             tabPanel(title = 'Create/upload a NAWI DCC', icon = icon('certificate'), tags$hr(), manageDCC.UI(id = 'manageDCC')),
             tabPanel(title = 'Conventional mass correction', icon = icon('ethernet'), tags$hr(), conventionalMass.UI(id = 'conventionalMass')),
             tabPanel(title = 'Air buoyancy correction factors', icon = icon('leaf'), tags$hr(), conventionalMass.UI(id = 'buoyancy')),
             headTags # mainly css code
  )
}

server <- function(input, output, session, devMode = TRUE) {
  devMode <- reactive(input$Desarrollador)
  fecha <- reactive(input$Fecha)
  output$brwz <- renderUI(
    if(devMode()) return(actionButton(inputId = 'brwz', label = tags$b('Pausar titulaR'), width = '90%')))
  observeEvent(input$brwz, browser())
  
  IDUsuario  <- reactive(c(input$nombre, input$correo))
  
  observeEvent(input$Start1, updateTabItems(inputId = 'tabs', selected = 'MRC_DisTab'))
  
  
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
