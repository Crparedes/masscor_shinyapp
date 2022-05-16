rm(list=ls())
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2) #Grammar of graphics
library(ggfortify)
library(rhandsontable)
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
    tabPanel(title = HTML('Home<br>&nbsp;'), icon = icon('compass'), tags$hr(), homeMasscor.UI(id = 'homeMasscor')),
    tabPanel(title = HTML('Create/upload <br>', spcs(7), 'NAWI DCC'), icon = icon('certificate'), tags$hr(), manageDCC.UI(id = 'manageDCC')),
    tabPanel(title = HTML('Conventional <br>', spcs(7), 'mass correction'), icon = icon('ethernet'), tags$hr(), conventionalMass.UI(id = 'conventionalMass')),
    tabPanel(title = HTML('Air buoyancy <br>', spcs(6), 'correction factors'), icon = icon('leaf'), tags$hr(), conventionalMass.UI(id = 'buoyancy')),
    headTags1, headTags2, headTags3 # mainly css code
  )
  
)


# ui <- function(request) {
# 
#   navbarPage(
#     title = title, windowTitle = 'masscor Graphical User Interface', id = 'MainNavTabs',# selected = 'Home',
#     theme = shinytheme("flatly"), position = 'fixed-top', collapsible = TRUE, lang = 'en',
#     tabPanel(title = HTML('Home<br>&nbsp;'), icon = icon('compass'), tags$hr(), homeMasscor.UI(id = 'homeMasscor')),
#     tabPanel(title = HTML('Create/upload <br>', spcs(7), 'NAWI DCC'), icon = icon('certificate'), tags$hr(), manageDCC.UI(id = 'manageDCC')),
#     tabPanel(title = HTML('Conventional <br>', spcs(7), 'mass correction'), icon = icon('ethernet'), tags$hr(), conventionalMass.UI(id = 'conventionalMass')),
#     tabPanel(title = HTML('Air buoyancy <br>', spcs(6), 'correction factors'), icon = icon('leaf'), tags$hr(), conventionalMass.UI(id = 'buoyancy')),
#     headTags # mainly css code
#   )
#   navbarPage(title = '', position = 'fixed-bottom')
# }

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
