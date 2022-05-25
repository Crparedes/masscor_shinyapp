rm(list = ls())   # Delete all objects in workspace
gc()            # Garbage collector

library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(ggplot2) #Grammar of graphics
library(ggfortify)
library(writexl)
library(rhandsontable)
# library(data.table)
library(masscor)
library(png)
library(knitr)
library(markdown)
library(htmlwidgets)
# icon("flask")

modules <- with(list(pt = 'Modules/'), paste0(pt, list.files(path = pt)))
sapply(c(modules), source)


ui <- fluidPage(
  withMathJax(),
  useShinyjs(),
  div(class = "navbar2", 
      navbarPage(windowTitle = 'masscor Graphical User Interface', title = Information, position = 'fixed-bottom', theme = shinytheme("flatly"))),
  navbarPage(
    title = title, windowTitle = 'masscor Graphical User Interface', id = 'MainNavTabs',# selected = 'Home',
    theme = shinytheme("flatly"), position = 'fixed-top', collapsible = TRUE, lang = 'en',
    tabPanel(title = HTML('Home<br>&nbsp;'), icon = icon('compass'), value = 'Home', 
             tags$hr(), tags$hr(), homeMasscor.UI(id = 'homeMasscor'),
             # actionButton(inputId = 'brwz1', label = tags$b('Browser()')) #Eliminar esta linea
             ),
    tabPanel(title = HTML('Create/upload <br>', spcs(7), 'NAWI DCC'), icon = icon('certificate'), value = 'CreateUploadDCC',
             tags$hr(), tags$hr(), manageDCC.UI(id = 'manageDCC')),
    tabPanel(title = HTML('Conventional <br>', spcs(7), 'mass correction'), icon = icon('ethernet'), 
             tags$hr(), tags$hr(), conventionalMass.UI(id = 'conventionalMass')),
    tabPanel(title = HTML('Air buoyancy <br>', spcs(6), 'correction factors'), icon = icon('leaf'), tags$hr(), tags$hr(), buoyancyCorrections.UI(id = 'MABC')),
    headTags1, headTags2, headTags3 # mainly css code
  )
)

server <- function(input, output, session, devMode = TRUE) {
  observeEvent(input$brwz1, browser(), ignoreInit = TRUE)
  
  ImportedCreated.NAWIDCC <- callModule(module = manageDCC.Server, id = 'manageDCC')
  callModule(module = conventionalMass.Server, id = 'conventionalMass', NAWIDCC = ImportedCreated.NAWIDCC, parent = session, mainNavTb = reactive(input$MainNavTabs))
  callModule(module = buoyancyCorrections.Server, id = 'MABC', NAWIDCC = ImportedCreated.NAWIDCC)
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
