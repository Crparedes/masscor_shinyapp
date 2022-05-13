manageDCC.UI <- function(id) {
  ns <- NS(id)
    column(width = 10, offset = 1,
           h3(tags$b('Digital Calibration Certificates for Non-Automatic Weighing Instruments with the R package masscor')),
           h4('Create a new NAWI DCC or upload a formely created one.'), tags$br(),
           tabBox(id = ns('manageDCC.TB'), title = NULL, width = 12, side = 'left',
                  tabPanel(title = 'Upload',
                           fluidRow(
                             column(4, 
                                    fileInput(ns('NAWI.DCC_uploaded'), label = 'Choose a (.rds) NAWI DCC created with masscor package:', multiple = FALSE, accept = '.rds'))), 
                           tags$br()),
                  tabPanel(title = 'Create new DCC', #tags$br(),
                           fluidRow(
                             column(6,
                                    box(width = 12, title = h4('NAWI DCC main components:'), tags$br(),
                                        tabBox(id = ns('createDCC.TB'), title = NULL, width = 12, side = 'left', 
                                               tabPanel(title = 'Administrative data', 
                                                        tags$div(id = "inline", style = 'font-size:12px',
                                                                 fileInput(ns('NAWI.DCC_uploaded'), label = 'Choose a (.rds) NAWI DCC created with masscor package:', multiple = FALSE, accept = '.rds'))),
                                               tabPanel(title = 'Measurement results'),
                                               tabPanel(title = 'Comments')
                                        ))),
                             column(6,
                                    box(width = 12, title = h4('Human-readable document preview:'), tags$br())))
                           )
                  )
           )
  
}


# Cargar im'agenes png https://www.rdocumentation.org/packages/png/versions/0.1-7/topics/readPNG
manageDCC.Server <- function(input, output, session) {
  # includeMarkdown("about.md")
}