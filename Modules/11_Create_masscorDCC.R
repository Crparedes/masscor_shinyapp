manageDCC.UI <- function(id) {
  ns <- NS(id)
  column(
    width = 10, offset = 1,
    h3(tags$b('Digital Calibration Certificates for Non-Automatic Weighing Instruments with the R package masscor')),
    h4('Create a new masscor NAWI DCC or upload a formely created one.'), tags$br(),
    tabBox(
      id = ns('manageDCC.TB'), title = NULL, width = 12, side = 'left',
      tabPanel(
        title = 'Upload a masscor NAWI DCC',  
        fluidRow(column(
          4, offset = 1, tags$hr(),
          fileInput(ns('NAWI.DCC_uploaded'), label = 'Choose a (.rds) NAWI DCC created with masscor:', multiple = FALSE, accept = '.rds', width = '100%')))
      ),
      tabPanel(
        title = 'Create new masscor NAWI DCC', #tags$br(),
        fluidRow(
          column(
            6,
            box(
              width = 12, title = h4('NAWI DCC main components:'), tags$hr(), status = 'primary',
              div(class = "tabBox2", 
                tabBox(
                  id = ns('createDCC.TB'), title = NULL, width = 12, side = 'left', 
                  tabPanel(
                    title = 'Administrative data', tags$br(),
                    tags$div(
                      #id = "inlineBOT", 
                      style = 'font-size:13px; width: 100%',
                      textInput(inputId = ns('institution'), label = ReqField('Calibrating laboratory'), width = '100%'),
                      splitLayout(cellWidths = c('30%', '70%'),
                        checkboxInput(inputId = ns('bolLogo'), label = 'Include institution logo', value = FALSE),
                        conditionalPanel(
                          'input.bolLogo', ns = ns,
                          fileInput(ns('InstitutLogo'), label = 'Choose institutional logo (.png):', multiple = FALSE, accept = '.png', width = '100%'))),
                      textInput(inputId = ns('accreditation'), label = ReqField('Accredited by'), width = '100%'),
                      textInput(inputId = ns('balanceID'), label = ReqField('NAWI name, description'), width = '100%'),
                      textInput(inputId = ns('serial'), label = ReqField('NAWI serial'), width = '100%'),
                      textInput(inputId = ns('certificate'), label = ReqField('Certificate number'), width = '100%'),
                      dateInput(inputId = ns('date'), label = ReqField('Calibration date'), width = '100%'),
                      tags$br()
                    )
                  ),
                  
                  tabPanel(
                    title = 'Measurement results',
                    tags$br()
                  ),
                  
                  tabPanel(
                    title = 'Comments',
                    tags$br()
                  )
                )))),
          column(
            6, box(width = 12, title = h4('Human-readable document preview:'), tags$hr()))
          )
        )
    )
  )
}

# Cargar im'agenes png https://www.rdocumentation.org/packages/png/versions/0.1-7/topics/readPNG
manageDCC.Server <- function(input, output, session) {
  # includeMarkdown("about.md")
}