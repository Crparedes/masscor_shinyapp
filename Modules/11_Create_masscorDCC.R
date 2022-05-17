manageDCC.UI <- function(id) {
  ns <- NS(id)
  column(
    width = 10, offset = 1,
    h3(tags$b('Digital Calibration Certificates for Non-Automatic Weighing Instruments with the R package masscor')),
    h4('Create a new masscor NAWI DCC or upload a formely created one.'), tags$br(),
    actionButton(inputId = ns('brwzInsideModule'), label = tags$b('Browser() - inside module')), #Eliminar esta linea
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
                      tags$br(),
                      actionButton(inputId = ns('Go2MeasRes'), label = tags$b('Go to measurement results'), width = '50%'),
                      tags$br(), HTML(spcs(1))
                    )
                  ),
                  
                  tabPanel(
                    title = 'Measurement results',
                    tags$br(),
                    tags$br(),
                    splitLayout(
                      actionButton(inputId = ns('Go2Comments'), label = tags$b('Go to comments'), width = '100%'),
                      actionButton(inputId = ns('FinishNAWIDCC1'), label = tags$b('Finish NAWI DCC'), width = '100%')),
                    tags$br()
                  ),
                  
                  tabPanel(
                    title = 'Comments',
                    tags$br(),
                    tags$br(),
                    splitLayout(
                      HTML(spcs(1)),
                      actionButton(inputId = ns('FinishNAWIDCC2'), label = tags$b('Finish NAWI DCC'), width = '100%')),
                    tags$br()
                  )
                )),
              tags$br()
              )),
          column(
            6, box(width = 12, title = h4('Human-readable document preview:'), tags$hr()))
          )
        )
    ), 
    column(1,tags$br()), 
    column(12, tags$hr(), tags$hr(), tags$hr())
  )
}

# Cargar im'agenes png https://www.rdocumentation.org/packages/png/versions/0.1-7/topics/readPNG
manageDCC.Server <- function(input, output, session) {
  observeEvent(input$brwzInsideModule, browser())
  
  BoleanIncompleteAdminDat <- reactive(all(is.null.empty(input$institution)))
  BoleanIncompleteMeasurRes <- reactive(all(is.null.empty(input$calib)))
  observeEvent(input$Go2MeasRes, ignoreInit = TRUE,
               if (BoleanIncompleteAdminDat()) {
                 showNotification('Please complete the required fields of administrative data', duration = 3, type = 'error')
               } else {
                 updateTabItems(inputId = 'createDCC.TB', selected = 'Measurement results')
               })
  observeEvent(input$createDCC.TB, ignoreInit = TRUE,
               if (BoleanIncompleteAdminDat()) {
                 showNotification('Please complete the required fields of administrative data', duration = 3, type = 'error')
                 updateTabItems(inputId = 'createDCC.TB', selected = 'Administrative data')
               } else {
                 if (BoleanIncompleteMeasurRes() && input$createDCC.TB == 'Comments') {
                   showNotification('Please complete the required fields of measurement results', duration = 3, type = 'error')
                   updateTabItems(inputId = 'createDCC.TB', selected = 'Measurement results')
                 }
               })             
  
  observeEvent(input$Go2Comments, ignoreInit = TRUE, updateTabItems(inputId = 'createDCC.TB', selected = 'Comments'))
  
  observeEvent(input$Go2MeasRes, ignoreInit = TRUE,
               if (is.null.empty(input$institution)) {
                 showNotification('Please complete the required fields of measurement results', duration = 3, type = 'error')
               } else {
                 updateTabItems(inputId = 'createDCC.TB', selected = 'Measurement results')
               })

    # includeMarkdown("about.md")
}