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
              div(class = "tabBox2", style = 'font-size:13px;',
                tabBox(
                  id = ns('createDCC.TB'), title = NULL, width = 12, side = 'left', 
                  tabPanel(
                    title = 'Administrative data', tags$br(),
                    tags$div(
                      style = 'width: 100%',
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
                    tags$div(id = "inlineBOT", #style = 'font-size:12px',
                             splitLayout(#cellWidths = c("75%", "25%"),
                                         numericInput(ns('d'), label = ReqField('Balance scale division:'), min = 0, value = NULL),
                                         radioButtons(ns('d_units'), label = HTML(spcs(3)), choices = names(unitsOpt), selected = 'mg', inline = TRUE))),
                    
                    tags$br(),
                    tags$b('Indication error'), tags$br(),
                    tags$div(id = "inline", 
                             sliderInput(ns('IndErrorPoints'), label = ReqField('Number of calibration points'), min = 2, max = 20, value = 11, step = 1)),
                    rHandsontableOutput(ns("HT.indicationError")), tags$br(),
                    tags$b('Repeatability test results'), tags$br(),
                    rHandsontableOutput(ns("HT.rep"), width = '100%'), tags$br(),
                    tags$b('Excentricity test results'), tags$br(),
                    rHandsontableOutput(ns("HT.eccen"), width = '100%'),
                    tags$br(),
                    splitLayout(
                      actionButton(inputId = ns('Go2Comments'), label = tags$b('Go to comments'), width = '100%'),
                      actionButton(inputId = ns('FinishNAWIDCC1'), label = tags$b('Finish NAWI DCC'), width = '100%'))
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