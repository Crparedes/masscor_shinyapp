manageDCC.UI <- function(id) {
  ns <- NS(id)
  column(
    width = 10, offset = 1,
    h3(tags$b('Digital Calibration Certificates for Non-Automatic Weighing Instruments with the R package masscor')),
    h4('Create a new masscor NAWI DCC or upload a formely created one.'), tags$br(),
    actionButton(inputId = ns('brwzInsideModule'), label = tags$b('Browser() - inside module')), #Eliminar esta linea
    tabBox(
      id = ns('manageDCC.TB'), title = NULL, width = 12, side = 'left', selected = 'Create new masscor NAWI DCC',
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
                          tags$div(id = "inline", fileInput(ns('InstitutLogo'), label = NULL, multiple = FALSE, accept = '.png', width = '100%', placeholder = 'Only PNG allowed')))),
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
                    tags$hr(),
                    h5(tags$b('Balance readability')),
                    tags$div(id = "inline", #style = 'font-size:12px',
                             fluidRow(
                               column(6, fluidRow(column(11, offset = 1, numericInput(ns('d'), label = ReqField('Smallest scale division:'), min = 0, value = NULL, step = 0.01)))),
                               column(6, radioButtons(ns('d_units'), label = NULL, choices = names(unitsOpt), selected = 'mg', inline = TRUE)))),
                    tags$hr(), tags$hr(),
                    h5(tags$b('Repeatability test')),
                    tags$div(id = "inline", 
                             fluidRow(
                               column(6, fluidRow(column(11, offset = 1, 
                                                         numericInput(ns('ReapTestPoints'), label = ReqField('No. of repeatiility test loads'), min = 1, max = 10, value = 2, step = 1),
                                                         numericInput(ns('ReapTestMeaPerPoints'), label = ReqField('No. of indications per load'), min = 3, max = 20, value = 10, step = 1)))),
                               #radioButtons(ns('ReapTestApproach'), label = ReqField('Number of repeatiility test points'), min = 1, max = 10, value = 10, step = 1),
                               column(6, '(Set these values before entering data in the table)'))), 
                    tags$br(),
                    rHandsontableOutput(ns("HT.repeatability"), width = '100%'),
                    tags$div(id = "inline", radioButtons(ns('repUnits'), label = 'Units of values in table', choices = names(unitsOpt), selected = 'g', inline = TRUE)),
                    tags$hr(), tags$hr(),
                    h5(tags$b('Excentricity test')), 
                    fluidRow(
                      column(3, offset = 1, div(img(src = "eccen.png", width = '90%'), style = "text-align: center;")),
                      column(7, offset = 1, 
                             rHandsontableOutput(ns("HT.eccen"), width = '100%'),
                             tags$div(id = "inline", radioButtons(ns('eccenUnits'), label = 'Units of values in table', choices = names(unitsOpt), selected = 'g', inline = TRUE)))),
                    tags$hr(), tags$hr(),
                    h5(tags$b('Indication error test')), 
                    tags$div(id = "inline", 
                             fluidRow(
                               column(6, fluidRow(column(11, offset = 1, numericInput(ns('IndErrorPoints'), label = ReqField('No. of calibration points'), min = 2, max = 20, value = 10, step = 1, width = '100%')))),
                               column(6, '(Set this value before entering data in the table)'))),
                    tags$br(),
                    rHandsontableOutput(ns("HT.indicationError")),
                    tags$div(id = "inline", numericInput(ns('IndErrorK'), label = ReqField('Expanded uncertainty coverage factor'), min = 1, max = 10, value = 2, step = 1)),
                    tags$hr(), tags$hr(),
                    h5(tags$b('Environmental conditions during calibration')), 
                    
                    tags$hr(), tags$hr(),
                    splitLayout(
                      actionButton(inputId = ns('Go2Comments'), label = tags$b('Go to comments'), width = '100%'),
                      actionButton(inputId = ns('FinishNAWIDCC1'), label = tags$b('Finish NAWI DCC'), width = '100%')),
                    tags$br(), HTML(spcs(1))
                  ),
                  
                  tabPanel(
                    title = 'Comments',
                    tags$br(),
                    tags$br(),
                    splitLayout(
                      HTML(spcs(1)),
                      actionButton(inputId = ns('FinishNAWIDCC2'), label = tags$b('Finish NAWI DCC'), width = '100%')),
                    tags$br(), HTML(spcs(1))
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