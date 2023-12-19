manageDCC.UI <- function(id) {
  ns <- NS(id)
  column(
    width = 10, offset = 1,
    h3(tags$b('Digital Calibration Certificates for Non-Automatic Weighing Instruments with the R package masscor')),
    h4('Create a new masscor NAWI DCC or upload a formely created one.'),
    tags$a(href = "NAWI_Example.rds", tags$b("(Download an example file to upload it to the App)"),
           download = NA, target = "_blank"),
    tags$br(),
    # actionButton(inputId = ns('brwzInsideModule'), label = tags$b('Browser() - inside module')), #Eliminar esta linea
    tags$div(id = "inline", 
             radioButtons(ns('SourceOption'), label = 'Which action will you perform?',
                          choices = list('Create new masscor NAWI DCC' = 'daCapo', "Upload a masscor NAWI DCC file" = 'file'),
                          selected = character(0), inline = TRUE)),
    niceSeparator(),
      
    fluidRow(column(
      6,
      # Conditional panel upload document
      {conditionalPanel(
        condition = 'input.SourceOption == "file"', ns = ns,
        h4('NAWI DCC File:'),
        fileInput(ns('NAWI.DCC_uploaded'), label = 'Choose a (.rds) NAWI DCC created with masscor:', multiple = FALSE, accept = '.rds', width = '100%'),
        uiOutput(ns('FinishNAWIDCC3'))
        )},
      
      # Conditional panel NAWI DCC creation
      {conditionalPanel(
        condition = 'input.SourceOption == "daCapo"', ns = ns,
        #tabPanel(
        #  title = 'Create new masscor NAWI DCC', #tags$br(),
        h4('NAWI DCC main components:'),
        div(
          class = "tabBox2", style = 'font-size:13px;',
          tabBox(
            id = ns('createDCC.TB'), title = NULL, width = 12, side = 'left',
            # Administrative Data
            {tabPanel(
              title = 'Administrative data',
              tags$div(
                style = 'width: 100%;padding: 15px;',
                h5(tags$b('Letterhead')),
              textInput(inputId = ns('institution'), label = ReqField('Calibrating laboratory'), width = '100%', placeholder = 'Name, address.'),
                splitLayout(cellWidths = c('30%', '70%'),
                            checkboxInput(inputId = ns('bolLogo'), label = 'Include institution logo', value = FALSE),
                            conditionalPanel(
                              'input.bolLogo', ns = ns,
                              tags$div(id = "inline",
                                       fileInput(ns('InstitutLogo'), label = NULL, multiple = FALSE, accept = '.png',
                                                 width = '100%', placeholder = 'Only PNG allowed')))),
                textInput(inputId = ns('accreditation'), label = NonReqField('Accredited by'), width = '100%'),
                textInput(inputId = ns('respPerson'), label = ReqField('Responsible person'), width = '100%', 
                          placeholder = 'Separate the names of different people using commas.'),
                tags$hr(),
                h5(tags$b('Object to calibrate:')),
                textInput(inputId = ns('balanceID'), label = ReqField('NAWI name, description'), width = '100%'),
                textInput(inputId = ns('serial'), label = ReqField('NAWI serial'), width = '100%'),
                tags$hr(),
                h5(tags$b('NAWI certificate information:')),
                textInput(inputId = ns('certificate'), label = ReqField('Certificate number'), width = '100%'),
                dateInput(inputId = ns('date'), label = ReqField('Calibration date'), width = '100%'),
              h5(tags$b('Calibration costumer:')),
                textInput(inputId = ns('Costumer'), label = ReqField('Costumer name and address'), width = '100%', placeholder = 'Name, address.'),
                textInput(inputId = ns('calPlace'), label = ReqField('Place of calibration'), width = '100%'),
                # Datos del personal responsable
                # lugar de calibraci'on
                #
                tags$br(),
                actionButton(inputId = ns('Go2MeasRes'), label = tags$b('Go to measurement results'), width = '50%'),
                tags$br(), HTML(spcs(1))
              ))},
            
            # Measurement results
            {tabPanel(
              title = 'Measurement results',
              tags$div(
                style = 'width: 100%;padding: 15px;',
                h5(tags$b('Readability')),
                tags$div(id = "inline", #style = 'font-size:12px',
                         fluidRow(
                           column(6, fluidRow(column(11, offset = 1, numericInput(ns('d'), label = ReqField('Balance scale division:'), min = 1e-7, value = NULL, step = 0.01)))),
                           column(6, radioButtons(ns('d.units'), label = NULL, choices = names(unitsOpt), selected = 'mg', inline = TRUE)))),
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
                rHandsontableOutput(ns("HT.repeatability")),
                tags$div(id = "inline", radioButtons(ns('rep.units'), label = ReqField('Units of values in table'), choices = names(unitsOpt), selected = 'g', inline = TRUE)),
                tags$hr(), tags$hr(),
                
                h5(tags$b('Excentricity test')), 
                fluidRow(
                  column(6,
                         rHandsontableOutput(ns("HT.eccen")),
                         tags$div(id = "inlineTOP", radioButtons(ns('eccen.units'), label = ReqField('Units of values in table'), choices = names(unitsOpt), selected = 'g', inline = FALSE))),
                  column(3, offset = 1, div(img(src = "eccen.png", width = '90%'), style = "text-align: center;"))),
                tags$hr(), tags$hr(),
                
                h5(tags$b('Indication error test')), 
                tags$div(id = "inline", 
                         fluidRow(
                           column(6, fluidRow(column(11, offset = 1, 
                                                     numericInput(ns('IndErrorPoints'), 
                                                                  label = ReqField('No. of calibration points'), min = 2, max = 30, value = 10, step = 1)))),
                           column(6, '(Set this value before entering data in the table)'))),
                tags$br(),
                rHandsontableOutput(ns("HT.indicationError")),
                tags$div(id = "inline", 
                         numericInput(ns('IndErrorK'), label = ReqField('Expanded uncertainty coverage factor'), min = 1, max = 10, value = 2, step = 1),
                         textInput(ns('traceability'), label = ReqField('Metrological traceability of the calibration'), width = '100%', 
                                   placeholder = 'Calibration certificate information of the standard weights.'),
                         radioButtons(ns('classSTD'), label = ReqField('OIML class of the standard weights'), 
                                      choices = c('E1', 'E2', 'F1', 'F2', 'M1', 'M2', 'M3'), inline = TRUE, selected = character(0))),
                
                #tags$div(id = "inline"),
                tags$hr(), tags$hr(),
                h5(tags$b('Environmental conditions during calibration')),
                splitLayout(
                  cellWidths = c('3%', '28%', '44%', '25%'), tags$div(), 
                  tags$div(
                    tags$b(ReqField('Ambient temperature')), 
                    tags$div(id = "inline", numericInput(ns('Temp1'), label = NonReqField('min.'), value = NULL, min = 0), 
                             numericInput(ns('Temp2'), label = NonReqField('max.'), value = NULL, min = 0)),
                    radioButtons(ns('TempUnits'), label = NULL, choices = tempeAllowedUnits, inline = FALSE)
                  ),
                  tags$div(
                    tags$b(ReqField('Barometric pressure')), 
                    tags$div(id = "inline", numericInput(ns('bPres1'), label = NonReqField('min.'), value = NULL, min = 0),
                             numericInput(ns('bPres2'), label = NonReqField('max.'), value = NULL, min = 0)),
                    radioButtons(ns('bPresUnits'), label = NULL, choices = bPresAllowedUnits, inline = FALSE, selected = 'hPa')
                  ),
                  tags$div(
                    tags$b(ReqField('Relative humidity')), 
                    tags$div(id = "inline", numericInput(ns('rHumi1'), label = NonReqField('min.'), value = NULL, min = 0),
                             numericInput(ns('rHumi2'), label = NonReqField('max.'), value = NULL, min = 0)),
                    radioButtons(ns('rHumiUnits'), label = NULL, choices = rHumiAllowedUnits, inline = FALSE)
                  )
                ),
                tags$hr(), tags$hr(),
                splitLayout(
                  actionButton(inputId = ns('Go2Comments'), label = tags$b('Go to comments (optional)'), width = '100%'),
                  uiOutput(ns('FinishNAWIDCC1'))),
                tags$br(), HTML(spcs(1))
              ))},
            
            #Comments
            {tabPanel(
              title = 'Comments',
              tags$div(
                style = 'width: 100%;padding: 15px;',
                textAreaInput(ns('Comments1'), label = 'Comments - Section 1', width = '100%', rows = 5, resize = 'none', 
                              placeholder = 'Optional comments - Part 1'),
                textAreaInput(ns('Comments2'), label = 'Comments - Section 2', width = '100%', rows = 5, resize = 'none', 
                              placeholder = 'Optional comments - Part 2'),
                textAreaInput(ns('Comments3'), label = 'Comments - Section 3', width = '100%', rows = 5, resize = 'none', 
                              placeholder = 'Optional comments - Part 3'), tags$hr(),
                splitLayout(
                  HTML(spcs(1)),
                  uiOutput(ns('FinishNAWIDCC2'))),
                tags$br(), HTML(spcs(1))
              ))}
          )
        ))},
    ),
    column(
      6,
      conditionalPanel(
        condition = 'input.SourceOption == "daCapo" || input.SourceOption == "file"', ns = ns,
        h4('Human-readable document preview:'), 
        #tags$b(''),
        splitLayout(
          conditionalPanel(condition = 'input.SourceOption == "daCapo"', ns = ns, uiOutput(ns('downloadDCC1'))),
          uiOutput(ns('downloadPDF1'))
        ),
        tags$hr(),
        withSpinner(uiOutput(ns('pff')), type = 6, color = "#2c3e50")
        #tags$hr(),
        #verbatimTextOutput(ns('primitive'))
      ))
    ),
    tags$hr(), tags$br(), niceSeparator()
  )
}