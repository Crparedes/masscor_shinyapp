conventionalMass.UI <- function(id) {
  ns <- NS(id)
  column( # Revisar https://www.oiml.org/en/files/pdf_d/d028-e04.pdf
    width = 10, offset = 1,
    h3(tags$b('Conventional value of the result of weighing in air using the masscor NAWI DCC')),
    tags$hr(),
    actionButton(inputId = ns('brwzInsideModule'), label = tags$b('Browser() - inside module')), #Eliminar esta linea
    uiOutput(ns('noDCC.loaded')), tags$br(), tags$hr(),
    splitLayout(
      cellWidths = c('50%', '15%', '35%'),
      tags$div(
        tags$b('Instructions'),
        tags$ol(
          tags$li("Select the NAWI scale division used for the indications"),
          "(Just in case a value bigger than the maximum balance readability was set.)",
          tags$li("Select the units of the indications to provide"),
          tags$li("Fill the column of the first table with the mass indications to be corrected."),
          tags$li("Download the files with the measurement results")
          ),
        "(More rows can be added by right-clicking in the table.)", tags$br(),
        "(Modify the balance readability and the units of indications before entering any data in the table.)", tags$hr(),# 
        tags$div(id = "inline", uiOutput(ns('current.d.value')), tags$br(),
                 radioButtons(ns('UnitsTable'), NonReqField('Units of indications in table'), choices = names(unitsOpt), selected = "g", inline = TRUE)), tags$br(),
        "(Conventional mass and uncertainties will be displayed in same units.)"), 
      tags$div(tags$b('Mass indications'), rHandsontableOutput(ns("Indications"))), 
      tags$div(tags$b('Conventional mass measurement results'), tags$br(), tags$br(),
               htmlOutput(ns('ConvMassText')), tags$br(),
               downloadButton(ns('DwnlConvMassResCSV'), 
                              'Download measurement results (CSV)'), tags$br(), tags$br(),
               downloadButton(ns('DwnlConvMassResXLS'), 
                              'Download measurement results (XLSX)'))
    )
    
    
    
    )
}