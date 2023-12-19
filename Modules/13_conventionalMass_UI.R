conventionalMass.UI <- function(id) {
  ns <- NS(id)
  column( # Revisar https://www.oiml.org/en/files/pdf_d/d028-e04.pdf
    width = 10, offset = 1,
    h3(tags$b('Conventional value of the result of weighing in air using the masscor NAWI DCC')),
    # actionButton(inputId = ns('brwzInsideModule'), label = tags$b('Browser() - inside module')), #Eliminar esta linea
    uiOutput(ns('noDCC.loaded')), tags$br(),
    fluidRow(
      column(
        6,            
        h4(tags$b('Instructions to calculate conventional masses:')),
        tags$ol(
          tags$li('Create or upload a NAWI DCC created with masscor'),
          tags$li("Select the NAWI scale division used for the mass indications:"),
          "(Just in case you set a balance readability lower than the maximum balance readability)", tags$br(), tags$br(),
          tags$div(id = "inline", uiOutput(ns('current.d.value'))), tags$hr(),
          
          tags$li("Select the units of the mass indications:"), tags$br(),
          tags$div(id = "inline", 
                   radioButtons(ns('UnitsTable'), NonReqField('Units of indications in table'), choices = names(unitsOpt), selected = "g", inline = TRUE)),
          tags$hr(),
          
          tags$li("Fill the column of the table to the right with the mass indications to be corrected."),
          "(More rows can be added by right-clicking in the table. It is important to modify 
          the balance readability and the units of indications before entering any data in the table.)", tags$br(), tags$br(),
          tags$li("Download the files with the conventional mass measurement results in the desired format."),
          "The measurement results and uncertainties will be provided in the same units as the mass indications in the table.")
      ),
      column(2, tags$br(), tags$br(), tags$b('Input mass indications:'), rHandsontableOutput(ns("Indications"))),
      column(3, tags$br(), tags$br(), #style = "background-color:#ffffff99;",
             tags$b('Conventional mass measurement results:'), tags$br(), tags$br(),
             htmlOutput(ns('ConvMassText')), tags$br(),
             downloadButton(ns('DwnlConvMassResCSV'), 'Download measurement results (CSV)'), tags$br(), tags$br(),
             downloadButton(ns('DwnlConvMassResXLS'), 'Download measurement results (XLSX)'))
    ),
    tags$hr(), tags$br(), niceSeparator()
    
    
    )
}