conventionalMass.UI <- function(id) {
  ns <- NS(id)
  column( # Revisar https://www.oiml.org/en/files/pdf_d/d028-e04.pdf
    width = 10, offset = 1,
    h3(tags$b('Conventional value of the result of weighing in air using the masscor NAWI DCC')),
    tags$hr(),
    actionButton(inputId = ns('brwzInsideModule'), label = tags$b('Browser() - inside module')), #Eliminar esta linea
    
    # fluidRow(column(
    #   4, uiOutput('UsedBalSclDiv'),
    #   'In the following table you can place mass indications obtained with the NAWI', tags$b(textOutput()),
    #   'and the values will be converted to conventional mass measurement results using the Calibration Certificate No. ', 
    #   tags$b(textOutput('aasdd')), 'issued by', tags$b(textOutput('asdf'))
    # ))
    
    
    )
}