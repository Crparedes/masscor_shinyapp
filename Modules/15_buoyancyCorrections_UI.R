buoyancyCorrections.UI <- function(id) {
  ns <- NS(id)
  column( # Revisar https://www.oiml.org/en/files/pdf_d/d028-e04.pdf
    width = 10, offset = 1,
    h3(tags$b('Air buoyancy correction factors using the masscor NAWI DCC and environmental conditions data.')),
    #actionButton(inputId = ns('brwzInsideModule'), label = tags$b('Browser() - inside module')), #Eliminar esta linea
    tags$br(),
    fluidRow(
      column(
        8,
        h4(tags$b('Instructions to calculate the magnitude of the air buoyancy correction (MABC) factors:')),
        # 'To calculate the magnitude of the air buoyancy correction (MABC) factor, you will require 
        # the measurement results of environmental conditions and the density of the object being weighed, 
        # all with corresponding standard uncertainties.',
        tags$ol(
          tags$li("Input the environmental conditions with standard uncertainties and units."), tags$br(),
          {fluidRow(style = 'font-size:12px;',
            column(
              3,
              tags$b(ReqField('Ambient temperature')),
              tags$div(id = "inline",
                       splitLayout(cellWidths = c("40%", "60%"),
                                   numericInput(ns('Temp1'), label = NULL, value = NULL, min = 0, step = 0.01, width = '50%'),
                                   numericInput(ns('u_Temp1'), label = NonReqField('\u00B1'), value = NULL, min = 0, step = 0.01, width = '50%'))),
              radioButtons(ns('TempUnits'), label = NULL, choices = tempeAllowedUnits, inline = TRUE)),
            column(
              4,
              tags$b(ReqField('Barometric pressure')),
              tags$div(id = "inline", 
                       splitLayout(cellWidths = c("30%", "40%"),
                                   numericInput(ns('BarPres1'), label = NULL, value = NULL, min = 0, step = 0.01, width = '50%'),
                                   numericInput(ns('u_BarPres1'), label = NonReqField('\u00B1'), value = NULL, min = 0, step = 0.01, width = '50%'))),
              radioButtons(ns('bPresUnits'), label = NULL, choices = bPresAllowedUnits, inline = TRUE, selected = 'hPa')),
            column(
              3, tags$b(ReqField('Relative humidity')),
              tags$div(id = "inline", 
                       splitLayout(cellWidths = c("40%", "60%"),
                                   numericInput(ns('relHum1'), label = NULL, value = NULL, min = 0, step = 0.01, width = '50%'),
                                   numericInput(ns('u_relHum1'), label = NonReqField('\u00B1'), value = NULL, min = 0, step = 0.01, width = '50%'))),
              radioButtons(ns('rHumiUnits'), label = NULL, choices = rHumiAllowedUnits, inline = TRUE))
          )}, 
          tags$br(), tags$br(),
          
          tags$li("Select the units for density quantities."), tags$br(),
          splitLayout(
            tags$div(id = "inline", 
                     radioButtons(ns('densityUnits'), label = NonReqField('Units for density quantities:'), choices = densityAllowedUnits, inline = TRUE))#,
            #actionButton(ns('CalculateRhoAir'), label = 'Calculate the density of local air')
          ), tags$br(), tags$br(),
          
          tags$li("Fill the right-top table with the densities and density uncertainties of the objects to be weighed."),
          "(More rows can be added by right-clicking in the table.)", tags$br(), tags$br(),
          
          tags$li("Download the files with the MABC factors in the desired format."),
        )),
      column(
        4, 
        tags$hr(), tags$hr(),
        tags$b('Density of the local air:'), tags$hr(), tags$hr(),
        tags$b('Density of the objects to be weighed:'), tags$hr(), tags$hr(),
        tags$b('MABC factors of the objects to be weighed:'), tags$hr(), tags$hr(),
        tags$br(),
        downloadButton(ns('DwnlConvMassResCSV'), 'Download measurement results (CSV)'), tags$br(), tags$br(),
        downloadButton(ns('DwnlConvMassResXLS'), 'Download measurement results (XLSX)')
      )
    ),
  
    niceSeparator(), niceSeparator(),
    
    h4(tags$b('Instructions to calculate (real) masses')), uiOutput(ns('noDCC.loaded')),
    
    fluidRow(
      column(
        8,
        
        )),
    niceSeparator()
  )
  
}