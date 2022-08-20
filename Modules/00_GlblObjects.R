masscorAppVersion <- '0.1.12'
DummyNumber <- c(0, 0.123456789)

unitsOpt <- c(kg = 1e3, g = 1e1, mg =	1e-3, ug = 1e-6)
tempeAllowedUnits <- c('ªC' = 'deg.C', K ='K')
rHumiAllowedUnits <- c(Percentaje = '%', Fraction = 'frac')
bPresAllowedUnits <- c('Pa', 'hPa', 'kPa', 'mmHg')
densityAllowedUnits <- c('$$g~cm^{-3}$$' = 'GramPerCubicCentiMeter', '$$kg~m^{-3}$$' = 'KiloGramPerCubicMeter')
densityHTMLUnits <- c(GramPerCubicCentiMeter = 'g cm<sup>-3</sup>', KiloGramPerCubicMeter = 'kg m<sup>-3</sup>')
densityPlainUnits <- c(GramPerCubicCentiMeter = 'g/cm^3', KiloGramPerCubicMeter = 'kg/m^3')

title <- tags$div(HTML(
  '<table text-align=left cellspacing=-10 cellPadding=30>
  <tr><th rowspan = 2>', spcs(5),
  '<a id = "logo" href = "http://www.inm.gov.co" target = ”_blank” title = "Masscor Graphical User Interface" data-height="80">
  <img src = "INM_masscor.png" height = "90" alt = "INM de Colombia" style = "margin-top: 5px">
  </a>', spcs(5),
  '</th>
  <th><h1 style="LINE-HEIGHT:5px; color: #dddddd; margin-bottom: 5px; font-size:45px;"><b>masscor package</b></h1></th></tr>
  <tr><th><h3 style="LINE-HEIGHT:0px; color: #dddddd; margin-top: 4px;">
  Graphical User Interface
  </h3></th></tr>
  </table>'))

Information <- tags$div(
  h6(
    #style = "a{color: #18bc9c; font-weight: bold}",
    "This interactive web application was developed in the framework of a project financially supported by the Ministerio de Ciencia, 
    Tecnología e Innovación de Colombia", 
    tags$a(href = "https://minciencias.gov.co/", "(MinCiencias)", style = "color: #18bc9c; font-weight: bold", target = "_blank"), 
    "under project number 9932100271370.", tags$br(),
    "The work was presented at the ", 
    tags$a(href = "https://ncsli.org/mpage/ws_2022/", "NCSL International Workshop & Symposium", style = 
             "color: #18bc9c; font-weight: bold", target = "_blank"),
    "(2022, Grapevine, Texas USA) by chemist", tags$a(href = "https://www.researchgate.net/profile/Cristhian-Paredes-2", 
                         "Cristhian Paredes.", style = "color: #18bc9c; font-weight: bold", target = "_blank"), 
    tags$br(),
    "The valuable knowledge transfer from the expert Coaches of the SIM-PTB-IDB joint project", 
    tags$a(href = "https://sim-metrologia.org/2021/09/13/workshop-for-launching-the-implementation-of-new-nmi-services-related-to-digital-transformation-idb-project/",
           "CABUREK-SIM-M4DT", target = "_blank", style = "color: #18bc9c; font-weight: bold"), 
    "notably inspired this implementation. Their help is greatly acknowledged."))
  

errorBoxNoNAWIDCC <- tags$div(
  tags$hr(),
  infoBox(title = 'Missing calibration information', color = 'yellow', width = 10, icon = icon('bug'), fill = TRUE, 
          value = tags$div(tags$br(),
                           "It seems that the calibration certificate information has not been loaded yet.", tags$br(), tags$br(), "Please go to the tab ",
                           tags$u(icon('certificate'), "Create/upload NAWI DCC"), 
                           " to manage the required masscor NAWI DCC file.", tags$br(),
                           tags$small("Be sure to press either the button", tags$u("Upload selected NAWI DCC"), " or ", tags$u("Finish NAWI DCC,"), " at the end of the process."), 
                           tags$br(), tags$br()#,
                           #"You will be redirected soon..."
                           )),
  tags$hr())