masscorAppVersion <- '0.1.12'
DummyNumber <- c(0, 0.123456789)

unitsOpt <- c(kg = 1e3, g = 1e1, mg =	1e-3, ug = 1e-6)
tempeAllowedUnits <- c('ªC' = 'deg.C', K ='K')
rHumiAllowedUnits <- c(Percentaje = '%', Fraction = 'frac')
bPresAllowedUnits <- c('Pa', 'hPa', 'kPa', 'mmHg')

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

Information <- h5(
  "This interactive web application was developed in the framework of a project financially supported by the Ministerio de Ciencia, 
  Tecnología e Innovación de Colombia", tags$a(href = "https://minciencias.gov.co/", "(MinCiencias)", target = "_blank"), "under project number 9932100271370.", tags$br(),
  "The work was presented at the ", tags$a(href = "https://URL.sin.asignar/", "XXX XXXX Anual Conference on XXXXX XXXX (20XX)", target = "_blank"),
  "by", tags$a(href = "https://www.researchgate.net/profile/Cristhian-Paredes-2", "Cristhian Paredes.", target = "_blank"))
  

errorBoxNoNAWIDCC <- infoBox(title = 'Missing calibration information', color = 'yellow', width = 10, icon = icon('bug'), fill = TRUE, 
                             value = tags$div(
                               tags$br(),
                               "It seems that no calibration certificate information has been loaded yet.", tags$br(), tags$br(), "Please go to the previous tab and
                               upload a formely created masscor NAWI DCC (a file with extension '.rds') or create a new one.", tags$br(), tags$br(), 
                               "Be sure to press either the button", tags$u("Upload selected NAWI DCC"), " or ", tags$u("Finish NAWI DCC,"), "accordingly.", tags$br(), tags$br()#,
                               #"You will be redirected soon..."
                               ))