spcs <- function(n) {return(paste0(rep('&nbsp;', n), collapse = ''))}

ReqField <- function(x) {return(HTML(paste0(x, '<font color=\"#FF0000\">*</font>', spcs(3))))}
NonReqField <- function(x) {return(HTML(paste0(x, spcs(3))))}


is.null.empty <- function(x) {if (is.null(x)) {return(TRUE)} else {if (length(x) == 0 || x == '' || any(is.na(unlist(x)))) {return(TRUE)} else {return(FALSE)}}}
are.null.empty <- function(x) {
  for (i in 1:length(x)) {
    if (is.null.empty(x[i])) {return(TRUE)}}
  return(FALSE)
}

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
  

HOT2R <- function(x) {
  if (is.null(x)) return(NA)
  rows <- length(x$data)
  cols <- length(x$data[[1]])
  extracted <- data.frame(matrix(nrow = rows, ncol = cols))
  for (i in 1:rows) {
    for (j in 1:cols) {
      extracted[i, j] <- ifelse(is.null(x$data[[i]][[j]]), NA, x$data[[i]][[j]])
    }
  }
  colnames(extracted) <- unlist(x$params$colHeaders)
  rownames(extracted) <- unlist(x$params$rowHeaders)
  return(extracted)
}

#x <- input$HT.eccen

#,https://cran.r-project.org/web/packages/masscor/index.html")