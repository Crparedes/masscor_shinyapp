masscorAppVersion <- '0.1.12'

DummyNumber <- c(0, 0.123456789)

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

EnsureMinValue <- function(x, min) {return(max(na.omit(c(x, min))))}

SummarizeRepInput <- function(x) {
  return(as.data.frame(t(apply(x, 1, function(xi) return(c(mean(xi), sd(xi)))))))
}

SummarizeEccenInput <- function(x) {
  return(c(mean(x[[1]]), max(abs(x[[1]][1] - x[[1]][2:5]))))
}


#x <- input$HT.eccen


HTMELIZAME.Esta <- function(mat, dVal, OrgUnits, UnitsTable) {
  htmlizado <- '<ol>'
  for (i in 1:nrow(mat)) {
    a1 <- format(round(c(mat[i, 1], DummyNumber), digits = abs(floor(log10(convertMassUnitsSI(dVal, from = OrgUnits, to = UnitsTable))))))[1]
    a2 <- format(round(c(mat[i, 2], DummyNumber), digits = abs(floor(log10(convertMassUnitsSI(dVal, from = OrgUnits, to = UnitsTable)) - 1))))[1]
    htmlizado <- c(htmlizado, '<li>', spcs(5), a1, '&nbsp;&#177;&nbsp;', 
                   format(a2, scientific = FALSE), '</li>')
  }
  htmlizado <- c(htmlizado, '</ol>')
  return(htmlizado)
}




#,https://cran.r-project.org/web/packages/masscor/index.html")

errorBoxNoNAWIDCC <- infoBox(title = 'Missing calibration information', color = 'yellow', width = 10, icon = icon('bug'), fill = TRUE, 
                             value = tags$div(
                               tags$br(),
                               "It seems that no calibration certificate information has been loaded yet.", tags$br(), tags$br(), "Please go to the previous tab and
                               upload a formely created masscor NAWI DCC (a file with extension '.rds') or create a new one.", tags$br(), tags$br(), 
                               "Be sure to press either the button", tags$u("Upload selected NAWI DCC"), " or ", tags$u("Finish NAWI DCC,"), "accordingly.", tags$br(), tags$br(),
                               "You will be redirected soon..."
                               ))

is.error <- function (expr, tell = FALSE, force = FALSE) { # Taken from https://www.rdocumentation.org/packages/berryFunctions/versions/1.21.14
  expr_name <- deparse(substitute(expr))
  test <- try(expr, silent = TRUE)
  iserror <- inherits(test, "try-error")
  if (tell) 
    if (iserror) 
      message("Note in is.error: ", test)
  if (force) 
    if (!iserror) 
      stop(expr_name, " is not returning an error.", call. = FALSE)
  iserror
}