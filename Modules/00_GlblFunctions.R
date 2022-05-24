spcs <- function(n) {return(paste0(rep('&nbsp;', n), collapse = ''))}
ReqField <- function(x) {return(HTML(paste0(x, '<font color=\"#FF0000\">*</font>', spcs(3))))}
NonReqField <- function(x) {return(HTML(paste0(x, spcs(3))))}

is.null.empty <- function(x) {if (is.null(x)) {return(TRUE)} else {if (length(x) == 0 || x == '' || any(is.na(unlist(x)))) {return(TRUE)} else {return(FALSE)}}}
are.null.empty <- function(x) {
  for (i in 1:length(x)) {
    if (is.null.empty(x[i])) {return(TRUE)}}
  return(FALSE)
}

# Getting troubles using hot_to_r(), The following is a workaround
HOT2R <- function(x, IgnoreRowNames = FALSE) {
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
  if (!IgnoreRowNames) rownames(extracted) <- unlist(x$params$rowHeaders)
  return(extracted)
}

# This is a uglier, more robust workaround... 
# There may be a trouble with the rhandsontable renderizer, Only 4 digital places are taken
HTMELIZAME.Esta <- function(mat, dVal, OrgUnits, UnitsTable) {
  htmlizado <- '<ol>'
  for (i in 1:nrow(mat)) {
    a1 <- format(round(c(mat[i, 1], DummyNumber), digits = abs(floor(log10(convertMassUnitsSI(dVal, from = OrgUnits, to = UnitsTable))))))[1]
    a2 <- format(round(c(mat[i, 2], DummyNumber), digits = abs(floor(log10(convertMassUnitsSI(dVal, from = OrgUnits, to = UnitsTable)) - 1))))[1]
    htmlizado <- c(htmlizado, '<li>', spcs(5), '(', a1, '&nbsp;&#177;&nbsp;', 
                   format(a2, scientific = FALSE), ') ', spcs(3), UnitsTable, '</li>')
  }
  htmlizado <- c(htmlizado, '</ol>')
  return(htmlizado)
}

niceSeparator <- function(){return(tags$hr(style = "border-top: 5px solid #2c3e50;"))}


EnsureMinValue <- function(x, min) {return(max(na.omit(c(x, min))))}

SummarizeRepInput <- function(x) {
  return(as.data.frame(t(apply(x, 1, function(xi) return(c(mean(xi), sd(xi)))))))
}

SummarizeEccenInput <- function(x) {
  return(c(mean(x[[1]]), max(abs(x[[1]][1] - x[[1]][2:5]))))
}



# Taken from https://www.rdocumentation.org/packages/berryFunctions/versions/1.21.14
is.error <- function (expr, tell = FALSE, force = FALSE) { 
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