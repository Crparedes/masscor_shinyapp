uploadDCC.UI <- function(id) {
  ns <- NS(id)
  box(title = div(style = 'font-size:20px', tags$b('Upload masscor DCC')), width = 12, status = 'primary', collapsible = TRUE, collapsed = FALSE
      
     )
}

uploadDCC.Server <- function(input, output, session) {

}