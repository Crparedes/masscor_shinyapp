conventionalMass.UI <- function(id) {
  ns <- NS(id)
  box(title = div(style = 'font-size:20px', tags$b('Create masscor DCC')), width = 12, status = 'primary', collapsible = TRUE, collapsed = FALSE
      
      )
}

conventionalMass.Server <- function(input, output, session) {

}