homeMasscor.UI <- function(id) {
  ns <- NS(id)
  column(width = 10, offset = 1,
         h3(tags$b('Mass measurement corrections using the R package masscor')), tags$br(),
         h4('The R package masscor provides')
         )
}

homeMasscor.Server <- function(input, output, session) {

}