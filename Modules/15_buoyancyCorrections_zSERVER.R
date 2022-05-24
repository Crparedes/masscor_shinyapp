buoyancyCorrections.Server <- function(input, output, session, NAWIDCC) {
  observeEvent(input$brwzInsideModule, browser())
  
  failedFileBolean <- reactive({#reactiveTimer(5e3)
    if (is.error(class(NAWIDCC$NAWIDCC()))) {return(TRUE)} else {
      if (class(NAWIDCC$NAWIDCC()) != 'calibCert') {return(TRUE)} else {return(FALSE)}
    }})
  output$noDCC.loaded <- renderUI(if(failedFileBolean()) {return(errorBoxNoNAWIDCC)}) 
  
  choices.d <- reactive(convertMassUnitsSI(NAWIDCC$NAWIDCC()$d, from = NAWIDCC$NAWIDCC()$standardUnits, to = NAWIDCC$NAWIDCC()$orgdUnits) * c(1, 10, 100))
  output$current.d.value <- renderUI(radioButtons(session$ns('current.d.value'), label = 'NAWI scale division used for indications:', 
                                                  choiceNames = paste0(choices.d(), NAWIDCC$NAWIDCC()$orgdUnits, sep = ' '), 
                                                  choiceValues = choices.d(), inline = TRUE))
  
  
}