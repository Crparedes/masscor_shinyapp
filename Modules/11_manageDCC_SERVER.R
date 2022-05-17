# Cargar im'agenes png https://www.rdocumentation.org/packages/png/versions/0.1-7/topics/readPNG

manageDCC.Server <- function(input, output, session) {
  observeEvent(input$brwzInsideModule, browser())
  
  # Navigation between tabs
  BoleanIncompleteAdminDat <- reactive(FALSE)#are.null.empty(c(input$institution, input$accreditation, input$balanceID, input$serial, input$certificate, input$date)))
  BoleanIncompleteMeasurRes <- reactive(all(is.null.empty(input$calib)))
  observeEvent(input$Go2MeasRes, ignoreInit = TRUE,
               if (BoleanIncompleteAdminDat()) {
                 showNotification('Please complete the required fields of administrative data', duration = 3, type = 'error')
               } else {
                 updateTabItems(inputId = 'createDCC.TB', selected = 'Measurement results')
               })
  observeEvent(input$createDCC.TB, ignoreInit = TRUE,
               if (BoleanIncompleteAdminDat()) {
                 showNotification('Please complete the required fields of administrative data', duration = 3, type = 'error')
                 updateTabItems(inputId = 'createDCC.TB', selected = 'Administrative data')
               } else {
                 if (BoleanIncompleteMeasurRes() && input$createDCC.TB == 'Comments') {
                   showNotification('Please complete the required fields of measurement results', duration = 3, type = 'error')
                   updateTabItems(inputId = 'createDCC.TB', selected = 'Measurement results')
                 }
               })             
  
  observeEvent(input$Go2Comments, ignoreInit = TRUE, updateTabItems(inputId = 'createDCC.TB', selected = 'Comments'))
  
  observeEvent(input$Go2MeasRes, ignoreInit = TRUE,
               if (is.null.empty(input$institution)) {
                 showNotification('Please complete the required fields of measurement results', duration = 3, type = 'error')
               } else {
                 updateTabItems(inputId = 'createDCC.TB', selected = 'Measurement results')
               })

  ## Handsontables
  values <- reactiveValues()
  
  # Indication error
  DF.indicationError <- reactive(data.frame(cbind(Units = rep(NA, 3), matrix(nrow = 3, ncol = input$IndErrorPoints)), row.names = c('Nominal mass', 'Indication error', 'Uncertainty')))
  observe({
    if (!is.null(input$HT.indicationError)) {
      values[["previous.analitos"]] <- isolate(values[["DF.indicationError"]])
      DF.indicationError = hot_to_r(input$HT.indicationError)
    } else {
      if (is.null(values[["DF.indicationError"]])) {DF.indicationError <- DF.indicationError} else {DF.indicationError <- values[["DF.indicationError"]]}
    }
    values[["DF.indicationError"]] <- DF.indicationError
  })
  output$HT.indicationError <- renderRHandsontable({
    DF.indicationError <- values[["DF.indicationError"]]
    if (!is.null(DF.indicationError))
      rhandsontable(DF.indicationError, stretchH = "all", colHeaders = c('Units', rep('', input$IndErrorPoints))) %>% 
      hot_col(col = 2:(input$IndErrorPoints + 1), type = 'numeric', format = "0.00000", allowInvalid = FALSE) %>%
      hot_col(col = 1, type = 'dropdown', source = unitsOpt, allowInvalid = FALSE)  %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = TRUE)
    })
}