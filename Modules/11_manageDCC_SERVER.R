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
  DF.indicationError <- reactive(data.frame(Units = c('g', 'mg', 'mg'), matrix(nrow = 3, ncol = input$IndErrorPoints), row.names = c('Nominal mass', 'Indication error', 'Uncertainty')))
  observe({
    if (!is.null(input$HT.indicationError)) {
      values[["previous.analitos"]] <- isolate(values[["DF.indicationError"]])
      DF.indicationError = hot_to_r(input$HT.indicationError)
    } else {
      if (is.null(values[["DF.indicationError"]])) {DF.indicationError <- DF.indicationError} else {DF.indicationError <- values[["DF.indicationError"]]}
    }
    values[["DF.indicationError"]] <- DF.indicationError()
  })
  output$HT.indicationError <- renderRHandsontable({
    DF.indicationError <- values[["DF.indicationError"]]
    if (!is.null(DF.indicationError))
      rhandsontable(DF.indicationError, colHeaders = c('Units', paste0('Point.', 1:input$IndErrorPoints)), rowHeaderWidth = 100) %>% 
      hot_col(col = 2:(input$IndErrorPoints + 1), type = 'numeric', allowInvalid = FALSE) %>%
      hot_col(col = 1, type = 'autocomplete', source = names(unitsOpt), strict = TRUE, allowInvalid = FALSE) %>%
      hot_cols(fixedColumnsLeft = 1)  %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })
  
  # Repeatability test
  DF.repeatability <- reactive(data.frame(matrix(nrow = input$ReapTestPoints, ncol = input$ReapTestMeaPerPoints), row.names = paste0('Load No. ', 1:input$ReapTestPoints)))
  observe({
    if (!is.null(input$HT.repeatability)) {
      values[["previous.analitos"]] <- isolate(values[["DF.repeatability"]])
      DF.repeatability = hot_to_r(input$HT.repeatability)
    } else {
      if (is.null(values[["DF.repeatability"]])) {DF.repeatability <- DF.repeatability} else {DF.repeatability <- values[["DF.repeatability"]]}
    }
    values[["DF.repeatability"]] <- DF.repeatability()
  })
  output$HT.repeatability <- renderRHandsontable({
    DF.repeatability <- values[["DF.repeatability"]]
    if (!is.null(DF.repeatability))
      rhandsontable(DF.repeatability, colHeaders = paste0('Ind.', 1:input$ReapTestMeaPerPoints), rowHeaderWidth = 100) %>% 
      hot_col(col = 1:input$ReapTestMeaPerPoints, type = 'numeric', allowInvalid = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })
}