# Cargar im'agenes png https://www.rdocumentation.org/packages/png/versions/0.1-7/topics/readPNG

manageDCC.Server <- function(input, output, session) {
  observeEvent(input$brwzInsideModule, browser())
  
  {# Navigation between tabs
  BoleanIncompleteAdminDat <- reactive(FALSE)#are.null.empty(c(input$institution, input$accreditation, input$balanceID, input$serial, input$certificate, input$date)))
  BoleanIncompleteMeasurRes <- reactive(are.null.empty(c(input$d, input$HT.repeatability)))
  observeEvent(input$Go2MeasRes, ignoreInit = TRUE,
               if (BoleanIncompleteAdminDat()) {
                 showNotification('Please complete the required fields', duration = 3, type = 'error')
               } else {
                 updateTabItems(inputId = 'createDCC.TB', selected = 'Measurement results')
               })
  observeEvent(input$createDCC.TB, ignoreInit = TRUE,
               if (BoleanIncompleteAdminDat()) {
                 showNotification('Please complete the required fields', duration = 3, type = 'error')
                 updateTabItems(inputId = 'createDCC.TB', selected = 'Administrative data')
               } else {
                 if (BoleanIncompleteMeasurRes() && input$createDCC.TB == 'Comments') {
                   showNotification('Please complete the required fields', duration = 3, type = 'error')
                   updateTabItems(inputId = 'createDCC.TB', selected = 'Measurement results')
                 }
               })             
  
  observeEvent(input$Go2Comments, ignoreInit = TRUE, updateTabItems(inputId = 'createDCC.TB', selected = 'Comments'))
  
  observeEvent(input$Go2MeasRes, ignoreInit = TRUE,
               if (is.null.empty(input$institution)) {
                 showNotification('Please complete the required fields', duration = 3, type = 'error')
               } else {
                 updateTabItems(inputId = 'createDCC.TB', selected = 'Measurement results')
               })
  }
  
  ## Handsontables
  values <- reactiveValues()
  
  # Repeatability test
  RepeatTableSize <- reactive(c(input$ReapTestPoints, input$ReapTestMeaPerPoints))
  DF.repeatability <- reactive(data.frame(matrix(nrow = RepeatTableSize()[1], ncol = RepeatTableSize()[2]), row.names = paste0('Load No. ', 1:RepeatTableSize()[1]) ))
  
  observe({
    if (!is.null(input$HT.repeatability)) {
      values[["p.repeatability"]] <- isolate(values[["DF.repeatability"]])
      DF.repeatability = HOT2R(input$HT.repeatability)
    } else {
      if (is.null(values[["DF.repeatability"]])) {DF.repeatability <- DF.repeatability} else {DF.repeatability <- values[["DF.repeatability"]]}
    }
    values[["DF.repeatability"]] <- DF.repeatability()
  })
  
  output$HT.repeatability <- renderRHandsontable({
    DF.repeatability <- values[["DF.repeatability"]]
    if (!is.null(DF.repeatability)) {
      rhandsontable(DF.repeatability, #colHeaders = paste0('Ind.', 1:input$ReapTestMeaPerPoints), 
                    rowHeaderWidth = 100) %>%
      hot_col(col = 1:input$ReapTestMeaPerPoints, type = 'numeric', allowInvalid = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_rows(fixedRowsTop = input$ReapTestPoints)
    }
  })
  

  
  # Eccentricity test
  DF.eccen <- data.frame(Indication = rep(NA, 5), row.names = paste0('Position No. ', 1:5))
  observe({
    if (!is.null(input$HT.eccen)) {
      values[["previous.analitos"]] <- isolate(values[["DF.eccen"]])
      DF.eccen = HOT2R(input$HT.eccen)
    } else {
      if (is.null(values[["DF.eccen"]])) {DF.eccen <- DF.eccen} else {DF.eccen <- values[["DF.eccen"]]}
    }
    values[["DF.eccen"]] <- DF.eccen
  })
  output$HT.eccen <- renderRHandsontable({
    DF.eccen <- values[["DF.eccen"]]
    if (!is.null(DF.eccen))
      rhandsontable(DF.eccen, rowHeaderWidth = 110) %>% 
      hot_col(col = 1, type = 'numeric', allowInvalid = FALSE) %>%
      hot_cols(colWidths = 110) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_rows(fixedRowsTop = 5)
  })
  
  # Indication error
  DF.indicationError <- reactive(data.frame(Units = c('g', 'mg', 'mg'), matrix(nrow = 3, ncol = input$IndErrorPoints), row.names = c('Nominal mass', 'Indication error', 'Uncertainty')))
  observe({
    if (!is.null(input$HT.indicationError)) {
      values[["previous.analitos"]] <- isolate(values[["DF.indicationError"]])
      DF.indicationError = HOT2R(input$HT.indicationError)
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
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_rows(fixedRowsTop = 3)
  })
  
  MeasurResdultsList <- 
  
}