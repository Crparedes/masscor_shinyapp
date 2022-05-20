# Cargar im'agenes png https://www.rdocumentation.org/packages/png/versions/0.1-7/topics/readPNG

manageDCC.Server <- function(input, output, session) {
  observeEvent(input$brwzInsideModule, browser())
  
  # Navigation between tabs
  {
  BoleanIncompleteAdminDat <- reactive(FALSE)#are.null.empty(c(input$institution, input$accreditation, input$balanceID, input$serial, input$certificate, input$date)))
  BoleanIncompleteMeasurRes <- reactive(FALSE)#are.null.empty(c(input$d, HOT2R(input$HT.repeatability), HOT2R(input$HT.eccen), HOT2R(input$HT.indicationError), input$Tempe, input$bPres, input$rHumi)))
  
  observeEvent(input$Go2MeasRes, ignoreInit = TRUE,
               if (BoleanIncompleteAdminDat()) {
                 showNotification('Please fill in all the required fields.', duration = 4, type = 'error')
               } else { updateTabItems(inputId = 'createDCC.TB', selected = 'Measurement results')})
  observeEvent(input$createDCC.TB, ignoreInit = TRUE,
               if (BoleanIncompleteAdminDat()) {
                 showNotification('Please fill in all the required fields.', duration = 4, type = 'error')
                 updateTabItems(inputId = 'createDCC.TB', selected = 'Administrative data')
               } else {
                 if (BoleanIncompleteMeasurRes() && input$createDCC.TB == 'Comments') {
                   showNotification('Please fill in all the required fields.', duration = 4, type = 'error')
                   updateTabItems(inputId = 'createDCC.TB', selected = 'Measurement results')
                 }
               }) 
  observeEvent(input$Go2Comments, ignoreInit = TRUE, updateTabItems(inputId = 'createDCC.TB', selected = 'Comments'))
  }
  
  ## Handsontables
  values <- reactiveValues()
  
  # Repeatability test
  {
    RepeatTableSize <- reactive(c(input$ReapTestPoints, input$ReapTestMeaPerPoints))
    DF.repeatability <- reactive(data.frame(matrix(nrow = RepeatTableSize()[1], ncol = RepeatTableSize()[2]), row.names = paste0('Load No. ', 1:RepeatTableSize()[1]) ))
    
    
    observe({
      if (!is.null(input$HT.repeatability)) {
        DF.repeatability <- HOT2R(input$HT.repeatability)
      } else {
        if (is.null(values[["DF.repeatability"]])) {DF.repeatability <- DF.repeatability} else {DF.repeatability <- values[["DF.repeatability"]]}
      }
      values[["DF.repeatability"]] <- DF.repeatability()
    })
    
    output$HT.repeatability <- renderRHandsontable({
      DF.repeatability <- values[["DF.repeatability"]]
      if (!is.null(DF.repeatability)) {
        rhandsontable(DF.repeatability, colHeaders = paste0('Ind.', 1:input$ReapTestMeaPerPoints), overflow = 'visible', 
                      rowHeaderWidth = 100) %>%
        hot_col(col = 1:input$ReapTestMeaPerPoints, type = 'numeric', allowInvalid = FALSE) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_rows(fixedRowsTop = input$ReapTestPoints)
      }
    })
  }
  
  # Eccentricity test
  {
    DF.eccen <- data.frame(Indication = rep(NA, 5), row.names = paste0('Position No. ', 1:5))
    observe({
      if (!is.null(input$HT.eccen)) {
        DF.eccen <- HOT2R(input$HT.eccen)
      } else {
        if (is.null(values[["DF.eccen"]])) {DF.eccen <- DF.eccen} else {DF.eccen <- values[["DF.eccen"]]}
      }
      values[["DF.eccen"]] <- DF.eccen
    })
    output$HT.eccen <- renderRHandsontable({
      DF.eccen <- values[["DF.eccen"]]
      if (!is.null(DF.eccen))
        rhandsontable(DF.eccen, rowHeaderWidth = 110, overflow = 'visible') %>% 
        hot_col(col = 1, type = 'numeric', allowInvalid = FALSE) %>%
        hot_cols(colWidths = 110) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_rows(fixedRowsTop = 5)
    })
  }
  
  # Indication error
  {
    DF.indicationError <- reactive(data.frame(Units = c('g', 'mg', 'mg'), matrix(nrow = 3, ncol = input$IndErrorPoints), row.names = c('Nominal mass', 'Indication error', 'Uncertainty')))
    observe({
      if (!is.null(input$HT.indicationError)) {
        DF.indicationError <- HOT2R(input$HT.indicationError)
      } else {
        if (is.null(values[["DF.indicationError"]])) {DF.indicationError <- DF.indicationError} else {DF.indicationError <- values[["DF.indicationError"]]}
      }
      values[["DF.indicationError"]] <- DF.indicationError()
    })
    output$HT.indicationError <- renderRHandsontable({
      DF.indicationError <- values[["DF.indicationError"]]
      if (!is.null(DF.indicationError))
        rhandsontable(DF.indicationError, colHeaders = c('Units', paste0('Point.', 1:input$IndErrorPoints)), rowHeaderWidth = 100, overflow = 'visible') %>% 
        hot_col(col = 2:(input$IndErrorPoints + 1), type = 'numeric', allowInvalid = FALSE) %>%
        hot_col(col = 1, type = 'autocomplete', source = names(unitsOpt), strict = TRUE, allowInvalid = FALSE) %>%
        hot_cols(fixedColumnsLeft = 1)  %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_rows(fixedRowsTop = 3)
    })
  }
  
  # CalibCert DCC creation
  NAWI.DCC.Completed <- reactiveVal(FALSE)
  observeEvent(input$FinishNAWIDCC1,
               if (BoleanIncompleteMeasurRes()) {
                 showNotification('NAWI DCC information is missing, please fill in all the required fields.', duration = 6, type = 'error')
               } else {
                 NAWI.DCC.Completed(TRUE)
               })
  
  observeEvent(input$FinishNAWIDCC2, NAWI.DCC.Completed(TRUE)) 
               
  {
    DCC <- eventReactive(
      eventExpr = NAWI.DCC.Completed(), ignoreInit = TRUE, 
      valueExpr = {
        calibCert(institution = input$institution, accreditation = input$accreditation, date = input$date,
                  balanceID = input$balanceID, serial = input$serial, certificate = input$certificate, 
                  d = input$d, d.units = input$d.units, 
                  indError = HOT2R(input$HT.indicationError)[, -1], indError.units = HOT2R(input$HT.indicationError)[, 1], 
                  expanded = TRUE, k = input$IndErrorK, traceability = input$traceability,
                  rep = HOT2R(input$HT.repeatability), rep.units = input$rep.units, 
                  eccen = HOT2R(input$HT.eccen), eccen.units = input$eccen.units, 
                  Temp = input$Temp, p = input$bPres, h = input$rHumi, 
                  unitsENV = c(input$TempUnits, input$bPresUnits, input$rHumiUnits))
    })
  output$primitive <- renderPrint(DCC())
  }
  
  
}