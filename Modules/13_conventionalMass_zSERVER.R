conventionalMass.Server <- function(input, output, session, NAWIDCC, parent, mainNavTb) {
  observeEvent(input$brwzInsideModule, browser())
  
  failedFileBolean <- reactive({#reactiveTimer(5e3)
    if (is.error(class(NAWIDCC$NAWIDCC()))) {return(TRUE)} else {
      if (class(NAWIDCC$NAWIDCC()) != 'calibCert') {return(TRUE)} else {return(FALSE)}
    }})
  output$noDCC.loaded <- renderUI(if(failedFileBolean()) {return(errorBoxNoNAWIDCC)})
  #observe({if(failedFileBolean()) {delay(8e3, updateTabItems(session = parent, inputId = 'MainNavTabs', selected = 'CreateUploadDCC'))}})
  
  
  choices.d <- reactive(convertMassUnitsSI(NAWIDCC$NAWIDCC()$d, from = NAWIDCC$NAWIDCC()$standardUnits, to = NAWIDCC$NAWIDCC()$orgdUnits) * c(1, 10, 100))
  output$current.d.value <- renderUI(radioButtons(session$ns('current.d.value'), label = 'NAWI scale division used for indications:', 
                                                  choiceNames = paste0(choices.d(), NAWIDCC$NAWIDCC()$orgdUnits, sep = ' '), 
                                                  choiceValues = choices.d(), inline = TRUE))
  
  
  frmtTable1 <- reactive(format(round(
    DummyNumber, 
    digits = abs(floor(log10(convertMassUnitsSI(as.numeric(input$current.d.value), from = NAWIDCC$NAWIDCC()$orgdUnits, to = input$UnitsTable))))))[1])
  frmtTable2 <- reactive(paste0(frmtTable1(), "0", collapse = ''))
  
  TableDat_0  <- reactiveValues(hot = data.frame('Indication' = c(0, rep(NA, 14))))#,  'Conventional.mass' = rep(NA, 14), 'Standard.uncertainty' = rep(NA, 14)))
  TableData <- reactive({
    DT <- NULL
    if (!is.null(input$HT.ConventionalMass)) {
      DT <- HOT2R(input$HT.ConventionalMass)
      TableDat_0[["hot"]]  <-  DT
    } else {#For initial data upload
      if (!is.null(TableDat_0[["hot"]])) {DT <- TableDat_0[["hot"]]}
    }
    if (!is.null(DT)) {
      rhandsontable(DT, readOnly = FALSE, fillHandle = list(direction = 'vertical', autoInsertRow = TRUE)) %>% 
        hot_col(col = 1, type = 'numeric', format = frmtTable1()) %>% 
        hot_validate_numeric(col = 1, min = 0, allowInvalid = FALSE) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_context_menu(
          customOpts = list(insert_row = list(name = "Add 10 rows at the bottom",
                                              callback = htmlwidgets::JS("function (key, options) {this.alter('insert_row', [0], 10); this.render();}"))))
    }
  })
  
  output$Indications <- renderRHandsontable(TableData())
  Indications <- reactive(HOT2R(input$Indications, IgnoreRowNames = TRUE))
  
  ConventionalMasses <- reactive({
    if (is.na(Indications())) return(data.frame(matrix(nrow = 1, ncol = 2)))
    
    Indications <- Indications()$Indication[!is.na(Indications()$Indication > 0)]
    if (length(Indications) == 0) return(data.frame(matrix(nrow = 1, ncol = 2)))
    correctedMasses <- sapply(Indications, function (x) {
      convMass(calibCert = NAWIDCC$NAWIDCC(), reading = x, units = input$UnitsTable)})
    crrMassUncertai <- sapply(Indications, function (x) {
      uncertConvMass(calibCert = NAWIDCC$NAWIDCC(), reading = x, units = input$UnitsTable,
                     d = as.numeric(input$current.d.value), d.units = NAWIDCC$NAWIDCC()$orgdUnits)})
    crrMassUncertai <- signif(crrMassUncertai, 2)
    return(data.frame(Conventional.mass = correctedMasses, Standard.uncertainty = crrMassUncertai))
  })
  
  
  output$ConvMassText <- renderUI(HTML(
    HTMELIZAME.Esta(ConventionalMasses(), dVal = as.numeric(input$current.d.value), OrgUnits = NAWIDCC$NAWIDCC()$orgdUnits, UnitsTable = input$UnitsTable)))
  
  ExportationGradeConvMassText <- reactive({
    minLength <- length(Indications()$Indication[!is.na(Indications()$Indication > 0)])
    dat <- NAWIDCC$NAWIDCC()
    ListedInfo <- list(
      'Field' = c('Name', 'Serial', 'Calibrated by', 'Certificate number', 'Date of calibration', 'Place of calibration'),
      'NAWI DCC information' = c(dat$balanceID, dat$serial, dat$institution, dat$add.info$CertificateNumber, as.character(dat$date), dat$add.info$CalibrationPlace),
      '___' = rep(NA, minLength),
      'Input indications' = Indications()$Indication[!is.na(Indications()$Indication > 0)],
      'Units' = rep(input$UnitsTable, minLength),
       'Conventional mass' = ConventionalMasses()[, 1],
       'Standard uncertainty' = ConventionalMasses()[, 2],
       'Units.CM' = rep(input$UnitsTable, minLength))
    return(data.frame(lapply(ListedInfo, "length<-", max(lengths(ListedInfo)))))
  })
  
  output$DwnlConvMassResCSV <- downloadHandler(
    filename = function() {paste0("Conventional_Mass_Measurement_Results_", format(Sys.time(), '%F_%R'), ".csv")}, 
    content = function(file) {write.csv(ExportationGradeConvMassText(), file = file, na = '', row.names = FALSE)}, contentType = NULL)
  output$DwnlConvMassResXLS <- downloadHandler(
    filename = function() {paste0("Conventional_Mass_Measurement_Results_", format(Sys.time(), '%F_%R'), ".xlsx")}, 
    content = function(file) {write_xlsx(ExportationGradeConvMassText(), path = file)}, contentType = NULL)
}

