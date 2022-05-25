buoyancyCorrections.Server <- function(input, output, session, NAWIDCC) {
  observeEvent(input$brwzInsideModule, browser())
  
  failedFileBolean <- reactive({#reactiveTimer(5e3)
    if (is.error(class(NAWIDCC$NAWIDCC()))) {return(TRUE)} else {
      if (class(NAWIDCC$NAWIDCC()) != 'calibCert') {return(TRUE)} else {return(FALSE)}
    }})
  output$noDCC.loaded <- renderUI(if(failedFileBolean()) {return(errorBoxNoNAWIDCC)}) 
  
  rhoAir <- reactive({
    x <- airDensity(model = 'CIMP2007', Temp = input$Temp1, p = input$BarPres1, h = input$relHum1, 
                    unitsENV = c(temp = input$TempUnits, p = input$bPresUnits, h = input$rHumiUnits))
    if (input$densityUnits == 'KiloGramPerCubicMeter') {x <- x * 10^3}
    return(c(format(signif(x, 6), scientific = FALSE), densityAllowedUnits[which(densityAllowedUnits == input$densityUnits)]))
  })
  
  u_rhoAir <- reactive({
    x <- uncertAirDensity(model = 'CIMP2007', Temp = input$Temp1, p = input$BarPres1, h = input$relHum1, 
                          u_Temp = input$u_Temp1, u_p = input$u_BarPres1, u_h = input$u_relHum1, 
                          unitsENV = c(temp = input$TempUnits, p = input$bPresUnits, h = input$rHumiUnits))
    if (input$densityUnits == 'KiloGramPerCubicMeter') {x <- x * 10^3}
    return(c(format(signif(x, 2), scientific = FALSE), densityAllowedUnits[which(densityAllowedUnits == input$densityUnits)]))
  })
  
  niceRhoAir <- reactive(HTML(spcs(4), '(', rhoAir()[1], '\u00B1', 
                              ifelse(u_rhoAir()[1] == 'NA', '(no uncertainties)', u_rhoAir()[1]), ')', spcs(2), 
                              densityHTMLUnits[input$densityUnits]))
  output$airDensityStatment <- renderUI(HTML(ifelse(is.error(niceRhoAir()), '-Provide environmental conditions data-', niceRhoAir())))
  
  
  TableDat_0  <- reactiveValues(hot = data.frame('Density' = c(0, rep(NA, 9)), 'Uncertainty' = c(0, rep(NA, 9))))#,  'Conventional.mass' = rep(NA, 14), 'Standard.uncertainty' = rep(NA, 14)))
  TableDensities <- reactive({
    DT <- NULL
    if (!is.null(input$HT.ConventionalMass)) {
      DT <- HOT2R(input$HT.ConventionalMass)
      TableDat_0[["hot"]]  <-  DT
    } else {#For initial data upload
      if (!is.null(TableDat_0[["hot"]])) {DT <- TableDat_0[["hot"]]}
    }
    if (!is.null(DT)) {
      rhandsontable(DT, readOnly = FALSE, fillHandle = list(direction = 'vertical', autoInsertRow = TRUE)) %>% 
        hot_col(col = 1:2, type = 'numeric', format = '0.0000') %>% 
        hot_validate_numeric(col = 1:2, min = 0, allowInvalid = FALSE) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_context_menu(
          customOpts = list(insert_row = list(name = "Add 10 rows at the bottom",
                                              callback = htmlwidgets::JS("function (key, options) {this.alter('insert_row', [0], 10); this.render();}"))))
    }
  })
  output$Densities <- renderRHandsontable(TableDensities())
  Densities <- reactive(HOT2R(input$Densities, IgnoreRowNames = TRUE))
  
  MABC_Factors <- reactive({
    rho_w <- ifelse(input$densityUnits == 'KiloGramPerCubicMeter', 8000, 8)
    u_rho_w <- ifelse(input$densityUnits == 'KiloGramPerCubicMeter', 60, 0.06)
    if (is.na(Densities())) return(data.frame(matrix(nrow = 1, ncol = 2)))
    
    Densities     <- Densities()$Density[!is.na(Densities()$Density > 0)]
    Uncertainties <- Densities()$Uncertainty[!is.na(Densities()$Uncertainty > 0)]
    if (length(Densities) == 0) return(data.frame(matrix(nrow = 1, ncol = 2)))
    
    mat <- matrix(nrow = min(c(length(Densities), length(Uncertainties))), ncol = 2)
    rhoAir <- as.numeric(rhoAir()[1])
    u_rhoAir <- as.numeric(u_rhoAir()[1])
    
    for (i in 1:nrow(mat)) {
      mat[i, 1] <- MABC(rho = Densities[i], rho_w = rho_w, rho_air = rhoAir)
      mat[i, 2] <- uncertMABC(rho = Densities[i], rho_w = rho_w, rho_air = rhoAir, 
                              u_rho = Uncertainties[i], u_rho_w = u_rho_w, u_rho_air = u_rhoAir)
    }
    
    return(data.frame(MABC = mat[, 1], uncertainty.MABC = mat[, 2]))
  })
  
  output$MABC.TextResults <- renderUI(
    HTML(ifelse(is.error(niceRhoAir()), '-Provide environmental conditions and object densities data-', paste0(HTMELIZAME.Esta.MABC(MABC_Factors()), collapse = ''))))
  
  ExportationGradeMABC.TextResults <- reactive({
    minLength <- nrow(MABC_Factors())
    #dat <- NAWIDCC$NAWIDCC()
    ListedInfo <- list(
      'Quantity' = c('Temperature', 'Barometric pressure', 'Relative humidity', NA, 'Air density'),
      'Value' = c(input$Temp1, input$BarPres1, input$relHum1, NA, as.numeric(rhoAir()[1])),
      'Uncertainty' = c(input$u_Temp1, input$u_BarPres1, input$u_relHum1, NA, as.numeric(u_rhoAir()[1])),
      'Units' = c(input$TempUnits, input$bPresUnits, input$rHumiUnits, '', densityPlainUnits[input$densityUnits]),
      '___' = rep(NA, minLength),
      'Object density' = Densities()$Density[!is.na(Densities()$Density > 0)],
      'Object density uncertainty' =  Densities()$Uncertainty[!is.na(Densities()$Uncertainty > 0)],
      'Density Units' = rep(densityPlainUnits[input$densityUnits], minLength),
      '____' = rep(NA, minLength),
      'MABC factor' = MABC_Factors()[, 1],
      'MABC factor uncertainty' = MABC_Factors()[, 2])
    
    return(data.frame(lapply(ListedInfo, "length<-", max(lengths(ListedInfo)))))
  })
  
  output$DwnlMABCsCSV <- downloadHandler(
    filename = function() {paste0("MABC_Factor_Results_", format(Sys.time(), '%F_%R'), ".csv")}, 
    content = function(file) {write.csv(ExportationGradeMABC.TextResults(), file = file, na = '', row.names = FALSE)}, contentType = NULL)
  output$DwnlMABCsXLS <- downloadHandler(
    filename = function() {paste0("MABC_Factor_Results_", format(Sys.time(), '%F_%R'), ".xlsx")}, 
    content = function(file) {write_xlsx(ExportationGradeMABC.TextResults(), path = file)}, contentType = NULL)
  
  
  
  
  
  
  # choices.d <- reactive(convertMassUnitsSI(NAWIDCC$NAWIDCC()$d, from = NAWIDCC$NAWIDCC()$standardUnits, to = NAWIDCC$NAWIDCC()$orgdUnits) * c(1, 10, 100))
  # output$current.d.value <- renderUI(radioButtons(session$ns('current.d.value'), label = 'NAWI scale division used for indications:', 
  #                                                 choiceNames = paste0(choices.d(), NAWIDCC$NAWIDCC()$orgdUnits, sep = ' '), 
  #                                                 choiceValues = choices.d(), inline = TRUE))
  
  
}