# Cargar im'agenes png https://www.rdocumentation.org/packages/png/versions/0.1-7/topics/readPNG

manageDCC.Server <- function(input, output, session) {
  observeEvent(input$brwzInsideModule, browser())
  
  ## Navigation between tabs
  {
  BoleanIncompleteAdminDat <- reactive(# FALSE)
    are.null.empty(c(input$institution, input$respPerson, input$balanceID, input$serial, input$certificate, input$date, input$calPlace)))
  BoleanIncompleteMeasurRes <- reactive(# FALSE)
    are.null.empty(c(input$d, HOT2R(input$HT.repeatability), HOT2R(input$HT.eccen), HOT2R(input$HT.indicationError), input$Temp1, input$bPres1, input$rHumi1)))
  
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
  observeEvent(input$Go2Comments, ignoreInit = TRUE,
               if (BoleanIncompleteMeasurRes()) {
                 showNotification('Please fill in all the required fields.', duration = 4, type = 'error')
                 updateTabItems(inputId = 'createDCC.TB', selected = 'Measurement results')
               } else {
                 updateTabItems(inputId = 'createDCC.TB', selected = 'Comments')
               })
  }
  
  ## Handsontables
  values <- reactiveValues()
  
  # Repeatability test
  {
    frmtReap <- reactive(format(round(DummyNumber, digits = abs(floor(log10(convertMassUnitsSI(input$d, from = input$d.units, to = input$rep.units))))))[1])
    RepeatTableSize <- reactive(c(input$ReapTestPoints, input$ReapTestMeaPerPoints))
    DF.repeatability <- reactive(data.frame(matrix(nrow = EnsureMinValue(RepeatTableSize()[1], 1), ncol = EnsureMinValue(RepeatTableSize()[2], 5), 
                                                   dimnames = list(paste0('Load No. ', 1:EnsureMinValue(RepeatTableSize()[1], 1)),
                                                                   paste0('Ind.', 1:EnsureMinValue(RepeatTableSize()[2], 5))))))

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
        rhandsontable(DF.repeatability, overflow = 'visible',  fillHandle = list(direction = 'horizontal'), rowHeaderWidth = 100) %>%
        hot_col(col = 1:EnsureMinValue(RepeatTableSize()[2], 5), type = 'numeric', allowInvalid = FALSE,
                format = frmtReap()#as.character(format(convertMassUnitsSI(input$d, from = input$d.units, to = input$rep.units), scientific = FALSE))
                ) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_rows(fixedRowsTop = input$ReapTestPoints) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE)
      }
    })
  }
  
  # Eccentricity test
  {
    frmtEccen <- reactive(format(round(DummyNumber, digits = abs(floor(log10(convertMassUnitsSI(input$d, from = input$d.units, to = input$eccen.units))))))[1])
    DF.eccen <- reactive(data.frame(Indication = rep(NA, 5), row.names = paste0('Position No. ', 1:5)))
    observe({
      if (!is.null(input$HT.eccen)) {
        DF.eccen <- HOT2R(input$HT.eccen)
      } else {
        if (is.null(values[["DF.eccen"]])) {DF.eccen <- DF.eccen} else {DF.eccen <- values[["DF.eccen"]]}
      }
      values[["DF.eccen"]] <- DF.eccen()
    })
    output$HT.eccen <- renderRHandsontable({
      DF.eccen <- values[["DF.eccen"]]
      if (!is.null(DF.eccen))
        rhandsontable(DF.eccen, rowHeaderWidth = 110, overflow = 'visible') %>% 
        hot_col(col = 1, type = 'numeric', allowInvalid = FALSE, format = frmtEccen()) %>%
        hot_cols(colWidths = 110) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_rows(fixedRowsTop = 5) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })
  }
  
  # Indication error
  {
    DF.indicationError <- reactive(data.frame(Units = c('g', 'mg', 'mg'), matrix(nrow = 3, ncol = EnsureMinValue(input$IndErrorPoints, 2)), 
                                              row.names = c('Nominal mass', 'Indication error', 'Uncertainty')))
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
        rhandsontable(DF.indicationError, colHeaders = c('Units', paste0('Point.', 1:EnsureMinValue(input$IndErrorPoints, 2))), rowHeaderWidth = 100, overflow = 'visible') %>% 
        hot_col(col = 2:(EnsureMinValue(input$IndErrorPoints, 2) + 1), type = 'numeric', allowInvalid = FALSE) %>%
        hot_col(col = 1, type = 'autocomplete', source = names(unitsOpt), strict = TRUE, allowInvalid = FALSE) %>%
        hot_cols(fixedColumnsLeft = 1)  %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_rows(fixedRowsTop = 3) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })
  }
  
  # CalibCert DCC creation
  NAWI.DCC.Completed <- reactiveVal(0)
  
  # When user tries to finish or update the DCC
  {output$FinishNAWIDCC1 <- renderUI(actionButton(
    inputId = session$ns('FinishNAWIDCC1'), label = tags$b(ifelse(NAWI.DCC.Completed() == 0, 'Finish NAWI DCC', 'Update NAWI DCC')), width = '100%'))
  output$FinishNAWIDCC2 <- renderUI(actionButton(
    inputId = session$ns('FinishNAWIDCC2'), label = tags$b(ifelse(NAWI.DCC.Completed() == 0, 'Finish NAWI DCC', 'Update NAWI DCC')), width = '100%'))
  output$FinishNAWIDCC3 <- renderUI({
    if(is.error(readRDS(input$NAWI.DCC_uploaded$datapath))) {
      return(tags$b('File must have .rds extension. The upload is not complete until a clickable button  that states',
                    tags$u('Upload selected NAWI DCC'), 'is shown and selected.'))
    } else {
      dataFile <- readRDS(input$NAWI.DCC_uploaded$datapath)
      if(class(dataFile) == 'calibCert') {
        return(actionButton(inputId = session$ns('FinishNAWIDCC3'), label = tags$b('Upload selected NAWI DCC'), width = '50%'))
      } else {
        return(tags$b('File does not seem to have been created in the platform... The upload is not complete until a clickable button that states',
                      tags$u('Upload selected NAWI DCC'), 'is shown and selected.'))
      }
    }
  })

  observeEvent(
    input$FinishNAWIDCC1,
    if (BoleanIncompleteMeasurRes()) {
      showNotification('NAWI DCC information is missing, please fill in all the required fields.', duration = 6, type = 'error')
    } else {
      NAWI.DCC.Completed(NAWI.DCC.Completed() + 1)
    })
  observeEvent(input$FinishNAWIDCC2, NAWI.DCC.Completed(NAWI.DCC.Completed() + 1))
  observeEvent(input$FinishNAWIDCC2, NAWI.DCC.Completed(NAWI.DCC.Completed() + 1))
  observeEvent(input$FinishNAWIDCC3, NAWI.DCC.Completed(NAWI.DCC.Completed() + 1))}

  # DCC creation
  { 
    logo <- reactive(readPNG(source = input$InstitutLogo$datapath))
    add.info <- reactive(list(
      masscorAppVersion = masscorAppVersion,
      Logo = tryCatch(logo(), error = function(x) return(FALSE)),
      ResponsiblePerson = input$respPerson,
      CalibrationPlace = input$calPlace,
      CompleteRepeatability = HOT2R(input$HT.repeatability),
      CompleteEccentricity = HOT2R(input$HT.eccen),
      Comments = list(Com1 = input$Comments1, Com2 = input$Comments2, Com3 = input$Comments3),
      CompleteEnvCond = c(input$Temp1, input$Temp2, input$bPres1, input$bPres2, input$rHumi1, input$rHumi2),
      CertificateNumber = input$certificate
    ))
    
    NAWIDCC <- eventReactive(
      eventExpr = NAWI.DCC.Completed(), ignoreInit = TRUE, 
      valueExpr = {
        if (input$SourceOption == "daCapo") {return(
          calibCert(institution = input$institution, accreditation = input$accreditation, date = input$date,
                    balanceID = input$balanceID, serial = input$serial, certificate = input$certificate, 
                    d = input$d, d.units = input$d.units, 
                    indError = data.frame(t(HOT2R(input$HT.indicationError)[, -1])), indError.units = HOT2R(input$HT.indicationError)[, 1], 
                    expanded = TRUE, k = input$IndErrorK, traceability = input$traceability, classSTD = input$classSTD,
                    rep = SummarizeRepInput(HOT2R(input$HT.repeatability)), rep.units = rep(input$rep.units, 2),
                    eccen = SummarizeEccenInput(HOT2R(input$HT.eccen)), eccen.units = rep(input$eccen.units, 2), 
                    Temp = mean(c(input$Temp1, input$Temp2)), p = mean(c(input$bPres1, input$bPres2)), h = mean(c(input$rHumi1, input$rHumi2)), 
                    unitsENV = c(input$TempUnits, input$bPresUnits, input$rHumiUnits),
                    add.info = add.info()
          ))
        } else {
          dataFile <- readRDS(input$NAWI.DCC_uploaded$datapath)
          if (class(dataFile) == 'calibCert') return(dataFile)
        }
      }
    )
  }
  
  # Create directory for every new institution
  tmp <- reactive(tempdir())
  FolderInstitution <- reactive(strsplit(x = NAWIDCC()$institution, split = ',')[[1]][1])
  subfldr <- reactive(NAWIDCC()$add.info$CertificateNumber)
  observe({
    if(!dir.exists(file.path(tmp(), FolderInstitution()))) {
      dir.create(path = file.path(tmp(), FolderInstitution()))
    }
  })
  #
  LclDirectory <- reactive(file.path(tmp(), FolderInstitution(), subfldr()))
  observe({
    dir.create(path = LclDirectory())
    addResourcePath(prefix = 'Human', directoryPath = LclDirectory())
    })
  
  
  # FolderInstitution <- reactive(strsplit(x = NAWIDCC()$institution, split = ',')[[1]][1])
  # subfldr <- reactive(NAWIDCC()$add.info$CertificateNumber)
  # observe({
  #   if(!dir.exists(paste0('www/Uploaded masscor NAWI DCC/', FolderInstitution()))) {
  #     dir.create(path = paste0('www/Uploaded masscor NAWI DCC/', FolderInstitution()))
  #   }
  # })
  # #
  # LclDirectory <- reactive(paste0('www/Uploaded masscor NAWI DCC/', FolderInstitution(), '/', subfldr()))
  # observe(dir.create(path = LclDirectory()))
   
  
    
  logoBolean <- reactiveVal(FALSE)
  observe({if (!is.error(logo())) {if (class(logo()) == "array") {
    writePNG(image = logo(), target = file.path(LclDirectory(), "Logo.png"))
    logoBolean(file.exists(file.path(LclDirectory(), "Logo.png")))}}})
  observe({if (class(NAWIDCC()$add.info$Logo) == "array") {
    writePNG(image = NAWIDCC()$add.info$Logo, target = file.path(LclDirectory(), "Logo.png"))
    logoBolean(file.exists(file.path(LclDirectory(), "Logo.png")))}})
  
  # Save copy
  observe({
    saveRDS(NAWIDCC(), file = file.path(LclDirectory(), "NAWIDCC.rds"))
  })
  
  downloadDCC1 <- eventReactive(
    eventExpr = NAWI.DCC.Completed(), ignoreInit = TRUE,
    downloadButton(session$ns('DwnlDCCFile1'), 'Download masscor NAWI DCC',  style = "width:95%;"))
  downloadPDF1 <- eventReactive(
    eventExpr = NAWI.DCC.Completed(), ignoreInit = TRUE,
    tags$div(
      a(href = file.path('Human', "Human_Readable_CC.pdf"),
          #file.path('Uploaded masscor NAWI DCC/', FolderInstitution(), subfldr(), "Human_Readable_CC.pdf"), 
        actionButton(icon = icon('download'), session$ns('DwnlPDFFile1'), 'Download human readable output (.pdf)',  style = "width:95%;"),
        download = NA, target = "_blank"),
      tags$br(), tags$br(),
      a(href = file.path('Human', "Human_Readable_CC.tex"),
        #file.path('Uploaded masscor NAWI DCC/', FolderInstitution(), subfldr(), "Human_Readable_CC.tex"), 
        actionButton(icon = icon('download'), session$ns('DwnlTexFile1'), 'Download LaTeX file (.tex)',  style = "width:95%;"),
        download = NA, target = "_blank")))
  
  
  output$DwnlDCCFile1 <- downloadHandler(
    filename = function() {paste0("DCC_NAWI_", input$balanceID, "_", input$serial, "_", input$date, ".rds")}, 
    content = function(file) {saveRDS(NAWIDCC(), file = file)}, contentType = NULL)
  
  
  output$pff <- renderUI({
    NAWIDCC <- NAWIDCC()
    tempReport <- file.path(LclDirectory(), "Human_Readable_CC.Rmd")
    file.copy("Rmd_LaTeX/Human_Readable_CC.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/eccen.png", file.path(LclDirectory(), "eccen.png"), overwrite = TRUE)
    params <- list(author = strsplit(x = NAWIDCC$institution, split = ',')[[1]][1],
                   address = strsplit(x = NAWIDCC$institution, split = ',')[[1]][2],
                   date = NAWIDCC$date, NAWIDCC = NAWIDCC, logoBolean = logoBolean())
    rmarkdown::render(tempReport, output_file = 'Human_Readable_CC.pdf', params = params, quiet = TRUE, envir = new.env(globalenv()))
    
    return(tags$iframe(style = "height:800px; width:100%; scrolling=yes", src = file.path('Human', "Human_Readable_CC.pdf")))})
                         #file.path('Uploaded masscor NAWI DCC/', FolderInstitution(), subfldr(), 'Human_Readable_CC.pdf')))})
  

  output$downloadDCC1 <- renderUI(downloadDCC1())
  output$downloadPDF1 <- renderUI(downloadPDF1())
  
  
  output$primitive <- renderPrint(ifelse(is.error(print(NAWIDCC())), return('Create a masscor NAWI DCC or upload a file'), return(NAWIDCC())))
  
  return(list('NAWIDCC' = NAWIDCC))
}