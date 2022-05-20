calibCert(institution = input$institution, accreditation = input$accreditation, date = input$date,
          balanceID = input$balanceID, serial = input$serial, certificate = input$certificate, 
          d = input$d, d.units = input$d.units, 
          indError = t(HOT2R(input$HT.indicationError)[, -1]), indError.units = HOT2R(input$HT.indicationError)[, 1], 
          expanded = TRUE, k = input$IndErrorK, traceability = input$traceability, classSTD = input$classSTD,
          rep = SummarizeRepInput(HOT2R(input$HT.repeatability)), rep.units = rep(input$rep.units, 2),
          eccen = SummarizeEccenInput(HOT2R(input$HT.eccen)), eccen.units = rep(input$eccen.units, 2), 
          Temp = input$Temp, p = input$bPres, h = input$rHumi, 
          unitsENV = c(input$TempUnits, input$bPresUnits, input$rHumiUnits),
          add.info = add.info()
)

massSTD <- c(0.01, 0.5, 1, 10, 20, 50, 100, 120, 150, 200, 220) ## [g]
indError <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.2, -0.2) ## [mg]
uncert <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.5) ## [mg]
d <- 0.1 ## [mg]


(DCC <- calibCert(institution = input$institution, accreditation = input$accreditation, date = input$date,
                 balanceID = input$balanceID, serial = input$serial, certificate = input$certificate, 
                 d = input$d, d.units = input$d.units, 
                 #indError = data.frame(massSTD, indError, uncert),
                 indError = data.frame(t(HOT2R(input$HT.indicationError)[, -1])),
                 indError.units = HOT2R(input$HT.indicationError)[, 1],
                 expanded = TRUE, k = input$IndErrorK, traceability = input$traceability, classSTD = input$classSTD,
                 SummarizeRepInput(HOT2R(input$HT.repeatability)), rep.units = rep(input$rep.units, 2),
                 eccen = SummarizeEccenInput(HOT2R(input$HT.eccen)), eccen.units = rep(input$eccen.units, 2),
                 Temp = input$Temp, p = input$bPres, h = input$rHumi, 
                 unitsENV = c(input$TempUnits, input$bPresUnits, input$rHumiUnits),
                 add.info = add.info()
                 ))
DCC$indError

print(DCC, complete = TRUE)
