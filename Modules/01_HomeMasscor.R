homeMasscor.UI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 5, offset = 1, h3(tags$b('Mass measurement corrections using the R package masscor')), tags$br(),
    #fluidRow(
     # column(
      #  width = 7, 
        h4("The R package", tags$a(href = "https://CRAN.R-project.org/package=masscor", tags$b("masscor"), target = "_blank"), "provides classes, functions,
           and methods for storing and using the calibration information of
           non-automatic weighing instruments (NAWI). This ", tags$b("masscor"), 
           "graphical user interface (GUI) presents a Digital Calibration Certificate (DCC) prototype for NAWIs.", tags$br(), tags$br(), 
           "The NAWI DCCs created in ", tags$b("masscor"), " may be used to automatically
           calculate mass measurement corrections with their associated uncertainties, as recommended by the", 
           tags$a(href = "https://www.euramet.org/Media/docs/Publications/calguides/I-CAL-GUI-018_Calibration_Guide_No._18_web.pdf", 
                  "EURAMET's guideline No. 18 (2015).", target = "_blank"),
           tags$br(), tags$br(),
           "The mass measurement corrections are determined using the calibration information and convert balance readings into conventional masses. 
           Conventional masses can further be converted into (real) masses calculating the magnitudes of air buoyancy correction factors. A module for are calculated using the 
           density of local air, which in turn, is estimated from environmental conditions using the ",
           tags$a(href = "https://iopscience.iop.org/article/10.1088/0026-1394/45/2/004/pdf", "CIMP-2007 formula (Picard, Davis, Gläser, and Fujii, 2008).", target = "_blank"),
           tags$br(), tags$br(), 
           h4(em('masscor GUI Version: 0.1.12'), class = 'rightAlign', style = "text-align: right;"),
           tags$br(), tags$hr()),
           #"The purpose of the", tags$b("masscor NAWI DCC"), "is to serve the users of NAWIs in their measurement processes."),
        h5(
          "NOTES:", tags$br(),
          tags$ul(
            tags$li("The version of the DCC proposed in ", tags$b("masscor"), " does not align (yet) with the", 
                    tags$a(href = "https://www.ptb.de/cms/en/research-development/into-the-future-with-metrology/the-challenges-of-digital-transformation/kernziel1einheitlichkeitim/digital-calibration-certificate-dcc.html", 
                           "XML-based schema of DCC,", target = "_blank"),
                    "implemented by the ", tags$a(href = "https://www.ptb.de/cms/en.html", "Physikalisch-Technische Bundesanstalt (PTB).", target = "_blank"), 
                    "The PTB XML-based schema of DCC plans to create an internationally recognized DCC format to be used in all areas of metrology, 
                     while the masscor scope is limited to NAWIs."),
            tags$br(),
            tags$li("This Web Application was made in the free software environment for statistical computing and graphics ", tags$a(href = "https://www.r-project.org/", "R,", target = "_blank"), 
                    "using the package",
                    tags$a(href = "https://shiny.rstudio.com/", "Shiny,", target = "_blank"), "designed to build interactive web apps with R.",
                    "A valuable introduction to R programming can be found in", 
                    tags$a(href = "https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf", "Venables and Smith (2022).", target = "_blank")))),
      tags$hr()),
    column(width = 5, div(img(src = "E2_2kg.jpg", width = '85%'), style = "text-align: center;"), tags$br(), tags$br()))
}

homeMasscor.Server <- function(input, output, session) {
  
}

