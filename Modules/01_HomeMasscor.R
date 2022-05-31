homeMasscor.UI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 6, offset = 1, h3(tags$b('Mass measurement corrections using the R package masscor')), tags$br(),
    #fluidRow(
     # column(
      #  width = 7, 
        h4("The R package", tags$a(href = "https://CRAN.R-project.org/package=masscor", tags$b("masscor"), target = "_blank"), "provides classes, functions
           and methods for storing and using the calibration information of
           non-automatic weighing instruments (NAWI).", tags$br(), tags$br(),
           tags$b("masscor"), "presents a prototipe of Digital Calibration Certificate (DCC) for NAWIs", tags$br(), tags$br(), 
           "The NAWI DCC created in ", tags$b("masscor"), " is useful to automatically
           calculate mass measurement corrections, with their associated uncertainties, as recommended by the", 
           tags$a(href = "https://www.euramet.org/Media/docs/Publications/calguides/I-CAL-GUI-018_Calibration_Guide_No._18_web.pdf", 
                  "EURAMET's guideline No. 18 (2015).", target = "_blank"),
           tags$br(), tags$br(),
           "The mass measurement corrections are used to convert balance readings to both conventional masses and (real) masses. 
           For the latter, the magnitudes of air buoyancy correction factors are calculed using the 
           density of local air, wich in turn, is estimated from environmental conditions using the ",
           tags$a(href = "https://iopscience.iop.org/article/10.1088/0026-1394/45/2/004/pdf", "CIMP-2007 formula (Picard, Davis, Gläser, and Fujii, 2008).", target = "_blank"),
           tags$br(), tags$br(),
           "NOTE: The version of the DCC proposed in ", tags$b("masscor"), " does not align to the", 
           tags$a(href = "https://www.ptb.de/cms/en/research-development/into-the-future-with-metrology/the-challenges-of-digital-transformation/kernziel1einheitlichkeitim/digital-calibration-certificate-dcc.html", 
                  "XML based schema of DCC,", target = "_blank"),
           "implemented by the ", tags$a(href = "https://www.ptb.de/cms/en.html", "Physikalisch-Technische Bundesanstalt (PTB)", target = "_blank"), 
           "and whose aim is to create an internationally recognized DCC format.",
           "The main purpuse of the", tags$b("masscor NAWI DCC"), "is to serve to the R users of the metrological community that employ 
           NAWIs in their measurement processes and R in their data treatment."),
        tags$br(), tags$br(),
        h5("This Web Application was made in the free software environment for statistical computing and graphics ", tags$a(href = "https://www.r-project.org/", "R", target = "_blank"), ", using the package",
           tags$a(href = "https://shiny.rstudio.com/", "Shiny,", target = "_blank"), "designed to build interactive web apps with R.", tags$br(), "A valuable introduction to R programming can be found from", 
           tags$a(href = "https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf", "Venables and Smith (2022).", target = "_blank"))
      ),
      column(width = 4, div(img(src = "E2_2kg.jpg", width = '85%'), style = "text-align: center;"), tags$br(), tags$br()))
}

homeMasscor.Server <- function(input, output, session) {
  
}

