
#' RStudio Addin to Create CRSS Input Files
#' 
#' `crss_input_addin()` is an RStudio Addin that sets key parameters of 
#' [crssi_create_dnf_files()], [crssi_create_cmip_nf_files()], and 
#' [crssi_create_hist_nf_xlsx()] before running it.
#' 
#' To use the Addin, RStudio v0.99.878 or later must be used. The key user input
#' to `crssi_create_dnf_files()` and `crssi_create_cmip_nf_files()` can be set 
#' in the GUI. The `oFiles` argument uses the default value of 
#' [nf_file_names()].
#' 
#' Additionally, there is an option to also create the HistoricalNaturalFlows.xlsx
#' file necessary for CRSS runs via `crssi_create_hist_nf_xlsx()`. 
#' 
#' @seealso [crssi_create_dnf_files()], [crssi_create_hist_nf_xlsx()]
#' 
#' @import shiny
#' @import miniUI

crss_input_addin <- function() {
  divHeight <- "50px"
  padLeft <- "padding-left: 10px;"
  
  max_year <- function() {
    year(Sys.Date(), TRUE) + (year(end(CoRiverNF::cyAnnTot), TRUE) - 
      year(start(CoRiverNF::cyAnnTot), TRUE) + 1)
  }
  
  ui <- miniPage(
    tags$head(
      tags$style(HTML("
        .errorMessage {
          color: red;
        }
                      "))  
    ),
    
    gadgetTitleBar(
      "Create CRSS Input Files", 
      right = miniTitleBarButton("done","Close and Run", primary = TRUE)
    ),
    miniContentPanel(padding = 0,
      # select files to create -------------------------                     
      fillRow(
        checkboxGroupInput(
          "createFiles",
          label = "Select files to create:",
          choices = c("DNF Files" = "dnf", "CMIP Files" = "cmip5", 
                      "HistoricalNaturalFlows.xlsx" = "histNF"),
          selected = c("dnf", "histNF"), 
          inline = TRUE
        ), 
        height = divHeight,
        style = padLeft
      ),
      
      # simulation start and end years ------------
      h4(htmlOutput("simYearTitle"), "style" = padLeft),
      h5(htmlOutput("simYearHeader"), "style" = padLeft),
      fillRow(
        uiOutput("simStartYearUI"),
        uiOutput("simEndYearUI"),
        htmlOutput("simYrsCheck"),
        height = divHeight,
        "style" = padLeft
      ),
      
      # show observed record options -----------------
      h4(htmlOutput("dnfSectionHeader"), "style" = padLeft),
      h5(htmlOutput("nfRecordHeader"), "style" = padLeft),
      fillRow(
        uiOutput("nfRecordStart"),
        uiOutput("nfRecordEnd"),
        htmlOutput("dnfStartEndErrors"), 
        height = divHeight,
        "style" = padLeft
      ),
      
      h5(htmlOutput("dnfFolderHeader"), "style" = padLeft),
      fillRow(
        uiOutput("dnfFolderOut"), 
        uiOutput("dnfOverwriteUI"),
        htmlOutput("checkInputFolder"),
        height = divHeight,
        "style" = padLeft
      ),
      
      # show CMIP options -------------------------------
      h4(htmlOutput("cmipSectionHeader"), "style" = padLeft),
      h5(htmlOutput("cmipInputHeader"), "style" = padLeft),
      fillRow(
        uiOutput("cmipIFileUI"),
        uiOutput("cmipScenNumUI"),
        htmlOutput("checkCmip5IFile"),
        height = divHeight,
        "style" = padLeft
      ),
      
      h5(htmlOutput("cmipInputHeader2"), "style" = padLeft),
      fillRow(
        uiOutput("cmipOFolderUI"),
        uiOutput("cmipOverwriteUI"),
        htmlOutput("checkCmip5OFolder"),
        height = divHeight,
        "style" = padLeft
      ),

      # if xlsx, select the parameters of that file -------------------
      h4(htmlOutput("histNfSectionHeader"), "style" = padLeft),
      fillRow(
        uiOutput("xlAvg"),
        uiOutput("xlPath"),
        htmlOutput("checkXlFolder"),
        height = divHeight,
        "style" = padLeft
      ),
      br(), br(), br(), br(),
      
      # final validation ----------------
      fillRow(
        htmlOutput("checkAllErrors"), 
        height = divHeight, 
        "style" = "padding-left: 10px; padding-top: 50px"
      )
    
    )
  )
  
  server <- function(input, output, session) {
    
    isDnfStartYearValid <- reactive({
      if (is.null(input$nfInputStartYear))
        return(TRUE)
      else
        as.integer(input$nfInputStartYear) >= 1906
    })
    
    isDnfEndAfterStart <- reactive({
      if (is.null(input$nfInputStartYear) | is.null(input$nfInputEndYear))
        return(TRUE)
      else
        as.integer(input$nfInputStartYear) <= as.integer(input$nfInputEndYear)
    })
    
    output$dnfStartEndErrors <- renderUI({
      errMsg <- ""
      
      if (!(is.null(input$nfInputStartYear) | is.null(input$nfInputEndYear))) {
        if (!isDnfStartYearValid())
          errMsg <- paste0(errMsg, "Start year should be after 1906", br())
        
        if(!isDnfEndAfterStart())
          errMsg <- paste0(
            errMsg,
            "The end date should be after the start date.", 
            br()
          )
      }
   
      div(class = "errorMessage", HTML(errMsg))
    })
    
    ismRange <- reactive({
      if(is.null(input$nfInputStartYear) | is.null(input$nfInputEndYear))
        return(10000)
      else
        as.integer(input$nfInputEndYear) -
          as.integer(input$nfInputStartYear) + 1
    })
    
    isSimYrsValid <- reactive({

      if ( all(
        isDnfSelected(), 
        !is.null(input$simEndYear), 
        !is.null(input$traceStartYear)
      )){
        as.integer(input$simEndYear) - as.integer(input$traceStartYear) + 1 <= 
          ismRange()
      } else{
        TRUE
      }
    })
    
    isEndYearValid <- reactive({
      if ( all(
        isDnfSelected() | isCmipSelected(),
        !is.null(input$simEndYear),
        !is.null(input$traceStartYear)
      ))
        as.integer(input$simEndYear) >= as.integer(input$traceStartYear)
      else
        TRUE
    })
    
    output$simYrsCheck <- renderUI({

      if (!isSimYrsValid())
        div(
          class = "errorMessage",
          HTML("Simulation Years cannot be longer than the number of years in the record from step 1.")
        )
      else if (!isEndYearValid())
        div(
          class = "errorMessage",
          HTML("The model run end year should be >= the model run start year.")
        )
      else
        HTML("")
    })
    
    isOutputFolderValid <- reactive({
      if (isDnfSelected() & !is.null(input$selectFolder))
        dir.exists(input$selectFolder)
      else
        TRUE
    })
    
    output$checkInputFolder <- renderUI({
      if(!isOutputFolderValid())
        div(class = "errorMessage", 
          HTML("Folder does not exist"))
      else
        HTML("")
    })
    
    # check the simulation options ------------------------
    
    output$simStartYearUI <- renderUI({
      if (isDnfSelected() | isCmipSelected() | isHistNfSelected())
        selectInput(
          'traceStartYear',
          'Traces Start In:',
          choices = seq(2000, max_year()),
          selected = year(Sys.Date(), numeric = TRUE) + 1
        )
    })
    
    output$simEndYearUI <- renderUI({
      if (isDnfSelected() | isCmipSelected())
        selectInput(
          "simEndYear", 
          "Traces End In:", 
          choices = seq(2000, max_year()), 
          selected = 2060
        )
    })
    
    output$simYearHeader <- renderText({
      if (isDnfSelected() | isCmipSelected() | isHistNfSelected())
        "Select the simulation start and end years of the CRSS simulations."
      else
        ""
    })
    
    output$simYearTitle <- renderText({
      if (isDnfSelected() | isCmipSelected() | isHistNfSelected())
        "Simulation Start and End Years"
      else
        ""
    })
    
    # check the DNF creation options ----------------------
    isDnfSelected <- reactive({
      !is.null(input$createFiles) & "dnf" %in% input$createFiles
    })
    
    output$nfRecordStart <- renderUI({

      if (isDnfSelected()) {
        selectInput(
          "nfInputStartYear", 
          "Start Year:",
          choices = 1906:2020,
          selected = 1906
        )
      } else 
        return()
    })
    
    output$nfRecordEnd <- renderUI({
      if (isDnfSelected()) {
        selectInput(
          "nfInputEndYear",
          'End Year',
          choices = 1906:2020,
          selected = year(end(CoRiverNF::cyAnnTot), TRUE)
        )
      } else
        return()
    })
    
    output$nfRecordHeader <- renderText({
      if (isDnfSelected())
          "Select the years to apply ISM to:"
      else
        return("")
    })
    
    output$dnfFolderOut <- renderUI({
      if (isDnfSelected())
        textInput('selectFolder', 'Select Folder', value = 'C:/')
      else
        return()
    })
    
    output$dnfOverwriteUI <- renderUI({
      if (isDnfSelected())
        radioButtons(
          "overwriteDnf", 
          label = "Overwrite existing files?",
          choices = c("No" = FALSE, "Yes" = TRUE),
          selected = FALSE,
          inline = TRUE
        )
      else
        return()
    })
    
    output$dnfFolderHeader <- renderText({
      if (isDnfSelected())
        "Select the folder to save the trace files in. The folder should already exist."
      else
        return("")
    })
    
    output$dnfSectionHeader <- renderText({
      if (isDnfSelected())
        "Create Direct Natural Flow Options"
      else
        return("")
    })
    
    # check CMIP creation options ------------------------
    isCmipSelected <- reactive({
      !is.null(input$createFiles) & "cmip5" %in% input$createFiles
    })
    
    isCmipFileValid <- reactive({
      if (isCmipSelected() & !is.null(input$cmipFile))
        file.exists(input$cmipFile)
      else
        TRUE
    })
    
    isCmipNCFile <- reactive({
      if (isCmipSelected() & !is.null(input$cmipFile))
        tools::file_ext(input$cmipFile) == "nc"
      else
        TRUE
    })
    
    output$checkCmip5IFile <- renderUI({
      errMsg <- ""
      
      if (!isCmipFileValid())
        errMsg <- paste(errMsg, "Netcdf file does not exist.")
      
      if (!isCmipNCFile())  
        errMsg <- paste(errMsg, "Please specify a '.nc' file.")
      
      div(class = "errorMessage", HTML(errMsg))
    })
    
    output$cmipSectionHeader <- renderText({
      if (isCmipSelected())
        "Create CMIP Natural Flow File Options"
      else
        ""
    })
    
    output$cmipInputHeader <- renderText({
      if (isCmipSelected())
        "Select the input netcdf file and the scenario number you wish to use."
      else
        ""
    })
    
    output$cmipInputHeader2 <- renderText({
      if (isCmipSelected())
        "Select folder to save CMIP natural flow files to."
      else
        ""
    })
    
    output$cmipIFileUI <- renderUI({
      if (isCmipSelected())
        textInput(
          "cmipFile", 
          "Select CMIP netcdf file to use:", 
          value = "C:/test.nc"
        )
    })
    
    output$cmipScenNumUI <- renderUI({
      if (isCmipSelected())
        textInput("cmipScenNum", label = "Scenario number:", value = "5")
    })
    
    output$cmipOFolderUI <- renderUI({
      if (isCmipSelected())
        textInput("cmipOFolder", "Select folder:", value = "C:/")
    })
    
    output$cmipOverwriteUI <- renderUI({
      if (isCmipSelected())
        radioButtons(
          "overwriteCmip", 
          label = "Overwrite existing files?",
          choices = c("No" = FALSE, "Yes" = TRUE),
          selected = FALSE,
          inline = TRUE
        )
      else
        return()
    })
    
    isCmipOutputFolderValid <- reactive({
      if (isCmipSelected() & !is.null(input$cmipOFolder))
        dir.exists(input$cmipOFolder)
      else
        TRUE
    })
    
    output$checkCmip5OFolder <- renderUI({
      if(!isCmipOutputFolderValid())
        div(class = "errorMessage", 
            HTML("Folder does not exist"))
      else
        HTML("")
    })
    
    # check the natural flow xlsx creation options -------------
    isHistNfSelected <- reactive({
      !is.null(input$createFiles) & "histNF" %in% input$createFiles
    })
    
    output$xlAvg <- renderUI({
      if (isHistNfSelected())
        sliderInput(
          "xlAvg", 
          "Select number of years to average when filling LB flow data",
          min = 1, 
          max = 20, 
          value = 5
        )
      else
        return()
    })
    
    output$xlPath <- renderUI({
      if (isHistNfSelected())
        textInput("xlPath", "Select folder to save file in:", value = "C:/")
      else
        return()
    })
    
    isXlPathValid <- reactive({
      
      if (isHistNfSelected() & !is.null(input$xlPath))
        return(dir.exists(input$xlPath))
      else
        # if you aren't creating the excel file, always return true for this
        TRUE
    })
    
    output$checkXlFolder <- renderUI({
      if(!isXlPathValid())
        div(class = "errorMessage", HTML("Folder does not exist"))
      else
        HTML("")
    })
    
    output$histNfSectionHeader <- renderUI({
      if (isHistNfSelected())
        "Create HistoricalNaturalFlows.xlsx Options"
      else
        ""
    })
    
    # check all output errors ----------------------
    
    isAllInputValid <- reactive({
      isSimYrsValid() & isOutputFolderValid() & isDnfStartYearValid() & 
        isDnfEndAfterStart() & isXlPathValid() & isCmipFileValid() &
        isCmipOutputFolderValid() & isCmipNCFile()
    })
    
    output$checkAllErrors <- renderUI({
      if(!isAllInputValid())
        div(
          class = "errorMessage",
          HTML("Please fix errors before clicking run.")
        )
      else
        HTML("")
    })
    
    # done --------------
    # Listen for 'done' events.
    observeEvent(input$done, {
      if(isAllInputValid()){
        rr <- zoo::as.yearmon(c(paste0(input$nfInputStartYear, "-1"),
                                paste0(input$nfInputEndYear, "-12")))
        if (isDnfSelected()) {
          crssi_create_dnf_files(
            "CoRiverNF",
            oFolder = input$selectFolder,
            startYear = as.integer(input$traceStartYear), 
            endYear = as.integer(input$simEndYear),
            recordToUse = rr,
            overwriteFiles = as.logical(input$overwriteDnf)
          )
          message(paste("\nAll DNF trace files have been saved to:", 
                        input$selectFolder))
        }
        
        if (isCmipSelected()) {
          crssi_create_cmip_nf_files(
            input$cmipFile, 
            oFolder = input$cmipOFolder,
            startYear = as.integer(input$traceStartYear),
            endYear = as.integer(input$simEndYear),
            scenarioNumber = input$cmipScenNum ,
            overwriteFiles = as.logical(input$overwriteCmip)
          )
          message(paste("\nAll CMIP trace files have been saved to:", 
                        input$cmipOFolder))
        }
        
        if (isHistNfSelected()) {
          crssi_create_hist_nf_xlsx(
            as.integer(input$traceStartYear), 
            nYearAvg = as.integer(input$xlAvg), 
            oFolder = input$xlPath
          )
          message(paste("\nHistoricalNaturalFlow.xlsx saved to:", input$xlPath))
        }
        
        stopApp()
      }
    })
    
  }
  
  runGadget(ui, server, viewer = paneViewer(550))
  
}
