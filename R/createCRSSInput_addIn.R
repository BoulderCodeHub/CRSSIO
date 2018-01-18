
#' RStudio Addin to Create CRSS Input Files
#' 
#' `crss_input_addin()` is accessed via a RStudio Addin. It is a GUI 
#' (Shiny Gadget) used to set key parameters of [crssi_create_dnf_files()]
#' before running it.
#' 
#' To use the Addin, RStudio v0.99.878 or later must be used. The key user input
#' to \code{createCRSSDNFInputFiles} can be set in the GUI. The \code{oFiles}
#' arguement uses the default value of \code{\link{CRSSNFInputNames}}.
#' 
#' @seealso [crssi_create_dnf_files()], [nf_file_names()]
#' 
#' @import shiny
#' @import miniUI

crss_input_addin <- function() {
  
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
    miniContentPanel(
      fillCol(
        h4('Create CRSS Input Files from Observed Natural Flow Record Using the ISM Method'),
        
        h5('1. Select the start and end years of the natural flow record to apply the ISM to.'),
        fillRow(
          flex = c(NA,1),
          selectInput(
            'nfInputStartYear', 
            'Start Year:',
            choices = 1906:2020,
            selected = 1906
          ),
          selectInput(
            "nfInputEndYear",
            'End Year',
            choices = 1906:2020,
            selected = 2015
          )
        ),
        fillRow(htmlOutput('startEndErrors')),
        br(),
        
        # h5('2. Select the number of years in the simulation period, i.e., the number of years each trace of data will contain'),
        # fillRow(flex = c(NA,1),
        #         numericInput(
        #           'simYrs', 
        #           'Simulation Years:',
        #           50, 
        #           min = 1, 
        #           max = 107, 
        #           step = 1
        #         ),
        #         htmlOutput('simYrsCheck')
        #         ),
        # br(),
        
        h5('2. Select the simulation start and end years of the CRSS simulations.'),
        fillRow(
          flex = c(NA,1),
          selectInput(
            'traceStartYear',
            'Traces Start In:',
            choices = seq(2000, 2099),
            selected = 2018
          ),
          selectInput(
            "endYear", 
            "Traces End In:", 
            choices = seq(2000, 2099), 
            selected = 2060
          )
        ),
        fillRow(htmlOutput('simYrsCheck')),
        br(),
        
        h5('3. Select the folder to save the trace files in. The folder should already exist. Leave off the trailing "/".'),
        fillRow(
          flex = c(NA,1),
          textInput('selectFolder', 'Select Folder', value = 'C:/'), 
          radioButtons(
            "overwrite", 
            label = "Overwrite existing files?",
            choices = c("No" = FALSE, "Yes" = TRUE),
            selected = FALSE
          )      
        ),
        fillRow(htmlOutput('checkInputFolder')),
        br(),
        
        h5("4. Do you want to create the HistoricalNaturalFlows.xlsx file?"),
        fillRow(
          flex = NA,
          radioButtons(
            "createHistNF", 
            label = "Create HistoricalNaturalFlows.xlsx?",
            choices = c("No" = FALSE, "Yes" = TRUE),
            selected = TRUE
          )
        ),
        
        htmlOutput('checkAllErrors')
      )
      
    )
  )
  
  server <- function(input, output, session) {
    
    isStartYearValid <- reactive({
      as.integer(input$nfInputStartYear) >= 1906
    })
    
    isEndAfterStart <- reactive({
      as.integer(input$nfInputStartYear) <= as.integer(input$nfInputEndYear)
    })
    
    output$startEndErrors <- renderUI({
      errMsg <- ''
      
      if(!isStartYearValid())
        errMsg <- paste0(errMsg, "Start year should be after 1906", br())
      
      if(!isEndAfterStart())
        errMsg <- paste0(
          errMsg,
          "The end date should be after the start date.", 
          br()
        )
   
      div(class = "errorMessage", HTML(errMsg))
    })
    
    ismRange <- reactive({
      as.integer(input$nfInputEndYear) -
        as.integer(input$nfInputStartYear) + 1
    })
    
    isSimYrsValid <- reactive({
      as.integer(input$endYear) - as.integer(input$traceStartYear) + 1 <= 
        ismRange()
    })
    
    isEndYearValid <- reactive({
      as.integer(input$endYear) >= as.integer(input$traceStartYear)
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
    
    # isTraceStartValid <- reactive({
    #   format(zoo::as.Date(input$traceStartYear),"%m-%d") == "01-31"
    # })
    
    # output$traceStartCheck <- renderUI({
    #   if(!isTraceStartValid())
    #     div(class = "errorMessage", 
    #         HTML("Traces need to start on January 31 of some year."))
    #   else
    #     HTML("")
    # })
    
    isOutputFolderValid <- reactive({
      file.exists(input$selectFolder)
    })
    
    output$checkInputFolder <- renderUI({
      if(!isOutputFolderValid())
        div(class = "errorMessage", 
          HTML("Folder does not exist"))
      else
        HTML("")
    })
  
    
    isAllInputValid <- reactive({
      #isTraceStartValid() & 
      isSimYrsValid() & isOutputFolderValid() & isStartYearValid() & 
        isEndAfterStart()
    })
    
    output$checkAllErrors <- renderUI({
      if(!isAllInputValid())
        div(class="errorMessage",
            HTML("Please fix errors before clicking run."))
      else
        HTML("")
    })
    
    # Listen for 'done' events.
    observeEvent(input$done, {
      if(isAllInputValid()){
        rr <- zoo::as.yearmon(c(paste0(input$nfInputStartYear, "-1"),
                                paste0(input$nfInputEndYear, "-12")))
        crssi_create_dnf_files(
          'CoRiverNF',
          oFolder = input$selectFolder,
          startYear = as.integer(input$traceStartYear), 
          endYear = as.integer(input$endYear),
          recordToUse = rr,
          overwriteFiles = as.logical(input$overwrite)
        )
        message(paste('All trace files have been saved to: ',input$selectFolder))
        stopApp()
      }
    })
    
  }
  
  runGadget(ui, server, viewer = paneViewer(550))
  
}
