
#' @import shiny
#' @import miniUI

createCRSSInputAddIn <- function() {
  
  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniPage(
    tags$head(
      tags$style(HTML("
        .errorMessage {
          color: red;
        }
                      "))  
    ),
    
    gadgetTitleBar("Create CRSS Input Files", 
                   right = miniTitleBarButton("done","Close and Run", primary = TRUE)),
    miniContentPanel(
      fillCol(
        h4('Create CRSS Input Files from Observed Natural Flow Record Using the ISM Method'),
        
        h5('1. Select the start and end months of the natural flow record to use the ISM method on. This should be January 1 of some year through December 31 of some year.'),
        fillRow(flex = c(NA,1),
                dateRangeInput('nfInputYrs', 'Start and End Months:',
                               start = '1906-01-01',end = '2012-12-31', startview = 'decade'),
                htmlOutput('startEndErrors')
                ),
        br(),
        
        h5('2. Select the number of years in the simulation period, i.e., the number of years each trace of data will contain'),
        fillRow(flex = c(NA,1),
                numericInput('simYrs', 'Simulation Years:',50, min = 1, max = 107, step = 1),
                htmlOutput('simYrsCheck')
                ),
        br(),
        
        h5('3. Select the start date of the trace files. (Should be January 31 of some year.)'),
        fillRow(flex = c(NA,1),
                dateInput('traceStartYear','Traces Start In:',value = '2017-01-31',
                          startview = "decade"), 
                htmlOutput('traceStartCheck')
                ),
        br(),
        
        h5('4. Select the folder to save the trace files in. The folder should already exist.'),
        fillRow(flex = c(NA,1),
                textInput('selectFolder', 'Select Folder', value = 'C:/'), 
                htmlOutput('checkInputFolder')
                ),
        br(),
        
        htmlOutput('checkAllErrors')
      )
      
    )
  )
  
  server <- function(input, output, session) {

    isStartDateValid <- reactive({
      format(zoo::as.Date(input$nfInputYrs[1]), "%m-%d") == "01-01"
    })
    
    isStartYearValid <- reactive({
      as.numeric(format(zoo::as.Date(input$nfInputYrs[1]),"%Y")) >= 1906
    })
    
    isEndDateValid <- reactive({
      format(zoo::as.Date(input$nfInputYrs[2]), "%m-%d") == "12-31"
    })
    
    isEndAfterStart <- reactive({
      !(as.numeric(format(zoo::as.Date(input$nfInputYrs[2]),"%Y")) < 
        as.numeric(format(zoo::as.Date(input$nfInputYrs[1]),"%Y")))
    })
    
    output$startEndErrors <- renderUI({
      errMsg <- ''
      if(!isStartDateValid())
        errMsg <- paste0(errMsg,"Start date needs to be January 1, some year.",br())
      
      if(!isStartYearValid())
        errMsg <- paste0(errMsg, "Start year should be after 1906", br())
      
      if(!isEndDateValid())
        errMsg <- paste0(errMsg,"End date needs to be December 31, some year.", br())
      
      if(!isEndAfterStart())
        errMsg <- paste0(errMsg,"The end date should be after the start date.", br())
   
      div(class = "errorMessage", HTML(errMsg))
    })
    
    ismRange <- reactive({
      as.numeric(format(zoo::as.Date(input$nfInputYrs[2]),"%Y")) -
        as.numeric(format(zoo::as.Date(input$nfInputYrs[1]),"%Y")) + 1
    })
    
    isSimYrsValid <- reactive({
      input$simYrs <= ismRange()
    })
    
    output$simYrsCheck <- renderUI({
      if(!isSimYrsValid())
        div(class = "errorMessage",
            HTML("Simulation Years cannot be longer than the number of years in the record from step 1."))
      else
        HTML("")
    })
    
    isTraceStartValid <- reactive({
      format(zoo::as.Date(input$traceStartYear),"%m-%d") == "01-31"
    })
    
    output$traceStartCheck <- renderUI({
      if(!isTraceStartValid())
        div(class = "errorMessage", 
            HTML("Traces need to start on January 31 of some year."))
      else
        HTML("")
    })
    
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
      isTraceStartValid() & isSimYrsValid() & isOutputFolderValid() &
        isStartDateValid() & isStartYearValid() & isEndDateValid() & isEndAfterStart()
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
        createCRSSDNFInputFiles('CoRiverNF',oFolder = input$selectFolder,
                              startDate = input$traceStartYear, simYrs = input$simYrs,
                              recordToUse = input$nfInputYrs)
        message(paste('All trace files have been saved to: ',input$selectFolder))
        stopApp()
      }
    })
    
  }
  
  runGadget(ui, server, viewer = paneViewer(550))
  
}
