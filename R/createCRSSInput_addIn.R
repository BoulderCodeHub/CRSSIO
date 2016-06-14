
#' @import shiny
#' @import miniUI

# We'll wrap our Shiny Gadget in an addin.
# Let's call it 'clockAddin()'.
createCRSSInputAddIn <- function() {
  
  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniPage(
    gadgetTitleBar("Create CRSS Input Files", 
                   right = miniTitleBarButton("done","Run", primary = TRUE)),
    miniContentPanel(
        h4('Create CRSS Input Files from Observed Natural Flow Record Using the ISM Method'),
        
        p('1. Select the start and end months of the natural flow record to use the ISM method on. This should be January 1 of some year through December 31 of some year.'),
        dateRangeInput('nfInputYrs', 'Start and End Months:',
                       start = '1906-01-01',end = '2012-12-31', startview = 'decade'),
        
        p('2. Select the number of years in the simulation period, i.e., the number of years each trace of data will contain'),
        numericInput('simYrs', 'Simulation Years:',50, min = 1, max = 107, step = 1),
        
        p('3. Select the start date of the trace files. (Should be January 31 of some year.)'),
        dateInput('traceStartYear','Traces Start In:',value = '2017-01-31',
                  startview = "decade"),
        
        p('4. Select the folder to save the trace files in:'),
        shinyFiles::shinyDirButton('fPath','Select the Folder',
                                   'Select the Folder'),
        
        p('Selected folder:'),
        verbatimTextOutput('filepaths')
      
    )
  )
  
  server <- function(input, output, session) {
    volumes <- shinyFiles::getVolumes()
    shinyFiles::shinyDirChoose(input,'fPath',session=session,roots = volumes)
    output$filePath <- renderPrint({shinyFiles::parseFilePaths(volumes, input$fPath)})
    # Listen for 'done' events. When we're finished, we'll
    # insert the current time, and then stop the gadget.
    observeEvent(input$done, {
      createCRSSDNFInputFiles('CoRiverNF',oFolder = shinyFiles::parseDirPath(volumes, input$fPath),
                              startDate = input$traceStartYear, simYrs = input$simYrs,
                              recordToUse = input$nfInputYrs)
      stopApp()
    })
    
  }
  
  # We'll use a pane viwer, and set the minimum height at
  # 300px to ensure we get enough screen space to display the clock.
  viewer <- paneViewer(300)
  runGadget(ui, server, viewer = viewer)
  
}
