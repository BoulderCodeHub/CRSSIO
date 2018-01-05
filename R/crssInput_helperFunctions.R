
#' returns all ISM matrices for all nodes
#' 
#' Given a matrix of natural flow data, applies the ISM method to all nodes
#' 
#' @param nfMat Matrix of natural flow data
#' @param startMonth The startMonth of the return matrix. Should be able to be
#' cast to a zoo::yearmon
#' @param nYrs The number of years to create the data for. 
#' 
#' @return list of matrices. Each node is one matrix entry into the list
#' @keywords internal
#' @noRd

getAllISMMatrices <- function(nfMat, startMonth, nYrs)
{
  # make sure matrix is correct dimension
  if(ncol(nfMat) != 29)
    stop('nfMat does not contain 29 columns (natural flow nodes).')
  # then call createISMMatrix for every node
  lapply(1:29, function(x) createISMMatrix(nfMat[,x], startMonth, nYrs))
}

#' Read in NF Excel file and format it
#' 
#' Read in the NF Excel file, removes whitespace and unnecessary rows and columns.
#'
#' @param iFile Relative or absolute file path to the Excel file
#' @return An xts matrix beginning in January 1906 with 29 columns. 
#' @keywords internal
#' @noRd
#' 
readAndFormatNFExcel <- function(iFile)
{
  message("Starting to read in natural flow Excel file.", "\n",
          "Please be patient this may take several minutes.")

  nf <- xlsx::read.xlsx(iFile, sheetName = 'Intervening Natural Flow')
  # going to take a lot of trimming, etc. to get rid of all the labels we don't 
  # need for the flow matrix
  message('Finished reading in natural flow Excel file.')

  # trim off extraneous data
  # know the first 7 rows are not needed
  nf <- as.matrix(nf[8:(nrow(nf)),2:31]) 
  # remove any rows that are NA since there could be one or more at the 
  # bottom of the file
  notNaRows <- which(!is.na(nf[,1]))
  nf <- nf[notNaRows,]
  # now remove the bottom row since this is the average for the period
  nf <- nf[1:(nrow(nf)-1),]
  if(nrow(nf) %% 12 != 0){
    stop('error in formatting the table resulted in a matrix that is not divisible by 12 months')
  }

  # remove gap column
  nf <- matrix(as.numeric(nf[,c(1:20,22:30)]),ncol = 29, byrow = FALSE)
  Sys.setenv(TZ = 'UTC') # set the system timezone to UTC
  nf.YearMon <- zoo::as.yearmon('1906-01-31') + seq(0,nrow(nf)-1)/12
  nf <- xts::as.xts(zoo::read.zoo(data.frame(nf.YearMon,nf)))
  nf
}

#' write out a single trace of data
#' @keywords internal
#' @noRd
writeSingleFile <- function(xData, fPath, headerInfo)
{
  colnames(xData) <- headerInfo
  utils::write.table(xData, file = fPath, quote = FALSE, row.names = FALSE)
}

#' Write out all of the trace files for a given node
#' 
#' @param nfXts Natural flow data as a matrix for a single node. The number of 
#' columns is the number of traces that will be written out to 
#' oFolder/trace[n]/oFile
#' @param oFile File name that each file will be saved as
#' @param oFolder Folder location to write files to
#' @keywords internal
#' @noRd
writeNFFilesByNode <- function(nfXts, oFile, oFolder, headerInfo)
{
  message('Beginning to write node: ',oFile)

  lapply(
    seq_len(ncol(nfXts)), 
    function(x) writeSingleFile(
      nfXts[,x], 
      file.path(oFolder,paste0('trace',x),oFile),
      headerInfo
    )
  )
}

#' write out the trace number and supply scenario number, to a given trace 
#' folder folderPath should be the top level folder, e.g., dmi/NFSinput
#' @keywords internal
#' @noRd

writeTraceSupplyNumbers <- function(traceNum, supplyScenNum, folderPath)
{
  # traceNum will output as follows:
  # units: NONE
  # 1
  # and supplyScenNum will output:
  # units: NONE
  # 1.19882012
  
  traceText <- matrix(c('units: NONE', traceNum),ncol = 1)
  supplyText <- matrix(c('units: NONE', supplyScenNum), ncol = 1)
  folderName <- file.path(folderPath, paste0('trace', traceNum))
  
  if(!dir.exists(folderName))
    stop("folderName does not exist", "\n", 
         "writeTraceSupplyNumbers() expects the directory to already exist")
  
  utils::write.table(
    traceText, 
    file = file.path(folderName, getOption('crssio.traceNumberSlot')),
    quote = FALSE, 
    row.names = FALSE, 
    col.names = FALSE
  )
  utils::write.table(
    supplyText, 
    file = file.path(folderName, getOption('crssio.supplyScenarioSlot')),
    quote = FALSE, 
    row.names = FALSE, 
    col.names = FALSE
  )
  
}

#' Write the hydrology increment file based on the trace number
#'
#' `writeHydroIncrement()` takes in the trace number as an argument and uses that
#' to create the hydrology increment data. Then the hydrology increment data
#' is saved in the specified folder. 
#' 
#' @keywords internal
#' @noRd

writeHydroIncrement <- function(traceNum, nYrs, startDate, folderPath){
  
  folderName <- file.path(folderPath, paste0('trace', traceNum))
  if(!dir.exists(folderName))
    stop("folderName does not exist", "\n", 
         "writeHydroIncrement() expects the directory to already exist")
  
  # the hydrology increment data starts with the trace number, and increments
  # by one every year until the end of the simulation
  # it is monthly data, so each index repeats 12 times
  tt <- as.vector(t(matrix(
    rep(traceNum:(nYrs + traceNum - 1), 12), 
    byrow = FALSE, 
    ncol = 12
  )))
  
  startInfo <- paste('start_date:', startDate, '24:00')
  tt <- matrix(c(startInfo, 'units: NONE', as.character(tt)), ncol = 1)
  utils::write.table(
    tt, 
    quote = FALSE, 
    row.names = FALSE, 
    col.names = FALSE,
    file = file.path(folderName, getOption('crssio.hydroIncrement'))
  )
}

#' Write the Sacramento Year Type Data
#' 
#' \code{writeSacYT} writes out the Sacramento year type data for a single 
#' trace, given a matrix of data for all traces.
#' 
#' @param traceNum The trace of data to save
#' @param ytData A matrix of all traces of data
#' @param startDate The start date of the trace file as a character. Should be
#' in yyyy-mm-dd format, which is what RiverWare expects.
#' @param folderPath The folder path to the directory containing the 
#' trace folders.
#' 
#' @keywords internal
#' @noRd

writeSacYT <- function(traceNum, ytData, startDate, folderPath)
{
  folderName <- file.path(folderPath, paste0('trace', traceNum))
  tt <- matrix(c(
    paste("start_date:", startDate, "24:00"), 
    "units: NONE", 
    as.character(ytData[,traceNum])),
    ncol = 1)
  utils::write.table(
    tt, 
    quote = FALSE, 
    row.names = FALSE, 
    col.names = FALSE, 
    file = file.path(folderName, getOption("crssio.sacYTSlot"))
  )
}

#' Create the Sacramento Year Type ISM Data
#' 
#' \code{getYTISMData} creates ISM matrix for the Sacramento Year Type data, 
#' using the specified historical period.
#' 
#' @param startDate The start date for the ISM matrix. Should be able to be 
#' converted to a \code{yearmon} class.
#' @param simYrs The number of years to create the ISM matrix for.
#' @param y1 The start year of the historical record to use.
#' @param y2 The ned year of the historical record to use.
#' 
#' @keywords internal
#' @noRd

getYTISMData <- function(startDate, simYrs, y1, y2)
{
  yt <- createISMMatrix(
    sacYT[paste0(y1, "/", y2)], 
    startMonth = zoo::as.yearmon(startDate),
    nYrs = simYrs, 
    monthly = FALSE
  )
  yt
}

#' Write the natural flow README.txt file
#' 
#' write_nf_readme() creates the natural flow README.txt file that contains 
#' metadata associated with when/how the natural flow files were created.
#' 
#' @param vals A named list that contains the following names: intro, simYrs,
#'   periodToUse, startDate, and createFrom.
#' @param oFolder The top level directory for the dmi trace folders.
#' 
#' @return Writes the README.txt file to `oFolder` and invisibly returns the 
#' text in README.txt
#' 
#' @examples 
#' vals <- list(
#'   intro = "Created From Observed Hydrology with ISM",
#'   simYrs = 20,
#'   periodToUse = "1988-2015",
#'   startDate = "2017-1-31",
#'   createFrom = "CoRiverNF (v0.3.0.9000)"
#' )
#' write_nf_readme(vals, ".")
#' 
#' @keywords internal
#' @noRd

write_nf_readme <- function(vals, oFolder)
{
  expNames <- c("intro", "simYrs", "periodToUse", "startDate", "createFrom")
  stopifnot(expNames %in% names(vals), dir.exists(oFolder))
  
  # data for writing out the README file
  intro <- paste0(vals$intro, " using CRSSIO (v", 
                  utils::packageVersion('CRSSIO'),') package')
  dateCreate <- paste('date created:', Sys.Date())
  createBy <- paste('created by:', Sys.info()[["user"]])
  periodToUse <- paste("period used:", vals$periodToUse)
  traceLength <- paste('trace length:', vals$simYrs, 'years')
  startYear <- paste('original start date:', vals$startDate)
  createFrom <- paste("created from:", vals$createFrom)
  
  oText <- paste('Natural Flow Data', intro, '----------', dateCreate, 
                 createBy, periodToUse, traceLength, startYear, createFrom, 
                 sep = '\n')
  
  # write out the README in the top level folder
  utils::write.table(
    oText, 
    file = file.path(oFolder, 'README.txt'), 
    quote = FALSE, 
    row.names = FALSE, 
    col.names = FALSE
  )
  invisible(oText)
}
