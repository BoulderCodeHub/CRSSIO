
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
  # then call ism_get_site_matrix for every node
  lapply(1:29, function(x) ism_get_site_matrix(nfMat[,x], startMonth, nYrs))
}

#' Read in NF Excel file and format it
#' 
#' Read in the NF Excel file, removes whitespace and unnecessary rows and columns.
#'
#' @param iFile Relative or absolute file path to the Excel file
#' @return An xts matrix beginning in January 1906 with 29 columns. 
#' @keywords internal
#' @noRd

read_and_format_nf_excel <- function(iFile)
{
  ymJan1906 <- zoo::as.yearmon("Jan 1906")
  
  nf <- readxl::read_xlsx(iFile, sheet = 'Intervening Natural Flow', skip = 3) %>%
  # going to take a lot of trimming, etc. to get rid of all the labels we don't 
  # need for the flow matrix
    dplyr::rename_at(
      .vars = "Natural Flow And Salt Calc model Object.Slot", 
      .funs = function(x) "date"
    ) %>%
    dplyr::mutate_at("date", .funs = zoo::as.yearmon) %>%
    # get rid of the filler row at top, and the rows containing averages on 
    # bottom
    dplyr::filter_at("date", dplyr::any_vars(!is.na(.))) %>%
    dplyr::filter_at("date", dplyr::any_vars(. >= ymJan1906)) %>%
    dplyr::select(-dplyr::matches("X__1")) %>%
    dplyr::mutate_if(is.character, as.numeric) %>%
    as.data.frame()
 
  # 1. potential update the other column names
  # then convert to xts
  
  if(nrow(nf) %% 12 != 0){
    stop('error in formatting the table resulted in a matrix that is not divisible by 12 months')
  }
  
  Sys.setenv(TZ = 'UTC') # set the system timezone to UTC
  nf <- xts::as.xts(zoo::read.zoo(nf))
  colnames(nf) <- paste0("X", seq(1, ncol(nf)))
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
  stopifnot(dir.exists(folderName))
  
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
  yt <- ism_get_site_matrix(
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

get_dnf_readme_vals <- function(iFile, startYear, endYear, periodToUse)
{
  simYrs <- endYear - startYear + 1
  startDate <- paste0(startYear, "-01-31")
  
  if (iFile == "CoRiverNF") {
    createFrom <- paste0("CoRiverNF (v",
                         utils::packageVersion("CoRiverNF"), ")")
  } else{
    createFrom <- iFile
  }
  
  list(
    intro = "Created From Observed Hydrology with ISM",
    simYrs = simYrs,
    periodToUse = periodToUse,
    startDate = startDate,
    createFrom = createFrom
  )
}

check_nf_oFolder <- function(oFolder, overwriteFiles, calledBy)
{
  if (!dir.exists(oFolder))
    stop(oFolder, " folder does not exist.", "\n",
         "Create the directory before calling ", calledBy, "()")
  
  # check that all folders are empty. If they are empty, then proceed no matter
  # what; if they are not empty, then check the overwrite argument. if overwrite
  # is false the throw error, if overwrite is true, then proceed
  if (length(list.files(oFolder, recursive = TRUE)) != 0 & !overwriteFiles){
    stop("Trace files exist in ", oFolder, "\n",
         "  Choose a different folder, delete the files, or use overwriteFiles = TRUE")
  }
}

check_recordToUse <- function(recordToUse)
{
  if (class(recordToUse) != "yearmon")
    stop("recordToUse must be class 'yearmon'.")
  
  if (length(recordToUse) != 2)
    stop("recordToUse should only contain two entries, or be 'NA'.")
  
  m1 <- as.integer(format(recordToUse[1], "%m"))
  m2 <- as.integer(format(recordToUse[2], "%m"))
  
  if (m1 != 1)
    stop("The first entry to recordToUse should be January of some year.")
  
  if (m2 != 12)
    stop("The second entry to recordToUse should be December of some year.")
  
  y1 <- as.integer(format(recordToUse[1], "%Y"))
  y2 <- as.integer(format(recordToUse[2], "%Y"))
  
  if (y1 < 1906 | y2 < 1906)
    stop("Years in recordToUse should not be before 1906.")
  
  if (y2 < y1)
    stop("The second entry in recordToUse should be after the first entry.")
  
  # if all checks pass, then convert to a string that works with xts matrices
  # and return that
  
  c(paste(y1, m1, sep = "-"), paste(y2, m2, sep = "-"))
}
