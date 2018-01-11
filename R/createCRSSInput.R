#' Create CRSS Natural Flow Input Files 
#' 
#' Creates the CRSS natural flow input files used by the Colorado River Simulation
#' System (CRSS)
#' 
#' `crssi_create_dnf_files()` and `createCRSSDNFInputFiles()` create individual 
#' trace files for the observed natural flow on the Colorado River Basin using the 
#' \href{http://onlinelibrary.wiley.com/doi/10.1111/j.1752-1688.1997.tb03557.x/abstract}{Index Sequential Method}. 
#' Trace files are formated and saved in a format expected
#' by the Colorado River Simulation System (CRSS). Data is read from the natural
#' flow workbook available at \url{http://www.usbr.gov/lc/region/g4000/NaturalFlow/current.html} or
#' from the `CoRiverNF` data package. It is much faster to use the data package
#' rather than the Excel workbook. The data package is created from the Excel
#' file, so the files created are identical for each of the two data sets.
#' 
#' In addition to creating the natural flow input files, data for two informative 
#' slots are created. These slots include the trace number and the supply scenario
#' number. The trace numbers are intuitive, i.e., they are integers from 1 to N
#' where N is the trace number. This information is saved in the slot name that
#' is set by the "crssio.traceNumberSlot" option. The scenario number provides
#' the user with a numeric representation of which supply scenario is used. The
#' observed historical natural flows (used by this function) are supply scenario 1. 
#' For this supply scenario, the decimals of the supply scenario number represent
#' the start and end year that the ISM method are applied to. For example, if 
#' you set \code{recordToUse} to \code{c('1988-1','2012-12')}, the decimal portion
#' will be 19882012, where the first 4 numbers represent the start year and
#' the second four numbers represent the end year. The supply scenario slot will 
#' be set to 1.19882012 in this example. This tells the user of CRSS that the 
#' supply scenario is the observed historical natural flows with the ISM method
#' applied to the 1988-2012 data. The supply scenario slot name is set
#' by the "crssio.supplyScenarioSlot" option.
#' 
#' The hydrologyIncrement data that sets the random number generator for
#' each year and trace is created for each trace folder. The slot name that is created
#' for the hydrologyIncrement is set by the "crssio.hydroIncrement" option.
#' 
#' Beginning in CRSS v2.6, input data for the Sacramento year type 
#' index are necessary. The historical Sacramento year type data (available at
#' \url{http://cdec.water.ca.gov/cgi-progs/iodir/WSIHIST}) are resampled using
#' ISM with the same years as the historical natural flows. This function also
#' creates an input file for each trace that includes the Sacramento year type
#' index. This file name is controlled by the \code{crssio.sacYTSlot} option and
#' defaults to "MWD ICS.SacWYType"
#' 
#' `overwriteFiles` allows the user to control whether existing files within the
#' trace folders should be overwritten (default is they are not). 
#' 
#' @param iFile Either the string "CoRiverNF", or the relative or absolute path 
#' to the excel workbook. When "CoRiverNF" is used, the data from the `CoRiverNF`
#' data package is used. Otherwise, it should be a valid path to the natural 
#' flow Excel workbook. 
#' @param oFolder Path to the top level directory where the trace folders and
#'   input files will be created. This folder should exist before using this
#'   function.
#' @param startYear The year to start the trace files in. Data will be trimmed 
#'   to start in this year.
#' @param startDate The start date to be listed in each trace file; should be in 
#'   2014-1-31 format
#' @param endYear The final year of data the trace files will contain.
#' @param simYrs The number of years of data each file should contain. Should be
#' no longer than the number of years in the input data.
#' @param oFiles A matrix of the file names (input into CRSS). The default uses
#'   \code{\link{CRSSNFInputNames}}. This must be specified in the correct 
#'   order, i.e., the same order as the nodes in the input Excel file.
#' @param recordToUse The start and end dates of the natural flow record to 
#'   perform ISM, if using something besides the full record. If it is `NA`, the 
#'   full record will be used. Otherwise, it should be a vector of length 2, 
#'   where the first entry is the start date and the second entry is the end 
#'   date. The vector should be of type \code{\link[zoo]{yearmon}}, or something 
#'   that will sucessfully convert to a \code{\link[zoo]{yearmon}} object.
#' @param overwriteFiles A boolean that determines whether or not the function
#'   should overwrite existing files. See 'Details'.
#' 
#' @return Nothing is returned by the function, but it writes out many files.
#' @examples
#' 
#' dir.create('tmp')
#' # create 107 traces of 107 years using the CoRiverNF R data package
#' createCRSSDNFInputFiles("CoRiverNF", 'tmp','2017-1-31',107)
#' # will create 20 years for 25 traces based on the 1988-2012 record:
#' createCRSSDNFInputFiles("CoRiverNF", 'tmp','2017-1-31',20, 
#'   CRSSNFInputNames(version=2),recordToUse=c('1988-1','2012-12'))
#' 
#' \dontrun{
#' # path to excel file
#' iFile <- 'user/docs/NaturalFlows1906-2012_withExtensions_1.8.15.xlsx'
#' # will create 50 years for 107 traces based on the full (1906-2012) record:
#' createCRSSDNFInputFiles(iFile,'NFSinput/','2015-1-31',50)
#' # will create 20 years for 25 traces based on the 1988-2012 record:
#' createCRSSDNFInputFiles(iFile,'scratch/','2016-1-31', 20, recordToUse = c('1988-1-31','2012-12-31'))
#' }
#' @seealso
#' \code{\link{CRSSNFInputNames}}
#' 
#' @export
crssi_create_dnf_files <- function(iFile, 
                                   oFolder, 
                                   startYear, 
                                   endYear, 
                                   oFiles = CRSSNFInputNames(),
                                   recordToUse = NA,
                                   overwriteFiles = FALSE)
{
  check_nf_oFolder(oFolder, overwriteFiles, "crssi_create_dnf_files")
  
  if(iFile == 'CoRiverNF'){
    # use the data in CoRiverNF::monthlyInt
    nf <- CoRiverNF::monthlyInt
    if(!anyNA(recordToUse)){
      # if not NA, then trim data, otherwise use full data
      nf <- nf[paste(recordToUse[1], recordToUse[2],sep = '/')]
    } else{
      nf <- nf['1906-01/'] # trim off OND 1905
    }
  } else{
    # use the data in the Excel workbook, if it exists.
    if(!file.exists(iFile)){
      stop('iFile does not exist')
    }
  
    nf <- readAndFormatNFExcel(iFile)

    if(!anyNA(recordToUse)){
      # trim data
      nf <- nf[paste(recordToUse[1], recordToUse[2],sep = '/')]
    }
  }
  
  # get the years used before changing nf
  if (!anyNA(recordToUse)) {
    y1 <- format(zoo::as.yearmon(recordToUse[1]), "%Y")
    y2 <- format(zoo::as.yearmon(recordToUse[2]), "%Y")
    periodToUse <- paste0(y1, '-', y2)
    # this only deals with historical observed NF, so that is supply scenario 
    # 1.xxxxxxxx, where the .xxxxxxxx are the beginning and ending years used
    # for ISM
    supplyScenario <- as.numeric(paste0(1,'.',y1,y2))
  } else {
    # uses the full record, so it's 1906 - some year. figure out some year
    y1 <- 1906
    y2 <- format(zoo::index(nf)[nrow(nf)], "%Y")
    periodToUse <- paste0(y1, '-', y2)
    supplyScenario <- as.numeric(paste0('1.1906',y2))
  }
  
  startDate <- paste0(startYear, "-01-31")
  simYrs <- endYear - startYear + 1
  
  nf <- getAllISMMatrices(nf, startDate, simYrs)
  
  nT <- ncol(nf[[1]]) # number of traces
  
	# number of months in each trace
	nM <- simYrs * 12
	headerInfo <- get_trace_file_header(startYear)
	
	# create all directories
	for(i in 1:nT){
	  fold <- paste(oFolder,'/trace',i,sep = '')
	  dir.create(fold)
	}

	# for each node, write out all of the trace files
	lapply(
	  1:29, 
	  function(x) writeNFFilesByNode(nf[[x]], oFiles[x], oFolder, headerInfo)
	)

	# for each trace, write out all of the trace and supply scenario number files
	message("Beginning to write node: supply scenario and trace number")
	lapply(1:nT, function(x) writeTraceSupplyNumbers(x, supplyScenario, oFolder))
	
	# for each trace, write out the hydrology increment slot
	message('Beginning to write node: hydrologyIncrement')
	lapply(1:nT, function(x) writeHydroIncrement(x, simYrs, startDate, oFolder))
	
	# for each trace, write out the sacramento year type file
	# convert from january to december
	eoyDate <- paste0(format(zoo::as.yearmon(startDate), "%Y"), "-12-31")
	ytData <- getYTISMData(eoyDate, simYrs, y1, y2)
	message("Beginning to write node: Sacramento year type")
	lapply(1:nT, function(x) writeSacYT(x, ytData, eoyDate, oFolder))
	
	# create the README
	write_nf_readme(
	  get_dnf_readme_vals(iFile, startYear, endYear, recordToUse), 
	  oFolder = oFolder
	)
}

#' @export
#' @rdname crssi_create_dnf_files
createCRSSDNFInputFiles <- function(iFile, 
                                    oFolder, 
                                    startDate, 
                                    simYrs, 
                                    oFiles = CRSSNFInputNames(),
                                    recordToUse = NA)
{
  .Deprecated("crssi_create_dnf_files")
  
  startYear <- as.numeric(strsplit(startDate, "-")[[1]][1])
  endYear <- startYear + simYrs - 1
  
  crssi_create_dnf_files(iFile, oFolder, startYear, endYear, oFiles, recordToUse)
}
