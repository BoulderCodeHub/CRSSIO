
# parse out the header info from the data;
# x can be as short as 1, and has an unknown number of header key words
# return a list with keywords and data

seperateHeaderFromData <- function(x) {
  iData <- 1
  # suprress warnings b/c we expect NAs for some time
  while(is.na(suppressWarnings(as.numeric(x[iData])))) {
    iData <- iData + 1
  }
  
  # iData is the index that the data actually start
  if(iData == 1) {
    atts <- NA
  } else {
    atts <- x[1:(iData-1)]
  }
  
  theData <- as.numeric(x[iData:length(x)])
  
  # parse the attributes
  atts <- simplify2array(strsplit(atts, ": ", fixed = TRUE))
  
  return(list("data" = theData, "atts" = atts))
}

#' changeStartDate
#' 
#' changes the start date in all files in each trace folder
#' 
#' Used to change the start date of all natural flow input files. Will change them for all 
#' files in a folder.
#' 
#' @param nTrace The number of traces to process.
#' @param folder A string with either a relative or absolute path that contains the trace 
#' folders, e.g., 'C:/CRSS/dmi/DNFSinput'
#' @param startDate string of new starting date. Should be in 2012-1-31 format. 
#' 
#' @export
changeStartDate <- function(nTrace, folder, startDate)
{
	timeInfo = paste(startDate,' 24:00', sep = '')
	
	# if the new start date is before the old start date, issue a warning
	# but only issue the warning once per call of this function, b/c issuing 
	# it for every single file and trace would be extremely annoying
	issueWarning <- FALSE
	
	for(i in 1:nTrace){
		message(paste('Starting trace:',i,'of',nTrace))
	
		currFold = file.path(folder,paste0('trace',i))
		# get list of all files contained in the trace folder
		currFiles = list.files(currFold)
		for(j in 1:length(currFiles)){
		  # read in the entire file
			tmpData = scan(
			  file.path(currFold,currFiles[j]), 
			  sep = '\t', 
			  what = character(),
			  quiet = TRUE
			)
			# find header info and data; tmpData is now a list
			tmpData <- seperateHeaderFromData(tmpData)
			
			# find the start_date keyword; 
			startKeyword <- match("start_date", tmpData$atts[1,])
			
			#if it doesn't exist, then can just skip this file
			if(!is.na(startKeyword)) {
			  issueWarning <- issueWarning | as.POSIXct(timeInfo) < as.POSIXct(tmpData$atts[2,startKeyword])
			  
			  # replace existing start_date
			  tmpData$atts[2,startKeyword] <- timeInfo
			  
			  # reconstruct the header info
			  atts <- paste0(tmpData$atts[1,], ": ", tmpData$atts[2,])
			  tmpData <- matrix(c(atts, tmpData$data), ncol = 1)
			  # writes out to the same folder it reads in from
			  utils::write.table(
			    tmpData, 
			    file = file.path(currFold,currFiles[j]),
			    quote = FALSE, 
			    row.names = FALSE,
			    col.names = FALSE
			  )
			}
		}
	}
	
	if(issueWarning) 
	  warning("The new start date is before the original start date.\n",
	          "  This may result in not a long enough time series in the new run.\n",
	          "  Consider using createCRSSDNFInputFiles() instead.")
	
	invisible(nTrace)
}

#' Change Start Date of Evap Files
#' 
#' Changes the start date in all files in each CRSS_DIR/dmi/evap/trace folder.
#' 
#' For evaporation files used in CRSS that are in CRSS_DIR/dmi/Evap, this function
#' will change their start date, and add 0s on if necessary to extend the evaporation
#' rates. The files specific to Powell are skipped.
#' 
#' @param startDate New start date. Should be in 2012-1-31 format.
#' @param nTrace The number of traces to process.
#' @param folder A string with either a relative or absolute path that contains 
#' the trace folders, e.g., C:/CRSS/dmi/DNFSinput
#' @param NZeros is the number of zeros to add to the data that is read in
#' 
#' @export
changeStartDateForEvapAndAddZeros <- function(nTrace, folder, startDate, NZeros)
{
	timeInfo = paste('start_date: ',startDate,' 24:00\n', sep = '')
	for(i in 1:nTrace){
		message(paste('Starting trace:',i,'of',nTrace))
		currFold = paste(folder,'/trace',i,'/',sep = '')
		# get list of all files contained in the trace folder
		currFiles = list.files(currFold)
		for(j in 1:length(currFiles)){
			if(!(currFiles[j] %in% c('Powell.Average_Air_Temperature','Powell.Average_Precipitation',
				'Powell.Gross_Evaporation_Coefficient','Powell.River_Evaporation_Coefficient'))){
				tmpData = as.matrix(utils::read.table(paste(currFold,currFiles[j],sep = ''), sep = '\t', skip = 2))
				# read in the header info and maintain units; necessary so the code works for flow and salinity files
				headerInfo = scan(paste(currFold,currFiles[j],sep = ''), what = 'char', nlines = 2, sep = '\t')
				headerInfo = paste(timeInfo, headerInfo[2], sep = '')
				tmpData <- matrix(c(tmpData, rep(0, NZeros)),ncol = 1)

				colnames(tmpData) = headerInfo
				# writes out to the same folder it reads in from
				utils::write.table(tmpData, file = paste(currFold,currFiles[j],sep = ''),quote = F, row.names = F)
			}
		}
	}
}

