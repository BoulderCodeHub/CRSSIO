
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

#' Change the start dates of CRSS input trace files
#' 
#' `crssi_change_nf_start_date()` changes the start date in all files in each 
#' trace folder. Used to change the start date of all natural flow input files. 
#' This Will change the start date for all files in a folder (flow, salinity, 
#' other).
#' 
#' @param nTrace The number of traces to process.
#' 
#' @param folder A string with either a relative or absolute path that contains 
#'   the trace folders, e.g., 'C:/CRSS/dmi/DNFSinput'.
#'   
#' @param start_year The new start_year for the trace files. Should be a single
#'   numeric value.
#'   
#' @param startDate Deprecated. 
#' 
#' @export
crssi_change_nf_start_date <- function(nTrace, folder, start_year, startDate)
{
  assert_that(
	  length(nTrace) == 1 && is.numeric(nTrace),
	  msg = "`nTrace` should be a single numeric value."
	 )
  
  assert_that(length(folder) == 1, msg = "`folder` should be a single value.")
  assert_that(dir.exists(folder), msg = "`folder` does not exist.")
  
  assert_that(
    length(start_year) == 1 && is.numeric(start_year) && start_year > 0 && 
      start_year <= 9999,
    msg = "`start_year` should be a single numeric value between 0 and 9999."
  )
  
  assert_that(
    missing(startDate),
    msg = "`startDate` is deprecated; please use `start_year` instead."
  )
  
  # if the new start date is before the old start date, issue a warning
	# but only issue the warning once per call of this function, b/c issuing 
	# it for every single file and trace would be extremely annoying
	issueWarning <- FALSE
	
	message(paste('Processing', nTrace, "traces:"))
	pb <- utils::txtProgressBar(min = 0, max = nTrace, style = 3)
	
	for (i in seq_len(nTrace)){
		currFold <- file.path(folder,paste0('trace',i))
		# get list of all files contained in the trace folder
		currFiles <- list.files(currFold)
		for (j in seq_len(length(currFiles))){
		  # read in the entire file
			tmpData <- scan(
			  file.path(currFold,currFiles[j]), 
			  sep = '\t', 
			  what = character(),
			  quiet = TRUE
			)
			# find header info and data; tmpData is now a list
			tmpData <- seperateHeaderFromData(tmpData)
			
			# find the start_date keyword; 
			startKeyword <- match("start_date", tmpData$atts[1,])
			
			# if it doesn't exist, then can just skip this file
			if (!is.na(startKeyword)) {
			  # parse the current start_date
			  timeInfo <- simplify2array(strsplit(tmpData$atts[2, startKeyword], "-"))
			  # replace the year with the new year
			  timeInfo[1,1] <- as.character(start_year)
			  # and then recreate the string
			  timeInfo <- paste(timeInfo, collapse = "-")
			  
			  issueWarning <- issueWarning | 
			    (as.POSIXct(timeInfo) < as.POSIXct(tmpData$atts[2, startKeyword]))
			  
			  # replace existing start_date
			  tmpData$atts[2, startKeyword] <- timeInfo
			  
			  # reconstruct the header info
			  atts <- paste0(tmpData$atts[1,], ": ", tmpData$atts[2,])
			  tmpData <- matrix(c(atts, tmpData$data), ncol = 1)
			  # writes out to the same folder it reads in from
			  utils::write.table(
			    tmpData, 
			    file = file.path(currFold, currFiles[j]),
			    quote = FALSE, 
			    row.names = FALSE,
			    col.names = FALSE
			  )
			}
		}
		
		utils::setTxtProgressBar(pb, i)
	}
	
	if(issueWarning) 
	  warning(
	    "The new start date is before the original start date.\n",
	    "  This may result in not a long enough time series in the new run.\n",
	    "  Consider using crssi_create_dnf_files() instead."
	  )
	
	update_readme(folder, start_year)
	
	invisible(nTrace)
}

update_readme <- function(folder, start_year)
{
  rfile <- file.path(folder, "README.txt")
  
  crssio_version <- paste("CRSSIO version:", utils::packageVersion('CRSSIO'))
  dateCreate <- paste('date modified:', Sys.Date())
  createBy <- paste('modified by:', Sys.info()[["user"]])
  startYear <- paste('new start year:', start_year)

  if (file.exists(rfile)) {
    # README exists, so append to it
    txt <- rbind(
      "----------",
      "The trace files had their start_dates changed by `crssi_change_nf_start_date()`.",
      dateCreate,
      createBy,
      startYear,
      crssio_version
    )
    
    write(txt, file = rfile, append = TRUE)
  } else {
    # README doesn't exist, so create it
    txt <- rbind(
      "Natural Flow Data",
      "These file have an unknown creation source.",
      "----------",
      "The trace files had their start_dates changed by `crssi_change_nf_start_date()`.",
      dateCreate,
      createBy,
      startYear,
      crssio_version
    )
    
    utils::write.table(
      txt, 
      file = rfile, 
      quote = FALSE, 
      row.names = FALSE, 
      col.names = FALSE
    )
  }
  invisible(txt)
}

#' Change Start Date of Evap Files
#' 
#' Changes the start date in all files in each CRSS_DIR/dmi/evap/trace folder.
#' 
#' For evaporation files used in CRSS that are in CRSS_DIR/dmi/Evap, this 
#' function will change their start date, and add 0s on if necessary to extend 
#' the evaporation rates. The files specific to Powell are skipped.
#' 
#' @param startDate New start date. Should be in 2012-1-31 format.
#' @param nTrace The number of traces to process.
#' @param folder A string with either a relative or absolute path that contains 
#' the trace folders, e.g., C:/CRSS/dmi/DNFSinput
#' @param NZeros is the number of zeros to add to the data that is read in
#' 
#' @export
crssi_change_evap_files <- function(nTrace, folder, startDate, NZeros)
{
	timeInfo <- paste('start_date: ',startDate,' 24:00\n', sep = '')
  powellFiles <- c(
	  'Powell.Average_Air_Temperature',
	  'Powell.Average_Precipitation',
	  'Powell.Gross_Evaporation_Coefficient',
	  'Powell.River_Evaporation_Coefficient'
	)
	
  message(paste('Processing', nTrace, "traces:"))
  pb <- utils::txtProgressBar(min = 0, max = nTrace, style = 3)
  
	for (i in seq_len(nTrace)){
		
		currFold <- paste(folder,'/trace',i,'/',sep = '')
		# get list of all files contained in the trace folder
		currFiles <- list.files(currFold)
		for (j in seq_len(length(currFiles))){
			if (!(currFiles[j] %in% powellFiles)){
				tmpData <- as.matrix(utils::read.table(
				  paste(currFold,currFiles[j],sep = ''),
				  sep = '\t',
				  skip = 2
				))
				# read in the header info and maintain units; 
				# necessary so the code works for flow and salinity files
				headerInfo <- scan(
				  paste(currFold,currFiles[j],sep = ''),
				  what = 'char',
				  nlines = 2,
				  sep = '\t'
				)
				headerInfo <- paste(timeInfo, headerInfo[2], sep = '')
				tmpData <- matrix(c(tmpData, rep(0, NZeros)),ncol = 1)

				colnames(tmpData) <- headerInfo
				# writes out to the same folder it reads in from
				utils::write.table(
				  tmpData,
				  file = paste(currFold,currFiles[j],sep = ''),
				  quote = FALSE,
				  row.names = FALSE
				)
			}
		}
		
		utils::setTxtProgressBar(pb, i)
	}
}
