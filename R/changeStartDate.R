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
#' @importFrom utils write.table
#' @importFrom utils read.table
#' @export
changeStartDate <- function(nTrace, folder, startDate)
{
	timeInfo = paste('start_date: ',startDate,' 24:00\n', sep = '')
	for(i in 1:nTrace){
		message(paste('Starting trace:',i,'of',nTrace))
	
		currFold = paste(folder,'/trace',i,'/',sep = '')
		# get list of all files contained in the trace folder
		currFiles = list.files(currFold)
		for(j in 1:length(currFiles)){
			tmpData = as.matrix(read.table(paste0(currFold,currFiles[j]), sep = '\t', skip = 2))
			# read in the header info and maintain units; necessary so the code works for flow and salinity files
			headerInfo = scan(paste0(currFold,currFiles[j]), what = 'char', nlines = 2, sep = '\t')
			headerInfo = paste0(timeInfo, headerInfo[2])
			colnames(tmpData) = headerInfo
			# writes out to the same folder it reads in from
			write.table(tmpData, file = paste(currFold,currFiles[j],sep = ''),quote = F, row.names = F)
		}
	}
}

# ---------------------------------------------------------
# THE BELOW CODE IS SPECIFIC TO DATA IN THE EVAP FOLDER
# ---------------------------------------------------------

# changes the start date in all files in each trace folder
# startDate should be in 2012-1-31 format
# nTrace is the number of traces to process
# folder is a string with either a relative or absolute path that contains the trace folders, e.g., C:/CRSS/dmi/DNFSinput
# startDate is a string containing the desired start date in the files; should be in 2012-1-31 format
# NZeros is the number of zeros to add to the data that is read in
#' @keywords internal
#' @importFrom utils read.table
#' @importFrom utils write.table
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
				tmpData = as.matrix(read.table(paste(currFold,currFiles[j],sep = ''), sep = '\t', skip = 2))
				# read in the header info and maintain units; necessary so the code works for flow and salinity files
				headerInfo = scan(paste(currFold,currFiles[j],sep = ''), what = 'char', nlines = 2, sep = '\t')
				headerInfo = paste(timeInfo, headerInfo[2], sep = '')
				tmpData <- matrix(c(tmpData, rep(0, NZeros)),ncol = 1)

				colnames(tmpData) = headerInfo
				# writes out to the same folder it reads in from
				write.table(tmpData, file = paste(currFold,currFiles[j],sep = ''),quote = F, row.names = F)
			}
		}
	}
}

