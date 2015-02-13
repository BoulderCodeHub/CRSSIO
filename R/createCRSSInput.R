#' Create CRSS Natural Flow Input Files 
#' 
#' Creates the CRSS natural flow input files from the standard excel workbook that is used to publish 
#' the natural flow data.
#' 
#' \code{createCRSSDNFInputFiles} creates individual trace files for the observed
#' natural flow on the Colorado River Basin using the Index Sequential Method 
#' (INSERT REFERENCE). Trace files are formated and saved in a format expected
#' by the Colorado River Simulation System (CRSS). Data is read from the natural
#' flow workbook available at \url{http://www.usbr.gov/lc/region/g4000/NaturalFlow/current.html}.
#' 
#' @param iFile The relative or absolute path to the excel workbook.
#' @param oFolder The location to write out all trace files.
#' @param startDate The start date to be listed in each trace file; should be in 2014-1-31 format
#' @param simYrs The number of years data each file should contain. Should be
#' no longer than the number of years in the input data.
#' @param oFiles A matrix of the file names (input into CRSS). The default uses
#' \code{\link{CRSSNFInputNames}}. This must be specified in the correct order, i.e.,
#' the same order as the nodes in the input Excel file.
#' 
#' @return Nothing is returned by the function, but it writes out many files.
#' @examples
#' # will create 50 years for 107 traces based on the 1906-2012 record
#' creatCRSSDNFInputFiles('NaturalFlows1906-2012_withExtensions_1.8.15.xlsx','NFSinput/','2015-1-31',50)
#' @seealso
#' \code{\link{CRSSNFInputNames}}
createCRSSDNFInputFiles <- function(iFile, oFolder, startDate, simYrs, oFiles = CRSSNFInputNames())
{
	nf <- xlsx::read.xlsx(iFile, sheetName = 'Intervening Natural Flow')
	# going to take a lot of trimming, etc. to get rid of all the labels we don't need for
	# the flow matrix
	
	# trim off extraneous data
  # know the first 7 rows are not needed
	nf <- as.matrix(nf[8:(nrow(nf)),2:31])
  # remove any rows that are NA since there could be one or more at the bottom of the file
	notNaRows <- which(!is.na(nf[,1]))
  nf <- nf[notNaRows,]
  # now remove the bottom row since this is the average for the period
  nf <- nf[1:(nrow(nf)-1),]
  if(nrow(nf)%%12 != 0){
    stop('error in formatting the table resulted in a matrix that is not divisible by 12 months')
  }
  if(simYrs > nrow(nf)/12){
    stop('Error: simYrs is larger than the number of years in the input data.')
  }
	# remove gap column
	nf <- matrix(as.numeric(nf[,c(1:20,22:30)]),ncol = 29, byrow = F)
	
	nT <- nrow(nf)/12
	
	# repeat all data, so can sample the traces that wrap around time
	nf <- rbind(nf, nf)
	
	# number of months in each trace
	nM <- simYrs * 12
	headerInfo = paste('start_date: ',startDate, ' 24:00\nunits: acre-ft/month',sep = '')
	
	# loop over each trace and file
	for(i in 1:nT){
		print(paste('Starting trace',i))
		flush.console()
		fold <- paste(oFolder,'/trace',i,sep = '')
		dir.create(fold)
		
		for(j in 1:length(oFiles)){
			tmp <- nf[(12*i-11):(12*i-12+nM),j]
			# add necessary header info
			tmp <- matrix(tmp, ncol = 1)
			colnames(tmp) = headerInfo
			write.table(tmp, file = paste(fold,'/',oFiles[j],sep = ''),quote = F, row.names = F)
		}
	}
	
}