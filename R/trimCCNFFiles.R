#' Trim Climate Change Natural Flow Files
#' 
#' Trims the Climate Change natural flow input data to start and/or end in a different year.
#' 
#' The climate change natural flows were developed to start in January 1950. Unlike the other natural
#' flow files used in CRSS, the data are associated with specific years, i.e., the index
#' sequential method is not used on the climate change hydrology. Sometimes it is necessary 
#' to trim the data to start, and end in a particular year. The function will return data starting
#' in January of the \code{startYear} and ending in December of \code{endYear}. 
#' 
#' While the function is typically used on climate change natural flow files, it
#' will still work with other natural flow input files.
#' 
#' Assumes folder numbers will always start at 1 and that all files in the folder should be 
#' processed.
#' 
#' @param startYear The desired start year. Should be after 1950 and before 2099.
#' @param endYear The desired end year. Should be after 1950 and before 2099.
#' @param iFolder The path to the trace files, e.g., '/dmi/VIC/'.
#' @param nTraces The number of traces to process. The default is 112, which is the number of traces
#' in the CMIP3 climate change hydrology.
#' @return The number of files that were processed
#' 
#' @examples 
#' # Trim all 112 traces found in 'CRSS/dmi/VIC' to start in Jan-2017 and end in Dec-2019
#' \dontrun{
#' trimCCNFFiles(2017,2019,'CRSS/dmi/VIC/')
#' }
#' 
#' @export
trimCCNFFiles <- function(startYear, endYear, iFolder, nTraces = 112)
{
  if(endYear < startYear){
    stop("In trimCCNFFiles, endYear cannot be before startYear.")
  }
  
  # create list of all folder names to process.
  allFolders <- paste0(iFolder,'/trace',1:nTraces)
  # call trimFilesInFolder for all folders
  xx <- sapply(allFolders, trimFilesInFolder, startYear, endYear)
  sum(xx)
}

#' @keywords internal
trimFilesInFolder <- function(folder, startYear, endYear)
{
  allFiles <- paste0(folder,'/',list.files(folder))
  message(paste('Processing:',folder))
  # call trimSindleFile for every file found in the folder
  xx <- sapply(allFiles, trimSingleFile, startYear, endYear)
  xx
}

# will trim the data in a single file and write out the new file
# ff is the file to trim
#' @keywords internal
#' @importFrom utils read.table
#' @importFrom utils write.table
trimSingleFile <- function(ff, startYear, endYear)
{
  # read in the flow or salinity data
  nf = as.matrix(read.table(ff, sep = '\t', skip = 2))
  # read in the header info and maintain units; necessary so the code works for flow and salinity files
  headerInfo = scan(ff, what = 'char', nlines = 2, sep = '\t', quiet = T)
  dataStartYear <- as.numeric(strsplit(strsplit(headerInfo[1],' ',fixed = T)[[1]][2], 
                                       '-',fixed = T)[[1]][1])
  
  # convert flows to zoo object to make sumsetting to trimmed data easier
  nf.months <- zoo::as.yearmon(dataStartYear + seq(0,length(nf)-1)/12)
  nfZ <- zoo::zoo(nf,nf.months)
  
  # create subset of months to trim data
  trimMonths <- zoo::as.yearmon(startYear + seq(0,(endYear-startYear+1)*12-1)/12)
  nfZ <- nfZ[trimMonths]
  nf <- as.matrix(nfZ)
  
  # change header info to have the new start date
  startInfo <- strsplit(headerInfo[1],' ',fixed = T)[[1]]
  startDate <- paste0(startYear,'-1-31')
  headerInfo[1] <- paste(startInfo[1],startDate,startInfo[3])
  headerInfo <- paste0(headerInfo[1],'\n', headerInfo[2])
  
  colnames(nf) <- headerInfo
  # writes out to the same folder it reads in from
  write.table(nf, ff,quote = F, row.names = F)
  1
}
