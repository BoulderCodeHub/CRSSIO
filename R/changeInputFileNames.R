#' Rename CRSS Input Files
#' 
#' Rename the CRSS natural flow and salt input files.
#' 
#' Because multiple versions of CRSS exist and the inflow locations have had their names changed
#' in recent years, it is necessary to create files with different file names. It might be 
#' easier to copy existing files instead of creating files from the source data, as 
#' \code{\link{createCRSSDNFInputFiles}} does. \code{copyAndChangeNames} assumes the folders
#' are structured for CRSS input, e.g., C:/CRSS/dmi/NFSinput/trace1/NFInput.Files. 
#' 
#' @param iFolder Path to the input folder cotaining trace folders.
#' @param oFolder Path to the ouptut folder containig trace folders.
#' @param nTrace Number of traces to process
#' @param fromNames A vector of the file names found in iFolder/traceN.
#' @param toNames A vector of the file names to create in oFolder/traceN.
#' @return Nothing is returned from function. 
#' @examples
#' # load the common old and new natural flow files names included with the CRSSIO package.
#' \dontrun{
#' iFolder <- 'C:/CRSS/dmi/NFSinputOrig/'
#' oFolder <- 'C:/CRSS/dmi/NFSinputNew/'
#' oldFileNames <- CRSSInputNames(version = 1)
#' newFileNames <- CRSSInputNames(version = 2)
#' copyAndChangeNFFileNames(iFolder, oFolder, 107,oldFileNames, newFileNames)
#' }
#' 
#' @export
copyAndChangeNFFileNames <- function(iFolder, oFolder, nTrace, fromNames, toNames)
{
  
  for(i in 1:nTrace){
    fromPath = paste(iFolder,'trace',i,'/',sep = '')
    toPath = paste(oFolder,'trace',i,'/',sep = '')
    dir.create(toPath)
    for(j in 1:length(fromNames)){
      file.copy(paste(fromPath,fromNames[j],sep = ''),paste(toPath, toNames[j],sep = ''))
    }
  }
} 