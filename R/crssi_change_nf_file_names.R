#' Rename CRSS Input Files
#' 
#' Rename the CRSS natural flow or salt input files.
#' 
#' Because multiple versions of CRSS exist and the inflow locations have had 
#' their names changed in recent years, it is necessary to create files with 
#' different file names. It might be easier to copy existing files instead of 
#' creating files from the source data, as [crssi_create_dnf_files()] does. 
#' `crssi_change_nf_file_names()` assumes the folders
#' are structured for CRSS input, e.g., C:/CRSS/dmi/NFSinput/trace1/... 
#' 
#' @param iFolder Path to the input folder cotaining trace folders.
#' @param oFolder Path to the ouptut folder containig trace folders.
#' @param nTrace Number of traces to process
#' @param fromNames A vector of the file names found in iFolder/traceN.
#' @param toNames A vector of the file names to create in oFolder/traceN.
#' 
#' @return Nothing is returned from function. 
#' 
#' @examples
#' # load the common old and new natural flow files names included with the 
#' # CRSSIO package.
#' \dontrun{
#' iFolder <- 'C:/CRSS/dmi/NFSinputOrig/'
#' oFolder <- 'C:/CRSS/dmi/NFSinputNew/'
#' oldFileNames <- nf_file_names(version = 1)
#' newFileNames <- nf_file_names(version = 2)
#' crssi_change_nf_file_names(iFolder, oFolder, 107,oldFileNames, newFileNames)
#' }
#' 
#' @export
crssi_change_nf_file_names <- function(iFolder, 
                                       oFolder, 
                                       nTrace, 
                                       fromNames, 
                                       toNames)
{
  assert_that(dir.exists(iFolder) && dir.exists(oFolder))
  
  message(paste('Processing', nTrace, "traces:"))
  pb <- utils::txtProgressBar(min = 0, max = nTrace, style = 3)
  
  for (i in seq_len(nTrace)){
    fromPath <- file.path(iFolder, paste0('trace', i))
    toPath <- file.path(oFolder, paste0('trace', i))
    dir.create(toPath)
    for (j in seq_len(length(fromNames))){
      file.copy(
        file.path(fromPath, fromNames[j]),
        file.path(toPath, toNames[j])
      )
    }
    
    utils::setTxtProgressBar(pb, i)
  }
} 
