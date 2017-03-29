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
#' 
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
#' 
readAndFormatNFExcel <- function(iFile)
{
  message('Starting to read in natural flow Excel file. Please be patient this may take several minutes.')

  nf <- xlsx::read.xlsx(iFile, sheetName = 'Intervening Natural Flow')
  # going to take a lot of trimming, etc. to get rid of all the labels we don't need for
  # the flow matrix
  message('Finished reading in natural flow Excel file.')

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

  # remove gap column
  nf <- matrix(as.numeric(nf[,c(1:20,22:30)]),ncol = 29, byrow = F)
  Sys.setenv(TZ = 'UTC') # set the system timezone to UTC
  nf.YearMon <- zoo::as.yearmon('1906-01-31') + seq(0,nrow(nf)-1)/12
  nf <- xts::as.xts(zoo::read.zoo(data.frame(nf.YearMon,nf)))
  nf
}

#' write out a single trace of data
#' @keywords internal
writeSingleFile <- function(xData, fPath, headerInfo)
{
  colnames(xData) = headerInfo
  utils::write.table(xData, file = fPath,quote = F, row.names = F)
}

#' Write out all of the trace files for a given node
#' 
#' @param nfXts Natural flow data as a matrix for a single node. The number of columns
#' is the number of traces that will be written out to oFolder/trace[n]/oFile
#' @param oFile File name that each file will be saved as
#' @param oFolder Folder location to write files to
#' @keywords internal
writeNFFilesByNode <- function(nfXts, oFile, oFolder, headerInfo)
{
  message('Beginning to write node: ',oFile)

  rV <- sapply(1:ncol(nfXts), 
         function(x) writeSingleFile(nfXts[,x], 
                                     file.path(oFolder,paste0('trace',x),oFile),
                                     headerInfo))
}

#' write out the trace number and supply scenario number, to a given trace folder
#' folderPath should be the top level folder, e.g., dmi/NFSinput
#' @keywords internal
writeTraceNumber <- function(traceNum, supplyScenNum, folderPath)
{
  # traceNum will output as follows:
  # units: NONE
  # 1
  # and supplyScenNum will output:
  # units: NONE
  # 1.19882012
  
  traceText <- matrix(c('units: NONE', traceNum),ncol = 1)
  supplyText <- matrix(c('units: NONE', supplyScenNum), ncol = 1)
  folderName <- file.path(folderPath, paste0('Trace', traceNum))
  
  utils::write.table(traceText, file = file.path(folderName, 'Trace.Number'),
                     quote = F, row.names = F, col.names = F)
  utils::write.table(supplyText, file = file.path(folderName, 'Supply.Number'),
                     quote = F, row.names = F, col.names = F)
}
