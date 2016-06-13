
# returns subset of a data
# assumes monthly data
getSubsetOfData <- function(startYear, monData, nYrs)
{
  startI <- startYear*12-11
  monData[startI:(nYrs*12+startI-1)]
}

#' Create a matrix of data based on the ISM method
#' 
#' Takes in a matrix of historical data and applies the ISM method to it. Because
#' this is typically used to create future hydrology data, the entire matrix
#' will have a new start month. 
#' 
#' @return xts matrix
#' 
#' @param xtsData An xts vector
#' @param startMonth The startMonth of the return matrix. Should be able to be
#' cast to a zoo::yearmon
#' @param nYrs The number of years to create the data for. Defaults to the number
#' of years in xtsData, but can be less. 
#' 
#' @export
#' 
createISMMatrix <- function(xtsData, startMonth, nYrs = NA)
{
  if(!xts::is.xts(xtsData)){
    stop('xtsData is not of type xts')
  }
  
  # using nmonths/12 because xtsnyears returns too many. 
  
  if(is.na(nYrs)){
    nYrs <- xts::nmonths(xtsData)/12
  } else{
    if(nYrs > xts::nmonths(xtsData)/12)
      stop('nYrs is longer than xtsData.')
  }
  
  # make the data not an xts object so we can rbind it together
  zz <- matrix(unclass(xtsData))#, nrow = length(xtsData))
  zz <- rbind(zz,zz) # now can easily loop through the data for ISM
  
  ntraces <- 1:(xts::nmonths(xtsData)/12)

  ismMatrix <- sapply(ntraces, getSubsetOfData, zz, nYrs)
  
  # now convert back to xts object with monthly timestep
  ism.yearMon <- zoo::as.yearmon(startMonth) + seq(0,nrow(ismMatrix)-1)/12
  ismMatrix <- xts::as.xts(zoo::read.zoo(data.frame(ism.yearMon, ismMatrix)))
  ismMatrix
}
