#' Create a matrix of data based on ISM
#' 
#' `ism_get_site_matrix()` takes in a matrix of historical data for a single 
#' site and applies the Index Sequential Method (ISM) to it. Because this is 
#' typically used to create future hydrology data, the entire matrix will have 
#' a new start month (`starMonth`). 
#' 
#' The method can be used on monthly or annual data. If you are applying it to 
#' monthly data, then \code{xtsData} needs to be monthly data, and \code{monthly}
#' should be set to \code{TRUE}. If using annual data, then \code{xtsData} should 
#' be annual, i.e., all with a December timestamp, and \code{monthly} should be
#' set to \code{FALSE}. If \code{monthly} is \code{FALSE} and \code{xtsData} is 
#' monthly data, an error will occur.
#' 
#' @return xts matrix with the number of years/months specified by `nYrs` 
#'   and the number of columns equal to the number of years of data in `xtsData`
#' 
#' @examples 
#' # monthly data, that will create a 48x4 xts matrix
#' t1 <- xts::xts(1:48, zoo::as.yearmon("Jan 2000") + seq(0,47)/12)
#' ism_get_site_matrix(t1, "Jan 2020")
#' 
#' # annual data that will create a 5 x 6 matrix
#' t2 <- xts::xts(1:6, zoo::as.yearmon("Dec 2000") + 0:5)
#' ism_get_site_matrix(t2, "Dec 2020", nYrs = 5, monthly = FALSE)
#' 
#' @param xtsData An xts vector.
#' @param startMonth The start month and year of the return matrix. Should be 
#'   able to be cast to a [zoo::yearmon].
#' @param nYrs The number of years to create the data for. Defaults to the 
#'   number of years in xtsData, but can be less. 
#' @param monthly Boolean that should be set to `TRUE` if the data are monthly; 
#'   should set to `FALSE` if annual data.
#' 
#' @export
ism_get_site_matrix <- function(xtsData, startMonth, nYrs = NA, monthly = TRUE)
{
  if(!xts::is.xts(xtsData)){
    stop('xtsData is not of type xts')
  }
  
  if(is.na(nYrs)){
    nYrs <- xts::nyears(xtsData)
  } else{
    if(nYrs > xts::nyears(xtsData))
      stop('nYrs is longer than xtsData.')
  }
  
  # make the data not an xts object so we can rbind it together
  zz <- matrix(unclass(xtsData))#, nrow = length(xtsData))
  zz <- rbind(zz,zz) # now can easily loop through the data for ISM
  
  ntraces <- 1:xts::nyears(xtsData)

  ismMatrix <- simplify2array(
    lapply(ntraces, getSubsetOfData, zz, nYrs, monthly)
  )
  
  # now convert back to xts object with monthly timestep
  if(monthly) {
    ismYearMon <- zoo::as.yearmon(startMonth) + seq(0,nrow(ismMatrix)-1)/12
  } else{
    ismYearMon <- zoo::as.yearmon(startMonth) + seq(0,nrow(ismMatrix)-1)
  }
  ismMatrix <- xts::as.xts(zoo::read.zoo(data.frame(ismYearMon, ismMatrix)))
  ismMatrix
}

#' @export
#' @rdname ism_get_site_matrix
createISMMatrix <- function(xtsData, startMonth, nYrs = NA, monthly = TRUE)
{
  .Deprecated("ism_get_site_matrix")
  ism_get_site_matrix(xtsData, startMonth, nYrs, monthly)
}
