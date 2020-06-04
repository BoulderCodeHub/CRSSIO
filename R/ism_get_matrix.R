#' @details 
#' `ism_get_site_matrix()` is a deprecated version of `ism()` that only works 
#' on `xts` objects. It takes in 1 column of historical data for a single 
#' site and applies (ISM) to it. This function allows you to change the start
#' date of the returned data in it, while `ism()` does not. When using `ism(), 
#' [reindex()] should be used after it to change the start date.
#' `ism_get_site_matrix()` can be used on monthly or annual data. If applying it
#' to monthly data, then `xtsData` needs to be monthly data, and `monthly`
#' should be set to `TRUE`. If using annual data, then `xtsData` should 
#' be annual, i.e., all with a December time stamp, and `monthly` should be
#' set to `FALSE`. If `monthly` is `FALSE` and `xtsData` is 
#' monthly data, an error will occur.
#' 
#' @return `ism_get_site_matrix()` returns an `xts` matrix with the number of 
#'   years/months specified by `nYrs` and the number of columns equal to the 
#'   number of years in `xtsData`
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
#' @rdname ism
ism_get_site_matrix <- function(xtsData, startMonth, nYrs = NA, monthly = TRUE)
{
  .Deprecated("ism")
  
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
