
#' Compute Volume from Elevation for a Reservoir
#' 
#' `elevation_to_storage()` computes the storage (volume) for a given elevation 
#' for a particular reservoir included in CRSS.
#' 
#' Elevation is assumed to be provided in feet, and the volume will be returned 
#' in acre-feet. Currently, the function will work for Flaming Gorge, Navajo, 
#' Blue Mesa, Powell, and Mead reservoirs. The elevation-volume tables contain 
#' data for 0.5 foot increments. Volume is linearly interpolated for sub 0.5
#' foot values of elevation. 
#' 
#' If the elevation is beyond the range of the resevoir, the function returns 
#' \code{NA}.
#' 
#' @param elevation The elevation (in feet) that will be converted to volume
#' @param reservoir The reservoir to compute the conversion for. Spaces are 
#' removed from the name, and case is ignored.
#' 
#' @return Volume in acre-feet, or \code{NA} if the elevation is invalid for the
#' reservoir.
#' 
#' @examples 
#' elevation_to_storage(1075, "mead")
#' elevation_to_storage(6000, "Flaming Gorge")
#' elevation_to_storage(c(6000, 6001, 6004.6), "flamingGorge")
#' 
#' @export
#' 

elevation_to_storage <- function(elevation, reservoir)
{
  reservoir <- tolower(gsub(" ", "", reservoir))
  resNames <- c("navajo", "bluemesa", "flaminggorge", "powell", "mead")
  if(!(reservoir %in% resNames))
    stop("Invalid reservoir")
  
  # evTables are system data for this package
  e2vFunc <- stats::approxfun(
    evTables[[reservoir]][,1], 
    evTables[[reservoir]][,2]
  )
  
  e2vFunc(elevation)
}

#' @export
#' @rdname elevation_to_storage
elevation2Volume <- function(elevation, reservoir)
{
  .Deprecated("elevation_to_storage")
  elevation_to_storage(elevation, reservoir)
}