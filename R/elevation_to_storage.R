
#' Convert Between Elevation and Volume for a Reservoir
#' 
#' `elevation_to_storage()` computes the storage (volume) for a given elevation 
#' for a particular reservoir included in CRSS.
#' 
#' Elevation is assumed to be in feet, and volume is assumed to be in acre-feet
#' for both input and return values in these functions. Currently, the functions
#' will work for Flaming Gorge, Navajo, Blue Mesa, Powell, and Mead reservoirs. 
#' The elevation-volume tables contain data for 0.5 foot increments. The data
#' are linearly interpolated for sub 0.5 foot values. 
#' 
#' If the input values (`elevation` or `storage`) are beyond the range of the 
#' resevoir, the function returns `NA`.
#' 
#' @param elevation The elevation (in feet) that will be converted to volume.
#' 
#' @param reservoir The reservoir to convert between elevation and storage. 
#'   Spaces are removed from the name, and case is ignored. This should be a 
#'   single reservoir.
#' 
#' @return `elevation_to_storage()` returns volume in acre-feet with the same 
#'   length as `elevation`, or `NA` if the elevation is invalid for the
#'   reservoir.
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
  reservoir <- check_reservoir(reservoir)
  
  # evTables are system data for this package
  e2vFunc <- stats::approxfun(
    evTables[[reservoir]][,1], 
    evTables[[reservoir]][,2]
  )
  
  e2vFunc(elevation)
}

check_reservoir <- function(reservoir)
{
  assert_that(
    length(reservoir) == 1 && is.character(reservoir), 
    msg = "`reservoir` should be a character vector with length = 1."
  )
  
  reservoir <- tolower(gsub(" ", "", reservoir))
  resNames <- c("navajo", "bluemesa", "flaminggorge", "powell", "mead")
  
  assert_that(reservoir %in% resNames, msg = "Invalid reservoir")
  
  reservoir
}
