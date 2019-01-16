
#' `storage_to_elevation()` computes the elevation from a given storage (volume) 
#' for a particular reservoir included in CRSS.
#' 
#' @param storage The volume (in acre-feet) that will be converted to an
#'   elevation (in feet).
#'   
#' @inheritParams elevation_to_storage
#' 
#' @return `storage_to_elevation`() returns an elevation in feet with the same
#'   length as `storage`, or `NA` if the storage is invalid for the reservoir.
#' 
#' @examples 
#' storage_to_elevation(6500000, "mead")
#' storage_to_elevation(100000, "Flaming Gorge")
#' storage_to_elevation(c(100000, 101000, 125000), "flamingGorge")
#' 
#' @rdname elevation_to_storage
#' 
#' @export

storage_to_elevation <- function(storage, reservoir)
{
  reservoir <- check_reservoir(reservoir)
  
  # evTables are system data for this package
  v2s_func <- stats::approxfun(
    evTables[[reservoir]][,2],
    evTables[[reservoir]][,1] 
  )
  
  v2s_func(storage)
}