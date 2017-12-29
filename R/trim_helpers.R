
#' Gets a yearmon series starting in January and ending in December
#' 
#' `get_yearmon_series()` returns a series of `yearmon` data that starts in 
#' January of `starYear` and ends in December `endYear`
#' 
#' @param startYear The year to start the `yearmon` data as a numeric.
#' @param endYear The year to end the `yearmon`` data as a numeric. 
#' 
#' @return Returns a `yearmon` object starting in January and ending in December.
#' 
#' @keywords internal
#' @noRd

get_yearmon_series <- function(startYear, endYear)
{
  if (endYear < startYear){
    stop("endYear cannot be before startYear.")
  }
  
  zoo::as.yearmon(
    startYear + seq(0, (endYear - startYear + 1) * 12 - 1) / 12
  )
}

#' Gets the header info necessary for the trace files
#' 
#' `get_trace_file_header()` returns formatted header info for the input trace
#' files.
#' 
#' @param startYear Numeric year the trace file starts with.
#' @param units The units that are specified in the trace data.
#' 
#' @return Returns a string that is formatted so it can be the header for 
#' the trace files.
#' 
#' @keywords internal
#' @noRd

get_trace_file_header <- function(startYear, units = "acre-ft/month")
{
  paste0("start_date: ", startYear, "-1-31 24:00",'\n', 
         "units: ", units)
}
