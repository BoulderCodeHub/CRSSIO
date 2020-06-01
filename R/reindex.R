#' Helper Functions for Natural Flow Data Time Steps
#' 
#' The objects that store natural flow data ([nfd], [crss_nf], or [crssi]) all 
#' include multiple xts objects. These functions help modify or query the 
#' timestep component of these objects.
#' 
#' `reindex()` changes the timesteps, i.e., indeces, of natural flow data as 
#' stored in [nfd], [crss_nf], or [crssi] objects.
#' 
#' @param x An object inheriting from [nfd], [crss_nf], or [crssi].
#' 
#' @param start_year The new starting year as a string or numeric.
#' 
#' @export
#' @rdname nfd_time_helpers
reindex <- function(x, start_year)
{
  UseMethod("reindex")
}

#' @export
reindex.crssi <- function(x, start_year)
{
  flow <- as_crss_nf(x)
  flow <- reindex(flow, start_year)
  
  sac_yt <- reindex(x[["sac_year_type"]], start_year)
  x[["annual"]] <- flow[["annual"]]
  x[["monthly"]] <- flow[["monthly"]]
  x[["sac_year_type"]] <- sac_yt
  
  crssi_validate(x)
}

#' @export
reindex.nfd <- function(x, start_year)
{
  # TODO: need the "regularize" functions first
}

# Takes an xts object, and updates it to have a new starting year. Maintains 
# the same months as the original object
# does not fancy checking, just updates the index.
reindex.xts <- function(x, start_year)
{
  assert_that(length(start_year) == 1 && 
                (is.numeric(start_year) || is.character(start_year)))
  
  mm <- month(zoo::index(x)[1])
  cc <- colnames(x)
  
  if (nrow(x) > 1) {
    prd <- xts::periodicity(x)
  
    if (prd$scale == "monthly") {
      denom <- 12
    } else if (prd$scale == "yearly") {
      denom <- 1
    } else {
      stop("CRSSIO:::reindex.xts() is only setup to handle monthly and yearly data.")
    }
  } else {
    denom <- 1
  }
  
  n <- nrow(x) - 1
  
  new_index <- zoo::as.yearmon(paste0(start_year, "-", mm)) + 0:n / denom
  
  x <- xts(x, order.by = new_index)
  colnames(x) <- cc
  
  x
}
