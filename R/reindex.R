#' Helper Functions for Natural Flow Data Time Series
#' 
#' The objects that store natural flow data ([nfd], [crss_nf], or [crssi]) all 
#' include multiple time series, which are internally stored as `xts` objects. 
#' These functions help modify or query the time step component of these 
#' objects.
#' 
#' `reindex()` changes the time steps, i.e., indeces, of natural flow data as 
#' stored in [nfd], [crss_nf], or [crssi] objects. The time steps are changed
#' to start in the specified `start_year`. This change applies to all time 
#' series data stored in the object, i.e., monthly and annual data and 
#' the Sacramento year type.
#' 
#' @param x An object inheriting from [nfd], [crss_nf], or [crssi].
#' 
#' @param start_year The new starting year as a string or numeric.
#' 
#' @param master_ts Ignored unless `x` has annual and monthly data. If `x` does
#'   contain both, the time step specified here is the "master" time step, i.e., 
#'   it is the time step the `start_year` is applied to. Then, the other 
#'   time step's start year is computed based on keeping the relative difference
#'   in the start year's the same. 
#' 
#' @export
#' @rdname nfd_time_helpers
reindex <- function(x, start_year, master_ts = "monthly")
{
  UseMethod("reindex")
}

#' @export
#' @rdname nfd_time_helpers
reindex.crssi <- function(x, start_year, master_ts = "monthly")
{
  flow <- suppressMessages(as_crss_nf(x))
  flow <- reindex(flow, start_year)
  
  sac_yt <- reindex(x[["sac_year_type"]], start_year)
  x[["annual"]] <- flow[["annual"]]
  x[["monthly"]] <- flow[["monthly"]]
  x[["sac_year_type"]] <- sac_yt
  
  crssi_validate(x)
}

#' @export
#' @rdname nfd_time_helpers
reindex.nfd <- function(x, start_year, master_ts = "monthly")
{
  master_ts <- match.arg(master_ts, c("monthly", "annual"))
  # only monthly or only annual is easy
  sy_mon <- start_year
  sy_ann <- start_year
  
  # if both - master_ts determines which ts controls
  if (has_monthly(x) && has_annual(x)) {
    sy <- get_start_years(x, start_year, master_ts)
    sy_mon <- sy[1]
    sy_ann <- sy[2]
  }
  
  # call reindex.xts for all monthly and annual data that exists in x
  nt <- n_trace(x)
  
  call_reindex <- function(data, new_year) {
    lapply(seq(nt), function(i) {
      reindex.xts(data[[i]], new_year)
    })
  }
  
  if (has_intervening(x, "monthly"))
    x$monthly$intervening <- call_reindex(x$monthly$intervening, sy_mon)
  
  if (has_total(x, "monthly"))
    x$monthly$total <- call_reindex(x$monthly$total, sy_mon)
  
  if (has_intervening(x, "annual"))
    x$annual$intervening <- call_reindex(x$annual$intervening, sy_ann)
  
  if (has_total(x, "annual"))
    x$annual$total <- call_reindex(x$annual$total, sy_ann)
  
  x
}

# Takes an xts object, and updates it to have a new starting year. Maintains 
# the same months as the original object
# does no checking, just updates the index.
#' @param ... Other parameters passed to function. Ignored if `x` is an `xts`
#'   object.
#' @export
#' @rdname nfd_time_helpers
reindex.xts <- function(x, start_year, ...)
{
  assert_that(length(start_year) == 1 && 
                (is.numeric(start_year) || is.character(start_year)))
  
  y <- year(zoo::index(x)[1], TRUE)
  start_year <- as.numeric(start_year)
  delta <- start_year - y
  
  zoo::index(x) <- zoo::index(x) + delta
    
  x
}

# gets the start years for the "master_ts" and the "other_ts" b/c x has both
# annual and monthly data
# always returns start_year as c(monthly start_year, yearly start_year)
get_start_years <- function(x, start_year, master_ts)
{
  if (has_total(x, "monthly")) {
    sym <- start(nfd_get_trace(x, 1, "total", "monthly"))
  } else {
    sym <- start(nfd_get_trace(x, 1, "intervening", "monthly"))
  }
  
  if (has_total(x, "annual")) {
    sya <- start(nfd_get_trace(x, 1, "total", "annual"))
  } else {
    sya <- start(nfd_get_trace(x, 1, "intervening", "monthly"))
  }
  
  sym <- year(sym, TRUE)
  sya <- year(sya, TRUE)
  start_year <- as.numeric(start_year)
  
  if (master_ts == "monthly") {
    # compute delta from monthly, and then apply that to annual sm, to get 
    # annual start_year. 
    delta <- start_year - sym
    sya <- sya + delta
    s <- c(start_year, sya)
  } else {
    # compute delta from annual, and apply to monthly
    delta <- start_year - sya
    sym <- sym + delta
    s <- c(sym, start_year)
  }
  
  s
}
