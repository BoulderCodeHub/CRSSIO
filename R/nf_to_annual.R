#' Sum monthly natural flow data to annual data
#' 
#' `nf_to_annual()` sums `nfd`, and `xts` data from monthly data to annual data.
#'
#' @param x An object inheriting from `xts`. 
#' 
#' @param ... Other parameters passed to methods.
#' 
#' @export
nf_to_annual <- function(x, ...) {
  UseMethod("nf_to_annual")
}

#' @param year "cy" or "wy" to sum over the calendar or water year, 
#'   respectively. For `nfd` like objects, year is determined from the year 
#'   specified in the object. 
#' 
#' @param full_year Only return sums for full years when `TRUE`. Otherwise, will
#'   sum all months in a year, even if that's a partial year.
#' 
#' @seealso [nf_to_intervening()], [nf_to_total()]
#' 
#' @examples 
#' # can sum monthly data to annual and get the existing stored annual data
#' library(CoRiverNF)
#' ann <- nf_to_annual(monthlyTot)
#' all.equal(ann, cyAnnTot, check.attributes = FALSE)
#' 
#' @export
#' 
#' @rdname nf_to_annual
nf_to_annual.xts <- function(x, ..., year = "cy", full_year = TRUE) {
  year <- match.arg(year, c("cy", "wy"))
  
  if (full_year) {
    x <- nfd_trim_ts(x, year = year)
  }
  
  if (year == "cy") {
    ep <- xts::endpoints(x, on = "years")
  } else {
    # for wy, need to manually get the index values of the eowy
    sm <- start(x)
    em <- end(x)
 
    i_start <- get_eowy(sm)
    i_end <- get_eowy(em)
    
    i_start <- min(i_start, em)
    i_end <- min(i_end, em)
    
    # sequence from i_start to i_end by 1 year
    ep <- seq(from = i_start, to = i_end, by = 1)
    
    if (utils::tail(ep, 1) != i_end) {
      ep <- c(ep, i_end)
    }
    
    ep <- c(0, match(ep, zoo::index(x)))
  }
  
  r <- apply(x, 2, xts::period.sum, ep)
  
  if (year == "wy") {
    i <- get_eowy(zoo::index(x)[ep])
  } else {
    i <- get_eocy(zoo::index(x)[ep])
  }
  
  r <- xts::as.xts(r, i)
  
  r
}

#' @param recompute If `nfd` object already has annual data, should the annual
#'   data be recomputed. An error will post if it has annual data and 
#'   `recompute` is `FALSE`.
#'   
#' @param keep_monthly If `TRUE` the monthly data are kept in the returned 
#'   object, otherwise they are dropped.
#'   
#' @examples 
#' # for nfd objects, annual data will be added to object and the monthly 
#' # data are kept by default
#' nf <- nfd(
#'   monthlyTot["2000/2002"], 
#'   flow_space = "total", 
#'   time_step = "monthly"
#' )
#' 
#' nf2 <- nf_to_annual(nf)
#' 
#' # nf2 now has annual data, and monthly data
#' 
#' nf2 <- nf_to_annual(nf, keep_monthly = FALSE)
#' # nf2 no longer has monthly data
#' 
#' @export
#' @rdname nf_to_annual
nf_to_annual.nfd <- function(x, ..., full_year = TRUE, recompute = FALSE,
                             keep_monthly = TRUE) {
  yr <- attr(x, "year")
  assert_that(
    has_monthly(x),
    msg = "`x` has no monthly data."
  )
  
  if (has_annual(x)) {
    assert_that(
      recompute, 
      msg = paste0("`x` already has annual data.\n",
        "Use `recompute = TRUE` to update the annual data.")
    )
  }
  
  mon_int <- mon_tot <- ann_int <- ann_tot <- NULL
  
  if (has_intervening(x, "monthly")) {
    mon_int <- x[['monthly']][["intervening"]]
    ann_int <- nf_to_annual(mon_int, year = yr, full_year = full_year)
  }
  
  if (has_total(x, "monthly")) {
    mon_tot <- x[['monthly']][['total']]
    ann_tot <- nf_to_annual(mon_tot, year = yr, full_year = full_year)
  }
  
  if (!keep_monthly) {
    mon_int <- mon_tot <- NULL
  }
  
  new_nfd(mon_int, mon_tot, ann_int, ann_tot, yr)
}

#' @rdname nf_to_annual
#' @export
nf_to_annual.crss_nf <- function(x, ..., full_year = TRUE, recompute = FALSE,
                                         keep_monthly = TRUE) {
  crss_nf(nf_to_annual(as_nfd(x), ..., full_year, recompute, keep_monthly))
}

#' @rdname nf_to_annual
#' @export
nf_to_annual.crssi <- function(x, ..., recompute = FALSE) {
  # keep_monthly has to be true for crssi, we already know it has full_year
  # of data, so pass full_year = TRUE

  nf <- suppressMessages(as_crss_nf(x))
  nf <- nf_to_annual(nf, keep_monthly = TRUE, recompute = recompute)

  r <- crssi(
    nf, 
    sac_year_type = x$sac_year_type, 
    scen_number = x$scen_number,
    scen_name = x$scen_name,
    drop_flow = FALSE
  )
  
  r
}

nf_to_annual.list <- function(x, ..., year = "cy", full_year = TRUE) {
  year <- match.arg(year, c("cy", "wy"))
  
  r <- lapply(x, function(x_list) {
    nf_to_annual(x_list, year = year, full_year = full_year)
  })
  
  r
} 

# get the end of wy for a given yearmon
get_eowy <- function(ym) {
  
  mask <- month(ym, TRUE) <= 9
  offset <- as.numeric(!mask)
  r <- zoo::as.yearmon(paste("Sep", year(ym, TRUE) + offset))
  
  r
}

get_eocy <- function(ym) {
  r <- zoo::as.yearmon(paste("Dec", year(ym, TRUE)))
  
  r
}
