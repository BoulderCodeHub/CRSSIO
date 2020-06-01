#' @include nfd.R crss_nf.R
NULL

#' @description 
#' `crss_nf()` is the constructor to create `crss_nf` obects. This class is an 
#' extension of `nfd`, however it imposes a strict number and naming convention
#' to the sites. Where `nfd` objects can have an arbitrary number of sites, and
#' they do not have to be named, `crss_nf` objects must have 29 sites, the names
#' must match the expected site names for CRSS gages [nf_gage_abbrv()], and the
#' data must include monthly intervening data. (Annual and/or monthly total can 
#' also be included, but they do not have to be.)
#' 
#' @details 
#' When creating `crss_nf` objects, arrays, matrices, and xts objects must have 
#' the site dimension named and those names must match the expected natural flow 
#' site names [nf_gage_abbrv()].
#' 
#' @return `crss_nf()` and `as_crss_nf()` return an object of class `crss_nf`.
#' 
#' @export
#' @rdname nfd
crss_nf <- function(data = NA, n_months = NA, n_trace = 1, 
                flow_space = c("intervening", "both"), 
                time_step = c("monthly", "both"), 
                start_yearmon = NA, year = c("cy", "wy")
)
{
  flow_space <- match.arg(flow_space, c("intervening", "both"))
  time_step <- match.arg(time_step, c("monthly", "both"))
 
  if (is_nfd(data)) {
    x <- as_crss_nf(data)
  } else {
    # if creating from scalar, want to make sure the correct dimension/name is
    # created. but if creating from existing data, will compute dimensions based
    # on data shape
    if (length(data) == 1) {
      n_sites <- 29
      site_names <- nf_gage_abbrv()
    } else {
      # defaults from nfd
      n_sites <- 29
      site_names <- NA
    }
    x <- nfd(data, n_months, n_trace, n_sites = n_sites, flow_space, time_step,
             start_yearmon, year, site_names = site_names)
    
    crss_nf_validate(x)
    
    class(x) <- c("crss_nf", "nfd")
  }
  
  x
}

#' @param x An `R` object.
#' @param ... Other parameters passed to `crss_nf()`.
#' @export
#' @rdname nfd
as_crss_nf <- function(x, ...)
{
  UseMethod("as_crss_nf")
}

as_crss_nf.default <- function(x, ...)
{
  stop("as_crss_nf() is not implemented for an object of class: ", class(x),
       "\nSee ?as_crss_nf for details.")
}

#' @export
as_crss_nf.nfd <- function(x, ...)
{
  crss_nf_validate(x)
  class(x) <- c("crss_nf", "nfd")
  x
}

#' @export
as_crss_nf.crssi <- function(x, ...)
{
  # drop sac_year_type, n_trace, scen_name, scen_number
  drop_vals <- c("sac_year_type", "n_trace", "scen_number", "scen_name")
  
  if (!exists("scen_name", where = x))
    drop_vals <- drop_vals[1:3]
  
  for (dv in drop_vals) {
    x[[dv]] <- NULL
  }
  
  message("Dropping ", paste(drop_vals, collapse = ", "), 
          "\nfrom crssi object to create crss_nf object.")
  
  crss_nf_validate(x)
  class(x) <- c("crss_nf", "nfd")
  x
}

#' @export
as_crss_nf.array <- function(x, ...)
{
  assert_that(dim(x)[3] == 29, msg = "Must have 29 sites.")
  
  as_crss_nf(as_nfd(x, ...))
}

#' @export
as_crss_nf.matrix <- function(x, ...)
{
  assert_that(ncol(x) == 29, msg = "Must have 29 columns.")
  
  as_crss_nf(as_nfd(x, ...))
}

#' @export
as_crss_nf.xts <- function(x, ...)
{
  assert_that(ncol(x) == 29, msg = "Must have 29 columns.")
  
  as_crss_nf(as_nfd(x, ...))
}

#' @export
#' @return `is_crss_nf()` returns `TRUE` if class inherits from `crss_nf`.
#' @rdname nfd
is_crss_nf <- function(x) {
  inherits(x, "crss_nf")
}

crss_nf_validate <- function(x)
{
  assert_that(inherits(x, "nfd"))
  assert_that(n_sites(x) == 29)
  assert_that(
    !is.null(sites(x)) && all(sites(x) == nf_gage_abbrv()), 
    msg = "Sites must be named and ordered the same as nf_gage_abbrv()."
  )
  assert_that(has_monthly(x) && has_intervening(x, "monthly"))
  invisible(x)
}
