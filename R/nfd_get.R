#' Get data from `nfd` objects.
#' 
#' `nfd_get_*()` functions get data from an [nfd] object in matrix form. If 
#' there is a time component, then the data are returned as an xts matrix. 
#' To extract the data, but keep as an `nfd` object, see [nfd_extract()].
#' 
#' `nfd_get_site()` returns the data for all traces for one specified `site`.
#' 
#' @param x An `nfd` object.
#' 
#' @param site Site name or index.
#' 
#' @param flow_space "total" or "intervening"
#' 
#' @param time_step "annual" or "monthly"
#' 
#' @return `nfd_get_site()` returns an `xts` object with columns representing 
#' different traces.
#' 
#' @seealso [nfd_extract()], [nfd]
#' 
#' @export
#' @rdname nfd_get_
nfd_get_site <- function(x, site, flow_space, time_step)
{
  assert_that(is_nfd(x))
  assert_that(length(site) == 1)
  flow_space <- match.arg(flow_space, c("total", "intervening"))
  time_step <- match.arg(time_step, c("annual", "monthly"))
  
  # check that site exists
  if (is.character(site)) {
    assert_that(site %in% sites(x))
    site_name <- site
  } else if (is.numeric(site)) {
    assert_that(site %in% seq(n_sites(x)))
    if (!is.null(sites(x)))
      site_name <- sites(x)[site]
  } else {
    stop("Invalid `site`.")
  }
  
  check_flow_ts(x, flow_space, time_step)
  
  rv <- do.call(cbind, lapply(seq(n_trace(x)), function(i) {
    x[[time_step]][[flow_space]][[i]][, site]
  }))
  colnames(rv) <- NULL
  
  # set attributes
  rv <- set_atts(rv, flow_space, time_step, site = site_name)
  rv
}

#' @details 
#' `nfd_get_trace()` returns the data for all sites for one specified `trace`.
#' 
#' @param trace Trace number (numeric scalar)
#' 
#' @return `nfd_get_trace()` returns an `xts` object with columns representing 
#' different sites.
#' 
#' @export
#' @rdname nfd_get_
nfd_get_trace <- function(x, trace, flow_space, time_step)
{
  assert_that(is_nfd(x))
  assert_that(length(trace) == 1 && is.numeric(trace))
  flow_space <- match.arg(flow_space, c("total", "intervening"))
  time_step <- match.arg(time_step, c("annual", "monthly"))
  
  # check that trace exists
  assert_that(
    trace <= n_trace(x), 
    msg = paste0(
      "`trace` should be valid trace index.\n", 
      "nfd object only has ", n_trace(x), " traces of data."
    )
  )
  
  check_flow_ts(x, flow_space, time_step)
  
  rv <- x[[time_step]][[flow_space]][[trace]]
  rv <- set_atts(rv, flow_space, time_step, trace = trace)
  
  rv
}

#' @details 
#' `nfd_get_time()` returns the data for all sites and all traces for the 
#' specified `time`. To get annual data, must specify December or September of 
#' the desired year for calendar year or water year data, respectively.
#' 
#' @param time Time to extract as `yearmon` or `character`.
#' 
#' @return `nfd_get_time()` returns a matrix with columns representing 
#' different sites and rows representing different traces.
#' 
#' @export
#' @rdname nfd_get_
nfd_get_time <- function(x, time, flow_space, time_step)
{
  assert_that(is_nfd(x))
  flow_space <- match.arg(flow_space, c("total", "intervening"))
  time_step <- match.arg(time_step, c("annual", "monthly"))
  
  assert_that(
    length(time) == 1 && (is.character(time) || inherits(time, "yearmon")),
    msg = "`time` should be a scalar charater or yearmon."
  )
  
  check_flow_ts(x, flow_space, time_step)
  
  # check that specified time exists in data
  if (is.character(time))
    time <- zoo::as.yearmon(time)
  
  tmp <- x[[time_step]][[flow_space]]
  all_time <- zoo::index(tmp[[1]])
  assert_that(
    time %in% all_time, 
    msg = "`time` was not found in the index of the nfd data."
  )
  
  rv <- do.call(rbind, lapply(seq(n_trace(x)), function(i) {
    tmp[[i]][time,]
  }))
  
  # do not want an xts object with all same timesteps
  rv <- zoo::coredata(rv)
  
  if (!is.null(sites(x)))
    colnames(rv) <- sites(x)
  else
    colnames(rv) <- NULL
  
  rownames(rv) <- seq(n_trace(x))
  
  rv <- set_atts(rv, flow_space, time_step, time = time)
  
  rv
}

check_flow_ts <- function(x, flow_space, time_step)
{
  # check flow_space and time_step
  if (time_step == "annual") 
    assert_that(
      has_annual(x), 
      msg = "Specified time_step does not exist in nfd object."
    )
  else 
    assert_that(
      has_monthly(x),
      msg = "Specified time_step does not exist in nfd object."
    )
  
  if (flow_space == "total")
    assert_that(
      has_total(x, time_step),
      msg = "Specified flow_space does not exist in nfd object."
    )
  else 
    assert_that(
      has_intervening(x, time_step),
      msg = "Specified flow_space does not exist in nfd object."
    )
  
  invisible(x)
}

set_atts <- function(x, flow_space, time_step, site = NULL, trace = NULL, 
                     time = NULL)
{
  new_atts <- list(
    flow_space = flow_space, 
    time_step = time_step, 
    site = site,
    trace = trace,
    time = time
  )
  
  # remove NULLs
  new_atts <- new_atts[lengths(new_atts) > 0]
  
  if (xts::is.xts(x)) {
    xts::xtsAttributes(x) <- new_atts
  } else {
    for (att in names(new_atts)) {
      attr(x, att) <- new_atts[[att]]
    }
  }
  
  x
}
