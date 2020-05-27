#' Get data from `nfd` objects.
#' 
#' `nfd_get_*()` functions get data from an [nfd] object in matrix form. If 
#' there is a time component, then the returend data are kepts as an xts matrix. 
#' To extract the data, but keep it as an `nfd` object, see [nfd_extract()].
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

set_atts <- function(x, flow_space, time_step, site = NULL, trace = NULL)
{
  new_atts <- list(
    flow_space = flow_space, 
    time_step = time_step, 
    site = site,
    trace = trace
  )
  
  # remove NULLs
  new_atts <- new_atts[lengths(new_atts) > 0]
  
  if (is.xts(x)) {
    xtsAttributes(x) <- new_atts
  } else {
    for (att in names(new_atts)) {
      attr(x, att) <- new_atts[[att]]
    }
  }
  
  x
}
