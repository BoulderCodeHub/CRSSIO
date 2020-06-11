#' @include nfd_stats.R nfd_pdf.R
NULL

#' @param which The month to compute the cdf for. A vector including any/all 
#'   values in `1:12`. Ignored if `time_step` is annual. Will be sorted if 
#'   passed in out of order.
#'   
#' @param breaks Specifies how the breaks are computed in [stats::density()]. 
#'   If `breaks` is `NULL`, then the number of breaks defaults to 25. If 
#'   `breaks` is a single number, that is the number of breaks that will be 
#'   used. Otherwise, `breaks` specifies the breaks, i.e., the number of breaks,
#'   and the minimum (`from`) and maximum (`to`) breaks that are used in 
#'   `stats::density()`. If specifiying for monthly data, `breaks` should be a
#'   matrix where there is a column for each month that a cdf will be computed
#'   for (`which`). 
#' 
#' @return `nfd_pdf` data.frame. 
#' 
#' @export
#' @rdname nfd_stats
nfd_pdf <- function(x, site, flow_space, time_step, which, breaks = NULL, 
                    plot = FALSE) 
{
  assert_that(flow_space %in% c("total", "intervening"))
  assert_that(time_step %in% c("annual", "monthly"))
  assert_that(length(site) == 1) 
  assert_that(
    site %in% sites(x) || site %in% seq(sites(x)),
    msg = "`site` does not exist in nfd object."
  )
  assert_that(length(plot) == 1 && is.logical(plot))
  assert_that(
    is.null(breaks) || (is.numeric(breaks) && length(breaks) >= 1),
    msg = "`breaks` should either be NULL or a numeric vector."
  )
  if (time_step == "monthly") {
    assert_that(all(which %in% 1:12))
    which <- sort(which)
    if (length(breaks > 1))
      assert_that(
        isTRUE(ncol(breaks) == length(which)), 
        msg = paste("ncol(breaks) must match the length(which).\n",
                    " There must be breaks speciifed for each month.")
      )
  }
  
  x_xts <- nfd_get_site(x, site, flow_space, time_step)
  if (time_step == "annual")
    x_df <- nfd_ann_pdf(x_xts, breaks, attr(x, "year"))
  else
    x_df <- nfd_mon_pdf(x_xts, which, breaks, attr(x, "year"))
  
  class(x_df) <- c("nfd_pdf", class(x_df))
  
  if (plot)
    plot(x_df, show = TRUE)
  
  x_df
}

#' If using the output from `nfd_pdf()` to then specify the breaks for another
#' call to `nfd_pdf()`, it is highly recommended to use 
#' `nfd_pdf_get_monthly_breaks()`, which will return a matrix of all the breaks
#' based on the months in `ndfpdf`. 
#' 
#' @param nfdpdf An object inheriting from `nfd_pdf`.
#' 
#' @export
#' @rdname nfd_stats
nfd_pdf_get_monthly_breaks <- function(nfdpdf)
{
  all_months <- sort(unique(nfdpdf$month))
  
  monthly_breaks <- do.call(cbind, lapply(all_months, function(m) {
    nfdpdf[nfdpdf$month == m,]$x
  }))
  
  as.matrix(monthly_breaks)
}

# x is an xts object. one site, multiple traces.
nfd_ann_pdf <- function(x, breaks, year_type)
{
  known_range <- FALSE
  if (is.null(breaks)) {
    n_breaks <- 25
  } else if (length(breaks) == 1) {
    n_breaks <- breaks
  } else {
    n_breaks <- length(breaks)
    known_range <- TRUE
  }
  
  d_out <- compute_pdf(x, known_range, n_breaks, breaks)
  
  # add in the xts attributes as columns
  d_out <- add_xtsatts_as_cols(d_out, xts::xtsAttributes(x), year_type)
  
  d_out
}

# x is an xts object, 1 site, multiple traces (columns)
nfd_mon_pdf <- function(x, which, breaks, year_type)
{
  known_range <- FALSE
  if (is.null(breaks)) {
    n_breaks <- 25
  } else if (length(breaks) == 1) {
    n_breaks <- breaks
  } else {
    n_breaks <- nrow(breaks)
    known_range <- TRUE
  }
  
  # convert which to 0 indexing for months for using xts::.index
  which <- which - 1
  d_out <- do.call(rbind, lapply(seq(which), function(i) {
    m <- which[i]
    # get the data for just the specified month
    tmp <- x[xts::.indexmon(x) == m]
    # compute pdf
    tmp <- compute_pdf(tmp, known_range, n_breaks, breaks[,i])
    # add the month into the df, but back to 1 indexed
    tmp$month <- m + 1
    
    tmp
  }))
  
  d_out <- add_xtsatts_as_cols(d_out, xts::xtsAttributes(x), year_type)
  
  d_out
}

add_xtsatts_as_cols <- function(df, keep_atts, year_type)
{
  df$flow_space <- keep_atts$flow_space
  df$time_step <- keep_atts$time_step
  df$site <- keep_atts$site
  df$year_type <- year_type
  
  df
}

# x is an xts object. computes density accross all data in x
# and returns data frame
compute_pdf <- function(x, known_range, n_breaks, breaks)
{
  if (known_range) {
    # know the from and to. Call that on every column of x
    d1 <- min(breaks)
    d2 <- max(breaks)
    d_out <- apply(x, 2, stats::density, n = n_breaks, from = d1, to = d2)
  } else {
    # first determine the min/max, then call that on very column
    d_out <- stats::density(x, n = n_breaks)
    if (ncol(x) > 1) {
      d1 <- min(d_out$x)
      d2 <- max(d_out$x)
      d_out <- apply(x, 2, stats::density, n = n_breaks, from = d1, to = d2)
    } else {
      # but don't need to recall density if there is only 1 column
      d_out <- list(d_out)
    }
  }
  
  # d_out is a list of objects returned by density. want to convert that to
  # a data frame with trace, x, and y columns
  d_out <- do.call(rbind, lapply(seq(d_out), function(i) {
    data.frame(trace = i, x = d_out[[i]]$x, y = d_out[[i]]$y)
  }))
  
  d_out
}
