#' @include nfd_stats.R nfd_pdf.R
NULL

#' @param which The month to compute the cdf for. A vector including any/all 
#'   values in `1:12`. Ignored if `time_step` is annual. Will be sorted if 
#'   passed in out of order.
#'   
#' @param breaks Specifies how the breaks are computed in [stats::density()], 
#'   which is used by `nfd_pdf()`. 
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
    if (length(breaks) > 1)
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

#' @details 
#' If using the output from `nfd_pdf()` to then specify the breaks for another
#' call to `nfd_pdf()`, it is highly recommended to use 
#' `nfd_pdf_get_breaks()`. `nfd_pdf_get_breaks()` will return a matrix of all 
#' the breaks if `nfdpdf` contains monthly data, which is the format expected
#' in `nfd_pdf()`. For annual data, it still works but is less necesary.  
#' 
#' @param nfdpdf An object inheriting from `nfd_pdf`.
#' 
#' @return `nfd_pdf_get_breaks()` returns a numeric vector (for annual data) or 
#'   matrix (for monthly data).
#' 
#' @export
#' @rdname nfd_stats
nfd_pdf_get_breaks <- function(nfdpdf)
{
  assert_that(inherits(nfdpdf, "nfd_pdf"))
  
  if (nfdpdf$time_step[1] == "monthly") {
  
    all_months <- sort(unique(nfdpdf$month))
    
    breaks <- do.call(cbind, lapply(all_months, function(m) {
      nfdpdf[nfdpdf$month == m,]$x
    }))
    
    breaks <- as.matrix(breaks)
  } else {
    breaks <- nfdpdf$x
  }
  
  breaks
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

#' @details 
#' For monthly plots, `plot.nfd_pdf()` will create an individual plot for all 
#' months that exist in the `nfd_pdf` data frame. 
#' 
#' @seealso [nfd_pdf()]
#' 
#' @export
#' @rdname plot.nfd_stats
plot.nfd_pdf <- function(x, ref = NULL, base_units = NULL, show = TRUE, ...)
{
  # input ok -------------------------------------------
  nfd_pdf_verify(x)
  if (!is.null(ref)) {
    nfd_pdf_verify(ref)
    assert_that(
      x$time_step[1] == ref$time_step[1],
      msg = "`x` and `ref` must have same time_step."
    )
  }
  
  assert_that(length(show) == 1 && is.logical(show))
  
  # plot ------------------------------------------------
  if (x$time_step[1] == "annual")
    gg <- nfd_pdf_plot_annual(x, ref, base_units, ...)
  else 
    gg <- nfd_pdf_plot_monthly(x, ref, base_units, ...)
  
  if (show)
    print(gg)
  
  invisible(gg)
}

nfd_pdf_plot_annual <- function(x, ref, base_units, ...)
{
  lt <- plot_ops("linetype", "line", ...)
  color <- plot_ops("color", "line", ...)
  lw <- plot_ops("linewidth", "line", ...)
  
  n_trace <- length(unique(x$trace))
  
  if (n_trace > 1) {
    gg <- gg_ribbon_cloud(ggplot(ribbon_stats(x, "x", "y"), aes(!!sym("x"))))
  
  } else {
    gg <- ggplot(x, aes(!!sym("x"), !!sym("y"))) + 
      geom_line(color = "grey20", linewidth = 1)
  }
  
  add_ref <- FALSE
  if (!is.null(ref)) {
    gg <- gg +
      geom_line(
        data = ref, 
        aes(!!sym("x"), !!sym("y")), 
        color = color, linetype = lt, linewidth = lw
      )
    add_ref <- TRUE
  }
  
  gg <- gg_pdf_style(gg, x, add_ref, base_units)
  
  gg <- list(gg)
  class(gg) <- "nfdplot"
  
  gg
}

nfd_pdf_plot_monthly <- function(x, ref, base_units, ...)
{
  # loop through x and call nfd_pdf_plot_annual for each set of monthly data
  all_months <- unique(x$month)
  
  gg <- do.call(c, lapply(all_months, function(mm) {
    x_temp <- dplyr::filter_at(x, "month", dplyr::any_vars(. == mm))
    ref_temp <- NULL
    if (!is.null(ref))
      ref_temp <- dplyr::filter_at(ref, "month", dplyr::any_vars(. == mm))
    nfd_pdf_plot_annual(x_temp, ref_temp, base_units, ...)
  }))
  
  gg
}

ribbon_stats <- function(x, group_variables, summary_var)
{
  x %>%
    dplyr::group_by_at(group_variables) %>%
    dplyr::summarise_at(summary_var, list(
      "q05" = ~quantile(., 0.05),
      "q25" = ~quantile(., 0.25),
      "q50" = ~quantile(., 0.50),
      "q75" = ~quantile(., 0.75),
      "q95" = ~quantile(., 0.95)
    ))
}

nfd_pdf_verify <- function(x)
{
  exp_cols <- c("trace", "x", "y", "flow_space", "time_step", "site", 
                "year_type")
  assert_that(inherits(x, c("ndf_pdf", "data.frame")))
  assert_that(all(exp_cols %in% colnames(x)))
  for (col in exp_cols[4:7]) {
    assert_that(
      length(unique(x[[col]])) == 1,
      msg = paste("Columns", col, "has more than one value: ", 
                  paste(unique(x[[col]]), collapse = ", "),
                  "\nThis makes the data.frame not a nfd_pdf object.")
    )
  }
  
  if (x$time_step[1] == "monthly")
    assert_that("month" %in% colnames(x))
  
  invisible(x)
}

gg_ribbon_cloud <- function(gg)
{
  fill_map <- c("median" = "grey20","25th-75th" = "grey60",
                "5th-95th" = "grey80")
  
  gg + 
    geom_ribbon(
      aes(ymin = q50, ymax = q50, fill = "median", color = "grey20"), 
      linewidth = 1
    ) + 
    geom_ribbon(aes(ymin = q05, ymax = q95, fill = "5th-95th"), alpha = 0.6) + 
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = "25th-75th"), alpha = 0.5) + 
    geom_line(aes(y = q50), color = "grey20", linewidth = 1) +
    scale_fill_manual("Percentile:", values = fill_map) + 
    scale_color_manual("", values = "grey20", guide = "none")
}

gg_pdf_style <- function(gg, x, add_ref, base_units)
{
  site <- x$site[1]
  flow_space <- x$flow_space[1]
  year_type <- ifelse(x$year_type[1] == "cy", "Calendar Year", "Water Year")
  n_trace <- length(unique(x$trace))
  time_step <- x$time_step[1]
  
  cap_text <- paste(n_trace, ifelse(n_trace == 1, "trace.", "traces."))
  if (add_ref)
    cap_text <- paste(cap_text, "Colored line is from reference period.")
  
  title_text <- paste(site, "PDF")
  if (time_step == "annual")
    sub_text <- paste(year_type, "Annual Flow")
  else
    sub_text <- paste(month.name[x$month[1]], "Flow")
  
  gg +
    scale_x_continuous(labels = scales::comma) +
    labs(x = base_units, y = "Probability", 
         title = title_text, subtitle = sub_text, caption = cap_text) 
}

#' @export
rbind.nfd_pdf <- function(..., deparse.level = 1)
{
  warning(paste0(
    "Combining multiple nfd_pdf objects may have unforseen consequences\n",
    "  when plotting the data. Use with caution."
  ))
  
  rbind.data.frame(..., deparse.level = deparse.level, stringsAsFactors = FALSE)
}