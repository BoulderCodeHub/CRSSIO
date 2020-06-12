#' Compute Statistics on NFD Objects
#' 
#' `nfd_stats()` computes six basic statistics for each trace in any [nfd] type 
#' objects. `nfd_pdf()` computes an empirical PDF for each trace and returns the
#' probabilities and magnitudes associated with the PDF.
#' 
#' The basic statistics are mean, variance, minimum, maximum, lag-1 
#' correlation, and skew. The resulting statistics are stored in a special 
#' data.frame - a `nfd_stats` object that inherits from 
#' `data.frame`. The resulting statistics can be plotted by calling `plot()` on
#' the resulting object.
#' 
#' @param x An object inheriting from [nfd].
#' 
#' @param site The site to compute the stats for. Numeric or name (character).
#' 
#' @param flow_space "total" or "intervening".
#' 
#' @param time_step "monthly" or "annual".
#' 
#' @param plot Boolean. If `TRUE` will show the plot. Otherwise, user can 
#'   subsequently call `plot()` on the returned data.
#' 
#' @return `nfd_stats` data.frame. 
#' 
#' @seealso [nfd], [plot.nfd_stats()], [plot.nfd_pdf()]
#'
#' @export
nfd_stats <- function(x, site, flow_space, time_step, plot = FALSE) 
{
  assert_that(flow_space %in% c("total", "intervening"))
  assert_that(time_step %in% c("annual", "monthly"))
  assert_that(length(site) == 1) 
  assert_that(
    site %in% sites(x) || site %in% seq(sites(x)),
    msg = "`site` does not exist in nfd object."
  )
  assert_that(length(plot) == 1 && is.logical(plot))
  
  x_xts <- nfd_get_site(x, site, flow_space, time_step)
  if (time_step == "annual")
    x_df <- nfd_ann_stats(x_xts, attr(x, "year"))
  else
    x_df <- nfd_mon_stats(x_xts, attr(x, "year"))
  
  class(x_df) <- c("nfd_stats", class(x_df))
  
  if (plot)
    plot(x_df, show = TRUE)
  
  x_df
}

nfd_ann_stats <- function(x, year_type)
{
  # flow_space, time_step, and site are xts attributes
  keep_atts <- xts::xtsAttributes(x)
  colnames(x) <- seq(ncol(x))
  x <- xts_to_long_df(x, add_year = TRUE)
  x <- get_nfd_stats(x, "value", "trace")
  
  # add attributes to df
  x$flow_space <- keep_atts$flow_space
  x$time_step <- keep_atts$time_step
  x$site <- keep_atts$site
  x$year_type <- year_type
  
  x
}

nfd_mon_stats <- function(x, year_type)
{
  # flow_space, time_step, and site are xts attributes
  keep_atts <- xts::xtsAttributes(x)
  colnames(x) <- seq(ncol(x))
  x <- xts_to_long_df(x, add_month = TRUE)
  x <- get_nfd_stats(x, "value", c("month", "trace"))
  
  # add attributes to df
  x$flow_space <- keep_atts$flow_space
  x$time_step <- keep_atts$time_step
  x$site <- keep_atts$site
  x$year_type <- year_type
  
  x
}

# adapted from get_plot_stats in knnstdisagg package 
# (https://github.com/bouldercodehub/knnstdisagg)
get_nfd_stats <- function(x_df, var_mutate, vars_group)
{
  var_name_order <- c(
    "mean" = "Mean",
    "stats::var" = "Variance",
    "max" = "Maximum",
    "min" = "Minimum",
    "stats::cor" = "Lag-1 Correlation",
    "skew" = "Skew"
  )
 
  res <- x_df %>%
    dplyr::group_by_at(vars_group) %>%
    dplyr::arrange_at("ym") %>%
    dplyr::mutate_at(var_mutate, list("tmp" = dplyr::lag)) %>%
    # means, standard deviation, max, min, skew, lag-1 correlation
    dplyr::summarise_at(
      var_mutate,
      list(
        ~ mean(.), ~ stats::var(.), ~ max(.), ~ min(.), ~ skew(.),
        ~ stats::cor(., get("tmp"), use = "complete.obs")
      )
    ) %>%
    tidyr::gather_(
      "variable",
      "value",
      tidyselect::vars_select(names(.), -tidyselect::one_of(vars_group))
    ) %>%
    dplyr::mutate_at(
      "variable",
      list(~ factor(var_name_order[.], levels = var_name_order))
    )
    
  res
}

#' @author Ken Nowak, Balaji Rajagopalan
#' @noRd
skew <- function(x)
{
  x1 <- x[!is.nan(x)]
  n <- length(x1)
  nfact <- n / ((n - 1) * (n - 2))
  xm <- mean(x, na.rm = TRUE)
  x_sd <- stats::sd(x, na.rm = TRUE)
  skew = sum((x1 - xm)^3)
  
  (nfact * skew) / x_sd^3
}

nfd_stats_verify <- function(x)
{
  exp_cols <- c("trace", "variable", "value", "flow_space", "time_step", "site",
                "year_type")
  assert_that(inherits(x, c("nfd_stats", "data.frame")))
  assert_that(all(exp_cols %in% colnames(x)))
  
  for (col in exp_cols[4:7]) {
    assert_that(
      length(unique(x[[col]])) == 1, 
      msg = paste("Columns", col, "has more than one value: ", 
                  paste(unique(x[[col]]), collapse = ", "),
                  "\nThis makes the data.frame not a nfd_stats object.")
    )
  }
  
  if (x$time_step[1] == "monthly")
    assert_that("month" %in% colnames(x))
  
  exp_vars <- c("Mean", "Variance", "Maximum", "Minimum", "Lag-1 Correlation",
                "Skew")
  
  assert_that(all(levels(x$variable) %in% exp_vars))
  
  invisible(x)
}

#' Plot nfd_stats and nfd_pdf Objects
#' 
#' `plot.nfd_stats()` plots the resulting "base" statistics after calling 
#' [nfd_stats()]. Statistics are plotted as boxplots across traces. 
#' `plot.nfd_pdf()` plots the resulting PDF after calling [nfd_pdf()]. The range
#' of PDFs for every trace are shown as the statistics around the probability
#' of any given flow occurring. For monthly data, one PDF is created for each
#' month. Historical or some other reference trace can be shown on both plots 
#' using `ref`.
#' 
#' @param x An object inheriting from `nfd_stats` or `nfd_pdf`.
#' 
#' @param ref An object inheriting from `nfd_stats`. Optional. Stats in this
#'   object are shown as points/lines instead of boxplots. 
#'   
#' @param base_units Optional. Used as y-axis label. 
#' 
#' @param show Boolean. If `TRUE` and in interactive mode, will show the plot
#'   in the plot window.
#'   
#' @param ... Additional options passed to [ggplot2::geom_point()] and 
#'   [ggplot2::geom_line()]. `size`, `shape`, and `color` can be overridden for
#'   points, and `size`, `linetype`, and `color` can be overriddent for lines. 
#'   
#' @return `nfdplot` object.
#' 
#' @seealso [nfd_stats()], [save_nfdplot()]
#' 
#' @export
plot.nfd_stats <- function(x, ref = NULL, base_units = NULL, show = TRUE, 
                           ...)
{
  # input ok -------------------------------------------
  nfd_stats_verify(x)
  if (!is.null(ref)) {
    nfd_stats_verify(ref)
    assert_that(
      x$time_step[1] == ref$time_step[1],
      msg = "`x` and `ref` must have same time_step."
    )
  }
  
  assert_that(length(show) == 1 && is.logical(show))
  
  if (x$time_step[1] == "annual")
    gg <- nfd_stats_plot_annual(x, ref, base_units, ...)
  else 
    gg <- nfd_stats_plot_monthly(x, ref, base_units, ...)
  
  if (show)
    print(gg)
  
  invisible(gg)
}

# return a nfdplot: list(gg)
nfd_stats_plot_monthly <- function(x, ref, base_units, ...)
{
  shape <- plot_ops("shape", "point", ...)
  color <- plot_ops("color", "point", ...)
  size <- plot_ops("size", "point", ...)
  site <- x$site[1]
  n_trace <- length(unique(x$trace))
  if (x$year_type[1] == "cy")
    mm <- 1:12
  else 
    mm <- c(10:12, 1:9)
  
  x$month <- factor(x$month, levels = mm)
  
  gg <- ggplot(x, aes_string(x = "month", y = "value")) +
    stat_boxplot_custom() +
    facet_wrap("variable", ncol = 2, scales = "free_y")
  
  if (!is.null(ref)) {
    ref$month <- factor(ref$month, levels = mm)
    gg <- gg +
      geom_point(
        data = ref,
        aes_string(x = "month", y = "value"),
        shape = shape,
        color = color,
        size = size
      )
  }
  
  gg <- gg +
    labs(
      x = NULL,
      title = paste(site, "- Monthly Statistics"),
      y = paste("Base units =", base_units),
      caption = paste(n_trace, "traces.")
    ) +
    scale_x_discrete(labels = month.abb[mm])
  
  gg <- list(gg)
  class(gg) <- "nfdplot"
  
  gg
}

nfd_stats_plot_annual <- function(x, ref, base_units, ...)
{
  shape <- plot_ops("shape", "point", ...)
  color <- plot_ops("color", "point", ...)
  size <- plot_ops("size", "point", ...)
  site <- x$site[1]
  n_trace <- length(unique(x$trace))
  year_type <- ifelse(x$year_type[1] == "cy", "Calendar Year", "Water Year")
  
  gg <- ggplot(x, aes_string(y = "value")) +
    stat_boxplot_custom() +
    facet_wrap("variable", ncol = 2, scales = "free_y")
  
  if (!is.null(ref))
    gg <- gg +
      geom_point(
        data = ref,
        aes_string(x = 0, y = "value"),
        shape = shape,
        color = color,
        size = size
      )
    
  gg <- gg +
    labs(
      x = NULL,
      title = paste(site, "-", year_type, "Statistics"),
      y = paste("Base units =", base_units),
      caption = paste(n_trace, "traces.")
    ) +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
    scale_x_continuous(breaks = 0) +
    coord_cartesian( xlim = c(-1, 1))
  
  gg <- list(gg)
  class(gg) <- "nfdplot"
  
  gg
}

# checks ... to see if something was specified, otherwise sets it to default
plot_ops <- function(op, type, ...)
{
  args <- list(...)
  if (exists(op, where = args)) {
    x <- args[[op]]
  } else {
    size <- ifelse(type == "point", 2, 1.2)
    
    defaults <- list(
      color = "#51B2FF",
      size = size,
      shape = 17,
      linetype = 2
    )
    
    x <- defaults[[op]]
  }
  
  x
}

#' @export
rbind.nfd_stats <- function(..., deparse.level = 1)
{
  warning(paste0(
    "Combining multiple nfd_stats objects may have unforseen consequences\n",
    "  when plotting the data. Use with caution."
  ))
  
  rbind.data.frame(..., deparse.level = deparse.level, stringsAsFactors = FALSE)
}
