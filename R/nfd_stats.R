plot.nfd_stats <- function(x, site, flow_space, time_step, base_units = NULL, 
                     which = "stats", show = TRUE, ...)
{
  # input ok -------------------------------------------
  assert_that(flow_sapce %in% c("total", "intervening", "both"))
  assert_that(time_step %in% c("annual", "monthly", "both"))
  assert_that(all(which %in% c("stats", "cdf")) || all(which %in% c(1:15)))
  assert_that(length(site) == 1 && (site %in% sites(x) || site %in% seq(sites(x))))
  assert_that(length(show) == 1 && is.logical(show))
  
  get_ann <- get_mon <- get_int <- get_tot <- FALSE
  
  if (flow_space %in% c("intervening", "both"))
    get_int <- TRUE
  if (flow_space %in% c("total", "both"))
    get_tot <- TRUE
  if (time_step %in% c("monthly", "both"))
    get_mon <- TRUE
  if (time_step %in% c("annual", "both"))
    get_ann <- TRUE
  
  if (any(which %in% c(1:12, 14)) && !get_mon)
    stop("Cannot create monthly plots if time_step is not monthly or both.")
  
  if (any(which %in% c(13, 15)) && !get_ann)
    stop("Cannot create annual plots if time_step is not annual or both.")
  
  # data exists -----------------------------------------
  # check that necesary data exists for the specified plot types
  if (get_int && get_mon)
    assert_that(has_monthly(x) && has_intervening(x, "monthly"))
  if (get_tot && get_mon)
    assert_that(has_monthly(x) && has_total(x, "monthly"))
  if (get_int && get_ann)
    assert_that(has_annual(x) && has_intervening(x, "annual"))
  if (get_tot && get_ann)
    assert_that(has_annual(x) && has_total(x, "annual"))
  
  # plot by time_step and flow_space ---------------------
  gg_int_mon <- gg_int_ann <- gg_tot_mon <- gg_tot_ann <- NULL
  if (get_int && get_mon)
    gg_int_mon <- plot_monthly(
      nfd_get_site(x, site, "intervening", "monthly"), 
      which, "intervening", base_units
    )
  
  if (get_tot && get_mon)
    gg_tot_mon <- plot_monthly(
      nfd_get_site(x, site, "total", "monthly"), 
      which, "total", base_units
    )
  
  if (get_int && get_ann)
    gg_int_ann <- plot_annual(
      nfd_get_site(x, site, "intervening", "annual"), 
      which, "intervening", base_units
    )
  
  if (get_tot && get_ann)
    gg_tot_ann <- plot_annual(
      nfd_get_site(x, site, "total", "annual"), 
      which, "total", base_units
    )
  
  gg <- c(gg_int_mon, gg_int_ann, gg_tot_mon, gg_tot_ann)
  
  if (show && interactive())
    print(gg)
  
  invisible(gg)
}

# return a nfdplot: list(gg)
plot_monthly <- function(x, which, flow_space, base_units)
{
  
}

plot_annual <- function(x, which, flow_space, base_units)
{
  make_plot <- which_ann_plots(which)
  
  if (make_plot["stats"]) {
    # annual stats
  }
  
  if (make_plot["cdf"]) {
    # annual cdf
  }
}

which_ann_plots <- function(which)
{
  stats <- cdf <- FALSE
  if ("stats" %in% which ||  15 %in% which)
    stats <- TRUE
  
  if ("cdf" %in% which || 13 %in% which)
    cdf <- TRUE
  
  c("stats" = stats, "cdf" = cdf)
}