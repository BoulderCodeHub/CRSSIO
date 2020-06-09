#' @export
plot.nfd <- function(x, trace = -1, site = 1, flow_space = "both", 
                     time_step = "both", base_units = NULL, which = "box", 
                     show = TRUE, ...)
{
  # input ok -------------------------------------------
  assert_that(flow_space %in% c("total", "intervening", "both"))
  assert_that(time_step %in% c("annual", "monthly", "both"))
  assert_that(length(show) == 1 && is.logical(show))
  
  if (length(trace) == 1 && trace == -1)
    trace <- seq(n_trace(x))

  assert_that(all(trace %in% seq(n_trace(x))))
  # check and convert which to character
  which <- check_plot_nfd_which(which)
  # check and convert site to number
  site <- check_plot_site(x, site)
  site_name <- get_site_name(x, site)
  
  get_ann <- get_mon <- get_int <- get_tot <- FALSE
  
  if (flow_space %in% c("intervening", "both"))
    get_int <- TRUE
  if (flow_space %in% c("total", "both"))
    get_tot <- TRUE
  if (time_step %in% c("monthly", "both"))
    get_mon <- TRUE
  if (time_step %in% c("annual", "both"))
    get_ann <- TRUE
  
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
  year_type <- year_type_label(x)
  if (get_int && get_mon)
    gg_int_mon <- plot_monthly(
      nfd_get_site(x, site, "intervening", "monthly"),
      trace, which, "intervening", site_name, year_type, base_units
    )
  
  if (get_tot && get_mon)
    gg_tot_mon <- plot_monthly(
      nfd_get_site(x, site, "total", "monthly"), 
      trace, which, "total", site_name, year_type, base_units
    )
  
  if (get_int && get_ann)
    gg_int_ann <- plot_annual(
      nfd_get_site(x, site, "intervening", "annual"), 
      trace, which, "intervening", site_name, year_type, base_units
    )
  
  if (get_tot && get_ann)
    gg_tot_ann <- plot_annual(
      nfd_get_site(x, site, "total", "annual"), 
      trace, which, "total", site_name, year_type, base_units
    )

  gg <- c(gg_int_mon, gg_int_ann, gg_tot_mon, gg_tot_ann)
  
  if (show && interactive())
    print(gg)
  
  invisible(gg)
}

check_plot_site <- function(x, site)
{
  assert_that(
    length(site) == 1,
    msg = "`site` should have a length = 1."
  )
  assert_that(
    site %in% sites(x) || site %in% seq(sites(x)), 
    msg = "`site` does not exist in nfd."
  )
  
  if (is.character(site))
    site <- which(site, sites(x))
  
  site
}

check_plot_nfd_which <- function(which)
{
  plot_types <- c("box", "cloud", "spaghetti")
  
  if (is.character(which)) {
    assert_that(
      all(which %in% plot_types), 
      msg = paste0("`which` can only be: ", paste(plot_types, collapse = ", "))
    )
  } else if (is.numeric(which)) {
    assert_that(
      all(which %in% 1:3), 
      msg = "`which` can only be in the [1-3] range."
    )

    which <- plot_types[which]
  } else {
    stop("`which` is not a character or numeric.")
  }
  
  which
}

get_site_name <- function(x, site) 
{
  if (is.null(sites(x))) 
    site_name <- paste("Site", site)
  else
    site_name <- sites(x)[site]
  
  site_name
}

# return a nfdplot: list(gg)
plot_monthly <- function(x, trace, which, flow_space, site_name, year_type,
                         base_units)
{
  
}

plot_annual <- function(x, trace, which, flow_space, site_name, year_type,
                        base_units)
{
  # convert to df
  colnames(x) <- 1:ncol(x)
  x <- xts_to_long_df(x)
  
  if (!all(unique(x$trace) %in% trace))
    x <- dplyr::filter_at(x, "trace", function(i) i %in% trace)
  
  gg_box <- gg_cloud <- gg_spag <- NULL

  if ("box" %in% which) {
    gg_box <- ggplot(x, aes_string("year", "value")) +
      stat_boxplot_custom(aes_string(group = "year"))
    
    gg_box <- nfd_plot_style(gg_box, "Annual", flow_space, site_name, trace, 
                             year_type, base_units)
      
    gg_box <- list(gg_box)
    
  }
  
  if ("spaghetti" %in% which) {
    gg_spag <- ggplot(x, aes_string("year", "value")) +
      geom_line(aes_string(group = "trace"))
    gg_spag <- nfd_plot_style(gg_spag, "Annual", flow_space, site_name, trace, 
                              year_type, base_units)
    gg_spag <- list(gg_spag)
  }
  
  if ("cloud" %in% which) {
    # TODO
  }
  
  gg <- c(gg_box, gg_cloud, gg_spag)
  
  # remove the null lists
  gg <- drop_null(gg)
  
  class(gg) <- "nfdplot"
  gg
}

nfd_plot_style <- function(gg, time_step, flow_space, site_name, trace, 
                           year_type, base_units) 
{
  gg + labs(
    title = paste(time_step, flow_space, "flow at", site_name),
    caption = paste(length(trace), "traces."),
    x = year_type,
    y = base_units
  ) +
    scale_y_continuous(labels = scales::comma)
}

year_type_label <- function(x)
{
  yt_lab <- c("cy" = "Year", "wy" = "Water Year")
  yt <- attr(x, "year")
  
  yt_lab[yt]
}

drop_null <- function(x)
{
  x[lengths(x) != 0]
}

xts_to_long_df <- function(x, add_year = TRUE) 
{
  df <- as.data.frame(x)
  df$ym <- zoo::index(x)
  
  df <- tidyr::pivot_longer(df, -ym, names_to = "trace")
  
  if (add_year)
    df$year <- year(df$ym, TRUE)
  
  df
}

#' @export
print.nfdplot <- function(x, ...)
{
  
}

#' @export
save_nfdplot <- function()
{
  
}

#' @export
c.nfdplot <- function(...)
{
  x <- NextMethod()
  class(x) <- "nfdplot"
  x
}