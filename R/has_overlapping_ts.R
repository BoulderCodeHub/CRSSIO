# See reindex.R for other documentation
#' @details 
#' `has_overlapping_ts()` determines whether the the annual and monthly data 
#' within the object are overlapping. If `exact` is `TRUE`, the start and end
#' time step for the monthly and annual data must match "exactly". This means for
#' an object storing calendar year data that the monthly data will start in 
#' January, year1 and end in December, year2 while the annual data must start 
#' in December, year1 and end in December, year2. The analogous is true for 
#' water year data, except the start and end months are October and September, 
#' respectively. 
#' 
#' If the object contains only monthly, or only annual data, 
#' `has_overlapping_ts()` will always return TRUE.
#' 
#' @param exact Boolean. Do the annual and monthly data have to overlap 
#'   "exactly". See *Details.* 
#'   
#' @export
#' @rdname nfd_time_helpers
has_overlapping_ts <- function(x, exact = TRUE)
{
  UseMethod("has_overlapping_ts")
}

#' @export
has_overlapping_ts.nfd <- function(x, exact = TRUE)
{
  reg <- FALSE
  if (has_annual(x) && has_monthly(x))
  {
    if (!exact) {
      # only check that there are some year/months that exist in both 
      # monthly and annual
      # check intervening, then total
      if (has_intervening(x, "monthly") && has_intervening(x, "annual"))
        reg <- reg || 
            any(zoo::index(nfd_get_trace(x, 1, "intervening", "annual")) %in% 
              zoo::index(nfd_get_trace(x, 1, "intervening", 'monthly')))
      
      if (has_total(x, "monthly") && has_total(x, "annual"))
        reg <- reg || 
            any(zoo::index(nfd_get_trace(x, 1, "total", "annual")) %in% 
              zoo::index(nfd_get_trace(x, 1, "total", 'monthly')))
    } else {
      year_type <- attr(x, "year")
      assert_that(year_type %in% c("cy", "wy"),
                  msg = "Invalid year attribute in nfd object.")
      reg <- TRUE
      if (has_intervening(x, "monthly") && has_intervening(x, "annual"))
        reg <- reg &&
          exact_overlap(
            zoo::index(nfd_get_trace(x, 1, "intervening", 'monthly')), 
            zoo::index(nfd_get_trace(x, 1, "intervening", "annual")), 
            year_type
          )
      
      if (has_total(x, "monthly") && has_total(x, "annual"))
        reg <- reg &&
        exact_overlap(
          zoo::index(nfd_get_trace(x, 1, "total", 'monthly')), 
          zoo::index(nfd_get_trace(x, 1, "total", "annual")), 
          year_type
        )
    }
  } else 
    reg <- TRUE
  
  reg
}

# checks to see if the month and year have an "exact" overlap
exact_overlap <- function(i_mon, i_year, year_type)
{
  sm <- c("wy" = "10", "cy" = "01")
  em <- c("wy" = "09", "cy" = "12")
  sm <- sm[year_type]
  em <- em[year_type]
  i <- ifelse(year_type == "wy", 1, 0)
  
  all(
    month(i_mon[1]) == sm,
    month(tail(i_mon, 1)) == em,
    year(i_mon[1], numeric = TRUE) + i == year(i_year[1], numeric = TRUE),
    year(tail(i_mon,1)) == year(tail(i_year, 1)),
    month(i_year[1]) == em,
    month(tail(i_year, 1)) == em
  )
}
