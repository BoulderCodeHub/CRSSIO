#' Add a Secondary Y-Axis to a ggplot, Converting from the Primary Axis Units
#' 
#' `add_secondary_y_conversion()` adds a secondary y-axis to a ggplot. While it is 
#' intended to be used to add a metric equivelent axis, it will work for any
#' conversion that is handled by [udunits2::ud.convert()].
#' 
#' \describe{
#' \item{`digits`}{Either a number or the string `"get_decimals"`. If a number, 
#' then that is the number of digits to the right of the decimal place that will
#' be shown. If `"get_decimals"`, then the secondary axis will show the same 
#' number of digits to the right of the decimal place as the primary axis does.}
#' \item{units}{For `from_unit` and `to_unit`, the following are common units 
#' used in CRSS that will work with [udunits2::ud.convert()]: "acre_feet", 
#' "m^3", "acre_feet/month", "acre_feet/year", "m^3/s", "km^3/s", "1e6m^3/s".}
#' }
#' 
#' @param gg A ggplot
#' 
#' @param from_unit The primary axis units
#' 
#' @param to_unit The secondary axis units
#' 
#' @param sec_name The name of the secondary axis
#' 
#' @param digits The number of digits to the right of the decimal place to show
#'   on the secondary axis. See details.
#' 
#' @examples 
#' library(ggplot2)
#' df <- data.frame(year = 2020:2029, elev = rnorm(10, 3580, 10))
#' gg <- ggplot(df, aes(year, elev)) + 
#'   geom_line()
#' add_secondary_y_conversion(gg, "feet", "meters")
#' 
#' @export
add_secondary_y_conversion <- function(gg, from_unit, to_unit, sec_name = to_unit, 
                                 digits = "get_decimals") 
{
  assert_that(
    inherits(gg, c("gg", "ggplot")), 
    msg = "`gg` does not inherit from c('gg', 'ggplot')"
  )
  
  current_y_labs <- ggplot2::ggplot_build(gg)$layout$panel_params[[1]]$y.labels
  
  current_y_num <- suppressWarnings(as.numeric(current_y_labs))
  
  if (anyNA(current_y_num)) {
    stop(
      "Current y labels do not appear to be plain numbers.\n",
      " Try calling again, but ensure labels haven not been modified by something like `scales::comma`"
    )
  }
  
  assert_that(
    length(digits) == 1 && (is.character(digits) || is.numeric(digits)),
    msg = "`digits` should be a single numeric value or single string."
  )
  
  if (is.numeric(digits)) {
    num_digits <- digits
  } else {
    assert_that(
      digits %in% c("get_decimals"),
      msg = paste0(
        "`digits` should be an allowable function name.\n", 
        "See ?add_secondary_metric"
      )
    )
    func <- get(digits)
    num_digits <- func(current_y_labs)
  }
  
  gg +
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::sec_axis(
        name = sec_name,
        trans = ~udunits2::ud.convert(., from_unit, to_unit), 
        breaks = udunits2::ud.convert(current_y_num, from_unit, to_unit), 
        labels = to_metric_labels(
          udunits2::ud.convert(current_y_num, from_unit, to_unit), 
          num_digits
        )
      )
    )
}

get_decimals <- function(x)
{
  nums <- simplify2array(strsplit(x, ".", fixed = TRUE))

  if (is.list(nums)) {
    # there is at least one number with no decimals and at least one number 
    # with decimals, so have to look into the list manually
    nums <- lapply(seq_len(length(nums)), function(i) {
      if (length(nums[[i]]) == 2) {
        nums[[i]]
      } else {
        c(nums[[i]][1], NA_real_)
      }
    })
    nums <- t(do.call(rbind, nums))
  }
  
  if(!is.null(nrow(nums))) {
    # there are decimals
    decimals <- as.numeric(max(nchar(nums[2,]), na.rm = TRUE))
  } else {
    decimals <- 0
  }
  
  decimals
}

to_metric_labels <- function(x, decimals)
{
  formatC(x, big.mark = ",", format = "f", digits = decimals)
}
