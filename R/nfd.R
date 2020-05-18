#' If data is a scalar, then n_time_steps and n_trace are used to determine the 
#' number of traces and time steps. Otherwise, those values are assumed from 
#' the dimensions of the data and the data class. 
#' 
#' For initializing blank annual data - the number of years is computed as the 
#' number of full years with a minimum of 1 year of data: 
#' `n_years = max(floor(n_months / 12), 1)`. The timestep will begin in 
#' December of the year as specified in `start_yearmon` if `year` is calendar 
#' year, or September if the the `year` is water year.
#' 
#' @export
nfd <- function(data = NA, start_yearmon = NA, n_months = NA,
                n_trace = 1, flow_space = c("intervening", "total", "both"), 
                time_step = c("annual", "monthly", "both"), year = c("cy", "wy")
                )
{
  assert_that(length(start_yearmon) == 1)
  assert_that(length(n_months) == 1)
  assert_that(length(n_trace) == 1 && is.numeric(n_trace))
  flow_space <- match.arg(flow_space, c("intervening", "total", "both"))
  time_step <- match.arg(time_step, c("annual", "monthly", "both"))
  year <- match.arg(year, c("cy", "wy"))
  
  if (is.na(start_yearmon)) {
    start_yearmon <- default_yearmon()
  } else {
    start_yearmon <- zoo::as.yearmon(start_yearmon)
  }
  
  if (isTRUE(is.na(data)) || (length(data) == 1 && is.numeric(data))) {
    if (is.na(n_months))
      n_months <- 1
    
    is_monthly <- time_step %in% c("monthly", "both")
    is_annual <- time_step %in% c("annual", "both")
    is_int <- flow_space %in% c("intervening", "both")
    is_tot <- flow_space %in% c("total", "both")
    
    # initialize an empty nfa/ or one with all the same values
    xts_mon <- xts_ann <- NULL
    if (is_monthly ) {
      xts_mon <- initialize_monthly_xts(data, start_yearmon, n_months)
      # create a list with n_trace entries of xts_mon
      
      xts_mon <- lapply(seq(n_trace), function(x) xts_mon)
    }
    
    if (is_annual) {
      xts_ann <- initialize_annual_xts(data, start_yearmon, n_months, year)
      xts_ann <- lapply(seq(n_trace), function(x) xts_ann)
    }
    
    mon_int <- mon_tot <- ann_int <- ann_tot <- NULL
    
    if (is_monthly && is_int)
      mon_int <- xts_mon
    
    if (is_monthly && is_tot)
      mon_tot <- xts_mon
    
    if (is_annual && is_int)
      ann_int <- xts_ann
    
    if (is_annual && is_tot)
      ann_tot <- xts_ann
    
    x <- new_nfd(mon_int, mon_tot, ann_int, ann_tot)
    
    x <- structure(x, class = c("nfd"))
    
  } else {
    x <- as_nfd(data, flow_space, start_yearmon = start_yearmon)
  }
  
  x
}

new_nfd <- function(mon_int, mon_tot, ann_int, ann_tot)
{
  list(
    "annual" = list(
      "intervening" = ann_int,
      "total" = ann_tot
    ),
    "monthly" = list(
      "intervening" = mon_int,
      "total" = mon_tot
    )
  )
}

#' @export
as_nfd <- function(x, flow_space = c("intervening", "total"), 
                   start_yearmon = NA, ...)
{
  UseMethod("as_nfd")
}

as_nfd.default <- function(x, ...)
{
  stop("as_nfd() is not implemented for an object of class: ", class(x),
       "\nSee ?nfd for details.")
}

# TODO
# for as_nfd.array
# lapply(seq(dim(MyArray)[3]), function(x) MyArray[ , , x])

initialize_monthly_xts <- function(val, start_month, n_time_steps)
{
  x <- matrix(val, nrow = n_time_steps, ncol = 29)
  colnames(x) <- nf_gage_abbrv()
  ym <- start_month + (0:(n_time_steps - 1)) / 12
  
  xts::xts(x, order.by = ym)
}

initialize_annual_xts <- function(val, start_month, n_months, year_type)
{
  n_years <- max(floor(n_months / 12), 1)
  
  mon <- c("cy" = "12", "wy" = "09")
  mon <- mon[[year_type]]
  
  ym <- zoo::as.yearmon(paste(year(start_month), mon, sep = "-")) + 
    seq(0, n_years - 1)
  
  x <- matrix(val, nrow = n_years, ncol = 29)
  colnames(x) <- nf_gage_abbrv()
  
  xts::xts(x, order.by = ym)
}

year <- function(x, numeric = FALSE)
{
  x <- format(x, "%Y")
  if (numeric)
    x <- as.numeric(x)
  
  x
}

# takes a yearmon `x`, and creates a vector yearmons starting with x and a 
# specified length
ym_seq <- function(x, length)
{
  x + 0:(length - 1)/12
}

default_yearmon <- function()
{
  zoo::as.yearmon(paste0(format(Sys.Date(), "%Y"), "-01"))
}

n_trace <- function(x)
{
  assert_that(is_nfd(x))
  n1 <- length(x$monthly$intervening)
  n2 <- length(x$monthly$total)
  n3 <- length(x$annual$intervening)
  n4 <- length(x$annual$total)
  
  max_n <- max(n1, n2, n3, n4)
  
  assert_that(all(
    (n1 == max_n || n1 == 0), (n2 == max_n || n2 == 0), 
    (n3 == max_n || n3 == 0), (n4 == max_n || n4 == 0)
  ))
  
  max_n
}

#' @export
is_nfd <- function(x) inherits(x, "nfd")

#' @export
print.nfd <- function(x, ...)
{
  mon_int <- mon_tot <- ann_int <- ann_tot <- NULL
  
  if (!is.null(x$annual$intervening))
    ann_int <- "annual intervening"
  
  if (!is.null(x$annual$total))
    ann_tot <- "annual total"
  
  if (!is.null(x$monthly$intervening))
    mon_int <- "monthly intervening"
  
  if (!is.null(x$monthly$total))
    mon_tot <- "monthly total"
  
  flow_space <- paste(c(ann_int, ann_tot, mon_int, mon_tot), collapse = "\n   -")
  
  cat(
    "nfd: Natural Flow Data\n",
    "----------------------\n",
    "n traces:", n_trace(x), "\n",
    "dates:", as.character(start(x)), "-", as.character(end(x)), "\n",
    "flow space:\n - ", flow_space
  )
  
  invisible(x)
}

has_monthly <- function(x)
{
  !is.null(x$monthly$intervening) || !is.null(x$monthly$total)
}

has_annual <- function(x)
{
  !is.null(x$annual$intervening) || !is.null(x$annual$total)
}

has_intervening <- function(x, timestep = "annual")
{
  !is.null(x[[timestep]][["intervening"]])
}

has_total <- function(x, timestep = "annual")
{
  !is.null(x[[timestep]][["total"]])
}

#' @export
start.nfd <- function(x, ...)
{
  if (has_monthly(x)) {
    if (has_intervening(x, "monthly")) {
      y <- zoo::index(x[["monthly"]][["intervening"]][[1]])[1]
    } else {
      y <- zoo::index(x[["monthly"]][["total"]][[1]])[1]
    }
  } else {
    if (has_intervening(x, "annual")) {
      y <- zoo::index(x[["annual"]][["intervening"]][[1]])[1]
    } else {
      y <- zoo::index(x[["annual"]][["total"]][[1]])[1]
    }
  }
  
  y
}

#' @export
end.nfd <- function(x, ...)
{
  if (has_monthly(x)) {
    if (has_intervening(x, "monthly")) {
      y <- tail(zoo::index(x[["monthly"]][["intervening"]][[1]]), 1)
    } else {
      y <- tail(zoo::index(x[["monthly"]][["total"]][[1]])[1], 1)
    }
  } else {
    if (has_intervening(x, "annual")) {
      y <- tail(zoo::index(x[["annual"]][["intervening"]][[1]])[1], 1)
    } else {
      y <- tail(zoo::index(x[["annual"]][["total"]][[1]])[1], 1)
    }
  }
  
  y
}