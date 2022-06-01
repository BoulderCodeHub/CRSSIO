#' Natural Flow Data (nfd)
#' 
#' `nfd()` creates an object to store natural flow data from the specified 
#' `data`. Conceptually, the `nfd` object is a 5-dimensional array, where the 
#' first three dimensions represent time (months or years), traces, and sites. 
#' The next two dimensions are the "flow space" (intervening or total flow), and
#' then the time time step. This allows for the storage of all natural flow data
#' used in CRSS and Colorado River modeling in one object, or to limit it and
#' only store the site or flow space of interest.
#' 
#' If `data` is a scalar, then `n_months` and `n_trace` are used to determine 
#' the number of traces and time steps. Otherwise, those values are assumed from 
#' the dimensions of the data and the data class. See **Data Types** section for 
#' a description of how different data types are handled. 
#' 
#' For initializing blank annual data - the number of years is computed as the 
#' number of full years with a minimum of 1 year of data: 
#' `n_years = max(floor(n_months / 12), 1)`. The timestep will begin in 
#' December of the year as specified in `start_yearmon` if `year` is calendar 
#' year, or September if the the `year` is water year.
#' 
#' The data are assumed to always exist for all sites first, and then the number
#' of timesteps or traces are determined after that. 
#' 
#' @section Data Types:
#' 
#' This section describes how different data types for the specified `data`/`x` 
#' are treated when creating an `nfd` object.
#' 
#' *Array:* Arrays should be an m x t x s array, where m is the number of
#' months (or years), t is the number of traces, and s is the number of sites. 
#' Array can also be an m x t x s x 2 array, where `x[,,,1]` is total flow 
#' and `x[,,,2]` is intervening flow. 
#' If there are rownames, then they must be in "yyyy-mm" format, 
#' otherwise an error will post. Rownames are not required, and if they are not
#' provided will be set starting with the specified `start_yearmon` or assuming
#' a starting date of January of the current year. Colnames are not required or 
#' used.
#' The names of the 3rd dimension represent the site names, and will be used if 
#' provided, otherwise the site names remain `NULL`.
#' 
#' *Matrix:* Matrices are assumed to be structured as m x s, where m is the 
#' number of months or years and s is the number of sites for a single trace. 
#' The user needs to specify the `time_step` and `flow_space`, although they are
#' assumed to be annual total if not specified. If `start_yearmon` is not 
#' specified, it is assumed to be January of the current year. The colnames of 
#' the matrix are used for the site names, if provided, otherwise they remain 
#' `NULL`.
#' 
#' *xts:* xts objects work the same way as matrices, except that `start_yearmon`
#' is determined form the xts object.
#' 
#' *data.frame:* data.frames can be converted to `nfd` objects
#' only if the variables ( i.e., column names) match specific variable names. 
#' In a "long" format, the data.frame must have `month`, `year`, `site`, 
#' `trace`, and `value` variables. In a "wide" format, the data.frame must have
#' `month`, `year`, and at least one other column. In the wide format, the other
#' column names are assumed to be site names, and there is assumed to be only
#' 1 trace of data. When using `as_nfd()` the different components of an `nfd`
#' object are guessed based on the data format. using `nfd()` uses the defaults
#' if not specified, but will produce warnings and errors if the data seem to 
#' reflect different components. For example if the data are annual and the 
#' `year` argument is specified as "wy", then warnings will post if the `month` 
#' variable contains values that are different than September. 
#' 
#' *list:* lists are treated the same way as data.frames. An error will post if
#' the list cannot first be converted to a data.frame.
#' 
#' All other data types will result in an error. 
#' 
#' @param data If `NA` or of length 1, creates an object with dimensions based
#'   on `n_months`, `n_trace`, `flow_space`, and `time_step`. Otherwise, creates 
#'   an object based on the provided data. Data should be a matrix, array, or 
#'   `[xts]` object. 
#'   
#' @param start_yearmon Start year and month of data. If `NA`, then assumes the
#'   data begins in January of the current year. Should be a [zoo::yearmon] 
#'   object or coercable to one. Scalar character.
#'   
#' @param n_months The number of months. Scalar numeric.
#' 
#' @param n_trace The number of traces. Scalar numeric.
#' 
#' @param n_sites The number of sites. Scalar numeric. When NA, the number of 
#'   sites will be solved for from the `data` shape. 
#'   
#' @param flow_space Data are intervening or total flow (or both). If both, then
#'   will store/create total and intervening flow data.
#'   
#' @param time_step Data are annual or monthly (or both). If both, then will 
#'   store/create annual and monthly data.
#'   
#' @param year Data are calendar year (`"cy"`) or water year (`"wy"`). This has
#'   implications for the timestep the annual data are stored in (December for 
#'   cy and September for wy) as well as how summation functions will be applied
#'   to monthly data.
#'   
#' @param site_names The names of the sites. If specified, must be the same 
#'   length as the number of sites (`n_sites`).
#'   
#' @return `nfd()` and `as_nfd()` return an object of class `nfd`. 
#' 
#' @export
nfd <- function(data = NA, n_months = NA, n_trace = 1, 
                n_sites = NA, flow_space = c("total", "intervening", "both"), 
                time_step = c("annual", "monthly", "both"), 
                start_yearmon = NA, year = c("cy", "wy"),
                site_names = NA
                )
{
  assert_that(length(start_yearmon) == 1)
  assert_that(length(n_months) == 1)
  assert_that(length(n_trace) == 1 && is.numeric(n_trace))
  assert_that(length(n_sites) == 1)
  flow_space <- match.arg(flow_space,  c("total", "intervening", "both"))
  time_step <- match.arg(time_step, c("annual", "monthly", "both"))
  year <- match.arg(year, c("cy", "wy"))
  
  if (is.na(start_yearmon) && !inherits(data, "xts")) {
    start_yearmon <- default_yearmon()
  } else {
    start_yearmon <- zoo::as.yearmon(start_yearmon)
  }
  
  if (isTRUE(is.na(data)) || (length(data) == 1 && is.numeric(data))) {
    if (is.na(n_months))
      n_months <- 1
    
    if (is.na(n_sites))
      n_sites <- 1
    
    if (!isTRUE(is.na(site_names)))
      assert_that(length(site_names) == n_sites)
    
    is_monthly <- time_step %in% c("monthly", "both")
    is_annual <- time_step %in% c("annual", "both")
    is_int <- flow_space %in% c("intervening", "both")
    is_tot <- flow_space %in% c("total", "both")
    
    # initialize an empty nfa/ or one with all the same values
    xts_mon <- xts_ann <- NULL
    if (is_monthly ) {
      xts_mon <- initialize_monthly_xts(
        data, start_yearmon, n_months, n_sites, site_names
      )
      # create a list with n_trace entries of xts_mon
      
      xts_mon <- lapply(seq(n_trace), function(x) xts_mon)
    }
    
    if (is_annual) {
      xts_ann <- initialize_annual_xts(
        data, start_yearmon, n_months, year, n_sites, site_names
      )
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
    
    x <- new_nfd(mon_int, mon_tot, ann_int, ann_tot, year)
    
  } else {
    x <- as_nfd(
      data, 
      start_yearmon = start_yearmon,
      n_months = n_months,
      n_trace = n_trace,
      n_sites = n_sites,
      flow_space = flow_space,
      time_step = time_step,
      year = year,
      site_names = site_names
    )
  }
  
  x
}

new_nfd <- function(mon_int, mon_tot, ann_int, ann_tot, year)
{
  x <- list(
    "annual" = list(
      "intervening" = ann_int,
      "total" = ann_tot
    ),
    "monthly" = list(
      "intervening" = mon_int,
      "total" = mon_tot
    )
  )
  
  x <- structure(x, class = c("nfd"))
  attr(x, "year") <- year
  
  x
}

#' @param x An `R` object.
#' @param ... Other parameters passed to `nfd()`.
#' @export
#' @rdname nfd
as_nfd <- function(x, ...)
{
  UseMethod("as_nfd")
}

as_nfd.default <- function(x, ...)
{
  stop("as_nfd() is not implemented for an object of class: ", class(x),
       "\nSee ?nfd for details.")
}

#' @export
as_nfd.array <- function(x, ...)
{
  # x[month, trace, site, (ann/int)]
  assert_that(length(dim(x)) %in% c(3, 4))
  assert_that(dim(x)[3] >= 1)

  # setup the variables that should be specified -----
  args <- list(...)
  n_trace <- dim(x)[2]
  ignore_arg("n_trace", args, n_trace)
  
  n_sites <- dim(x)[3]
  ignore_arg("n_sites", args, n_sites)
  
  # site names
  site_names <- dimnames(x)[[3]]
  if (is.null(site_names)) {
    site_names <- args[["site_names"]]
    if (is.null(site_names))
      site_names <- NA
  } else {
    ignore_arg("site_names", args, site_names)
  }
  
  if (!isTRUE(is.na(site_names))) 
    assert_that(length(site_names) == n_sites)
  
  # flow_space
  if (length(dim(x)) == 4) {
    flow_space <- "both"
    ignore_arg("flow_space", args, flow_space)
  } else {
    if (exists("flow_space", args)) {
      flow_space <- match.arg(
        args[["flow_space"]], 
        c("total", "intervening", "both")
      )
    } else {
      # preserves same behaviour if it is not specified in as_nfd() as if nfd()
      # is called with no flow_space argument
      flow_space <- "total"
    }
  }
  
  # time_step
  if (exists("time_step", args)) {
    time_step <- match.arg(args[["time_step"]], c("annual", "monthly", "both"))
  } else {
    time_step <- "annual"
  }
  
  # n_months
  n_months <- dim(x)[1]
  if (time_step == "annual")
    n_months <- n_months * 12
  ignore_arg("n_months", args, n_months)
  
  # year
  if (exists("year", args)) {
    year <- match.arg(args[["year"]], c("cy", "wy"))
  } else {
    year <- "cy"
  }
  
  # start_yearmon
  if (exists("start_yearmon", args)) {
    start_yearmon <- zoo::as.yearmon(args[["start_yearmon"]])
  } else {
    start_yearmon <- default_yearmon()
  }
  
  is_monthly <- time_step %in% c("monthly", "both")
  is_annual <- time_step %in% c("annual", "both")
  is_int <- flow_space %in% c("intervening", "both")
  is_tot <- flow_space %in% c("total", "both")
  
  # create xts data ---------
  mon_int <- mon_tot <- ann_int <- ann_tot <- NULL
  if (flow_space == "both") {
    d1 <- lapply(seq(n_trace), function(n) {
      tmp <- x[ , n, , 1]
      if (is_annual) {
        tmp <- initialize_annual_xts(
          tmp, start_yearmon, n_months, year, n_sites, site_names
        )
      } else {
        tmp <- initialize_monthly_xts(
          tmp, start_yearmon, n_months, n_sites, site_names
        )
      }
    })
    
    d2 <- lapply(seq(n_trace), function(n) {
      tmp <- x[ , n, , 2]
      if (is_annual) {
        tmp <- initialize_annual_xts(
          tmp, start_yearmon, n_months, year, n_sites, site_names
        )
      } else {
        tmp <- initialize_monthly_xts(
          tmp, start_yearmon, n_months, n_sites, site_names
        )
      }
    })
    
    if (is_annual) {
      ann_tot <- d1
      ann_int <- d2
    } else {
      mon_tot <- d1
      mon_int <- d2
    }
    
  } else {
    d1 <- lapply(seq(n_trace), function(n) {
      tmp <- x[ , n, ]
      if (is_annual) {
        tmp <- initialize_annual_xts(
          tmp, start_yearmon, n_months, year, n_sites, site_names
        )
      } else {
        tmp <- initialize_monthly_xts(
          tmp, start_yearmon, n_months, n_sites, site_names
        )
      }
    })
    
    # determine which variable d1 corresponds to.
    if (is_annual && is_tot)
      ann_tot <- d1
    else if (is_monthly && is_tot)
      mon_tot <- d1
    else if (is_annual && is_int)
      ann_int <- d1
    else if (is_monthly && is_int)
      mon_int <- d1
  }
  
  new_nfd(mon_int, mon_tot, ann_int, ann_tot, year)
}

#' @export
as_nfd.matrix <- function(x, ...)
{
  # setup the variables that should be specified -----
  args <- list(...)
  n_trace <- 1
  ignore_arg("n_trace", args, 1)
  
  # flow_space
  if (exists("flow_space", args)) {
    flow_space <- match.arg(
      args[["flow_space"]], 
      c("total", "intervening", "both")
    )
    assert_that(
      flow_space != "both", 
      msg = "flow_space cannot be both for a matrix or xts object."
    )
  } else {
    # preserves same behaviour if it is not specified in as_nfd() as if nfd()
    # is called with no flow_space argument
    flow_space <- "total"
  }
  
  # time_step
  if (exists("time_step", args)) {
    time_step <- match.arg(args[["time_step"]], c("annual", "monthly", "both"))
    assert_that(
      time_step != "both", 
      msg = "time_step cannot be both for a matrix or xts object."
    )
  } else {
    time_step <- "annual"
  }
  
  # n_months
  n_months <- nrow(x)
  if (time_step == "annual")
    n_months <- n_months * 12
  ignore_arg("n_months", args, n_months)
 
  n_sites <- ncol(x)
  ignore_arg("n_sites", args, n_sites)
  
  # site names
  site_names <- dimnames(x)[[2]]
  if (is.null(site_names)) {
    site_names <- args[["site_names"]]
    if (is.null(site_names))
      site_names <- NA
  } else {
    ignore_arg("site_names", args, site_names)
  }
  
  if (!isTRUE(is.na(site_names))) 
    assert_that(length(site_names) == n_sites)
  
  # year
  if (exists("year", args)) {
    year <- match.arg(args[["year"]], c("cy", "wy"))
  } else {
    year <- "cy"
  }
  
  # start_yearmon
  if (exists("start_yearmon", args)) {
    start_yearmon <- zoo::as.yearmon(args[["start_yearmon"]])
  } else {
    start_yearmon <- default_yearmon()
  }
  
  is_monthly <- time_step %in% c("monthly", "both")
  is_annual <- time_step %in% c("annual", "both")
  is_int <- flow_space %in% c("intervening", "both")
  is_tot <- flow_space %in% c("total", "both")
  
  # recreate xts data ----
  # redundant, but ensures the indexes are correct, and the colnames get set
  if (is_annual) {
    d1 <- list(initialize_annual_xts(
      x, start_yearmon, n_months, year, n_sites, site_names
    ))
  } else {
    d1 <- list(initialize_monthly_xts(
      x, start_yearmon, n_months, n_sites, site_names
    ))
  }
  
  # set all list entries ---
  mon_int <- mon_tot <- ann_int <- ann_tot <- NULL
  if (is_annual && is_tot)
    ann_tot <- d1
  else if (is_monthly && is_tot)
    mon_tot <- d1
  else if (is_annual && is_int)
    ann_int <- d1
  else if (is_monthly && is_int)
    mon_int <- d1

  new_nfd(mon_int, mon_tot, ann_int, ann_tot, year)
}

#' @export
as_nfd.xts <- function(x, ...) {
  # start_yearmon
  # cast as yearmon in case the index is daily or smaller. 
  start_yearmon <- zoo::as.yearmon(zoo::index(x)[1])
  args <- list(...)
  ignore_arg("start_yearmon", args, start_yearmon)
  
  as_nfd.matrix(zoo::coredata(x), start_yearmon = start_yearmon, ...)
}

#' @export
as_nfd.crss_nf <- function(x, ...) {
  class(x) <- "nfd"
  x
}

#' @export
as_nfd.data.frame <- function(x, ...) {
  long_cols <- c("year", "month", "site", "trace", "value")
  assert_that(
    all(c("year", "month") %in% colnames(x)), 
    msg = "data.frame must have year and month columns to be converted to nfd object."
  )
  
  # determine if the data.frame is in long or wide format
  if (all(long_cols %in% colnames(x)) && all(colnames(x) %in% long_cols)) {
    cat("Converting 'long' data.frame to nfd.\n")
  } else {
    # assume data frame is in wide format. assume all columns that are not 
    # year or month are sites
    all_sites <- colnames(x)
    all_sites <- all_sites[!(all_sites %in% c("year", "month"))]
    cat("Converting 'wide' data.frame to nfd.\n")
    cat(
      "Assuming the following columns are different sites:\n", 
      paste(all_sites, collapse = ", ")
    )
    cat('\n')
    
    # convert to long format
    x <- tidyr::pivot_longer(x, -c("year", "month"), names_to = "site") %>%
      dplyr::mutate(trace = 1)
  }
  
  # check format of month column. 
  x <- check_df_month_col(x)
  
  params <- list(...)
  
  # flow_space --------------------
  if (!exists("flow_space", params) || is.na(params$flow_space)) {
    stop("`flow_space` must be specified.")
  } else {
    flow_space <- params$flow_space
  }
  
  # yearly or monthly ----------------
  mm <- unique(x$month)
  if (!exists("time_step", params) || is.na(params$time_step)) {
    if (length(mm) == 1 && (mm == 12 || mm == 9)) {
      time_step <- "annual"
      tmp_yt <- ifelse(mm == 12, "cy", "wy")
    } else if (all(1:12 %in% mm)) {
      time_step <- "monthly"
      tmp_yt <- min(x$year)
      tmp_yt <- min(dplyr::filter(x, year == tmp_yt)$month)
      tmp_yt <- ifelse(tmp_yt == 1, "cy", ifelse(tmp_yt == 10, "wy", NA))
    } else {
      stop(paste(
        "Cannot determine if data are yearly or monthly.", 
        "Either explicitly provide this using the `time_step` parameter,",
        "or ensure that the data.frame contains correct months' data.",
        sep = "\n"
      ))
    }
    cat(
      "Guessing that data.frame contains ", time_step, " data.\n", 
      "If this is not correct, please try nfd() or as_nfd() with time_step specified."
    )
  } else {
    time_step <- params$time_step
    if (time_step == "annual") {
      assert_that(
        length(mm) == 1 && (mm == 12 || mm == 9),
        msg = "time_step is specified as annual data, so it should only have December or September months."
      )
      tmp_yt <- ifelse(mm == 12, "cy", "wy")
    } else if (time_step == "monthly") {
      assert_that(
        all(1:12 %in% mm),
        msg = "All months should be found in data.frame when time_step is monthly."
      )
      tmp_yt <- min(x$year)
      tmp_yt <- min(dplyr::filter(x, year == tmp_yt)$month)
      tmp_yt <- ifelse(tmp_yt == 1, "cy", ifelse(tmp_yt == 10, "wy", NA))
    } else {
      stop("timestep = 'both' is too ambiguous for converting data.frames to nfd objects.")
    }
  }
  
  # if yearly wy or cy -------------------------
  if (!exists("year", params) || is.na(params$year)) {
    year_type <- tmp_yt
    cat(
      "Guessing that data.frame is on ", year_type, " basis.",
      "If this is not correct, please try nfd() or as_nfd() with year specified."
    )
  } else {
    year <- params$year
    if (tmp_yt != year) {
      warning(
        "data seems like it is on ", tmp_yt, 
        " basis. However, year is specified as ", year, 
        ".\n Check data and/or year specification."
      )
    }
    year_type <- year
  }

  # create list of xts objects from data frame ------------------
  tt <- unique(x$trace)
  xts_list <- lapply(tt, function(i) {
    tmp <- dplyr::filter(x, trace == i)
    tmp$time_step <- zoo::as.yearmon(paste0(tmp$year, "-", tmp$month))
    tmp$year <- NULL
    tmp$month <- NULL
    tmp$trace <- NULL
    tmp <- tidyr::pivot_wider(tmp, names_from = "site")
    ts <- tmp$time_step
    tmp$time_step <- NULL
    
    xts::xts(tmp, ts)
  })
  
  # creat nfd object with data
  is_monthly <- time_step == "monthly"
  is_annual <- time_step == "annual"
  is_int <- flow_space == "intervening"
  is_tot <- flow_space == "total"
  
  # create xts data ---------
  mon_int <- mon_tot <- ann_int <- ann_tot <- NULL
  if (is_monthly && is_int) {
    mon_int <- xts_list
  }
  
  if (is_monthly && is_tot) {
    mon_tot <- xts_list
  }
  
  if (is_annual && is_int) {
    ann_int <- xts_list
  }
  
  if (is_annual && is_tot) {
    ann_tot <- xts_list
  }
  
  new_nfd(mon_int, mon_tot, ann_int, ann_tot, year = year_type)
}

check_df_month_col <- function(x) {
  if (is.numeric(x$month)) {
    assert_that(
      all(x$month %in% 1:12),
      msg = "month numbers must be in the range [1-12]."
    )
  } else if (is.character(x$month)) {
    assert_that(
      all(x$month %in% month.name), 
      msg = "All months should be full month names found in `month.name`."
    )
    # convert to numbers
    x$month <- match(x$month, month.name)
  } else {
    stop("month column should be either a numeric or a character.")
  }
  
  x
}

#' @export
as_nfd.list <- function(x, ...) {
  as_nfd(as.data.frame(x), ...)
}

# Ignore the specified arg if it esists in args. Will post message that `used`
# is being used instead.
ignore_arg <- function(arg, args, used)
{
  if (exists(arg, where = args) && !is.na(args[[arg]]) && used != args[[arg]]) {
    warning(paste0(
      "User specified ", arg, 
      " will be ignored as it is inferred from data shape.\n",
      "Using ", used, " instead."
    ))
  }
}

initialize_monthly_xts <- function(val, start_month, n_time_steps, n_sites, 
                                   site_names)
{
  x <- matrix(val, nrow = n_time_steps, ncol = n_sites)
  if (!isTRUE(is.na(site_names)))
    colnames(x) <- site_names
  
  ym <- start_month + (0:(n_time_steps - 1)) / 12
  
  xts::xts(x, order.by = ym)
}

initialize_annual_xts <- function(val, start_month, n_months, year_type, 
                                  n_sites, site_names)
{
  n_years <- max(floor(n_months / 12), 1)
  
  mon <- c("cy" = "12", "wy" = "09")
  mon <- mon[[year_type]]
  
  # if the start_month is OND and year_type wy, then the year should be
  # shifted forward by 1 year. 
  if (year_type == "wy" && month(start_month) %in% c("10", "11", "12"))
    year_seq <- seq(1, n_years)
  else
    year_seq <- seq(0, n_years - 1)
  
  # creates a "yyyy-mm" string, casts to yearmon, and then creates all
  # remaining years.
  ym <- zoo::as.yearmon(paste(year(start_month), mon, sep = "-")) + 
    year_seq
  
  x <- matrix(val, nrow = n_years, ncol = n_sites)
  
  if (!isTRUE(is.na(site_names)))
    colnames(x) <- site_names
  
  xts::xts(x, order.by = ym)
}

year <- function(x, numeric = FALSE)
{
  x <- format(x, "%Y")
  if (numeric)
    x <- as.numeric(x)
  
  x
}

month <- function(x, numeric = FALSE)
{
  x <- format(x, "%m")
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
  
  # check that all data either have the same number of traces, or have 0 traces
  assert_that(all(
    (n1 == max_n || n1 == 0), (n2 == max_n || n2 == 0), 
    (n3 == max_n || n3 == 0), (n4 == max_n || n4 == 0)
  ))
  
  max_n
}

n_sites <- function(x)
{
  assert_that(is_nfd(x))
  
  if (has_monthly(x)) {
    if (has_intervening(x, "monthly"))
      s <- dim(x[["monthly"]][["intervening"]][[1]])[2]
    else 
      s <- dim(x[["monthly"]][["total"]][[1]])[2]
  } else if (has_annual(x)) {
    if (has_intervening(x, "annual"))
      s <- dim(x[["annual"]][["intervening"]][[1]])[2]
    else
      s <- dim(x[["annual"]][["total"]][[1]])[2]
  } else {
    stop("nfd does not appear to have annual or monthly data.\n", 
        "Cannot determine number of sites.")
  }
  
  s
}

sites <- function(x)
{
  assert_that(is_nfd(x))
  
  if (has_monthly(x)) {
    if (has_intervening(x, "monthly"))
      s <- colnames(x[["monthly"]][["intervening"]][[1]])
    else 
      s <- colnames(x[["monthly"]][["total"]][[1]])
  } else if (has_annual(x)) {
    if (has_intervening(x, "annual"))
      s <- colnames(x[["annual"]][["intervening"]][[1]])
    else
      s <- colnames(x[["annual"]][["total"]][[1]])
  } else {
    stop("nfd does not appear to have annual or monthly data.\n", 
         "Cannot determine site names.")
  }
  
  s
}

n_ts <- function(x, ts_func, ts_type)
{
  assert_that(is_nfd(x))
  
  if (ts_func(x)) {
    val = c(-Inf, -Inf)
    if (has_intervening(x, ts_type))
      val[1] <- nrow(x[[ts_type]]$intervening[[1]])
    if (has_total(x, ts_type))
      val[2] <- nrow(x[[ts_type]]$total[[1]])
    
    val <- val[val != -Inf]
    assert_that(length(val) > 0 && all(val[1] %in% val))
    val <- val[1]
    
  } else
    val = -Inf
  
  val
}

n_years <- function(x)
{
  n_ts(x, has_annual, "annual")
}

n_months <- function(x)
{
  n_ts(x, has_monthly, "monthly")
}

#' @rdname nfd
#' @return `is_nfd()` returns `TRUE` if class inherits from `nfd`.
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
  
  flow_space <- paste(c(ann_int, ann_tot, mon_int, mon_tot), collapse = "\n - ")
  
  cat(
    "nfd: Natural Flow Data\n",
    "----------------------\n",
    "n traces:", n_trace(x), "\n",
    "dates:", as.character(start(x)), "-", as.character(end(x)), "\n",
    "flow space:\n -", flow_space
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
      y <- utils::tail(zoo::index(x[["monthly"]][["intervening"]][[1]]), 1)
    } else {
      y <- utils::tail(zoo::index(x[["monthly"]][["total"]][[1]]), 1)
    }
  } else {
    if (has_intervening(x, "annual")) {
      y <- utils::tail(zoo::index(x[["annual"]][["intervening"]][[1]]), 1)
    } else {
      y <- utils::tail(zoo::index(x[["annual"]][["total"]][[1]]), 1)
    }
  }
  
  y
}
