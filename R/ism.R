#' Apply the Index Sequential Method
#' 
#' `ism()` applies the index sequential method to one site (column) of data. 
#' 
#' @param x An R object. `ism()` is implemented for matrices, [xts], [nfd], 
#'   [crss_nf], and [crssi] objects.
#'   
#' @param n_years_keep The number of years of data to keep. This must be <= the
#'   number of years of data that exists in `x`. 
#'   
#' @param ... Further arguments passed to subsequent methods.
#' 
#' @export
ism <- function(x, n_years_keep = NA, ...)
{
  UseMethod("ism")
}

#' @export
#' @rdname ism
ism.crssi <- function(x, ...)
{
  sac_year_type <- x[["sac_year_type"]]
  scen_number <- x[["scen_number"]]
  scen_name <- x[["scen_name"]]
  
  args <- list(...)
  if (exists("is_monthly", where = args))
    stop("is_monthly should not be passed to crssi as it contains monthly and annual data.")
  
  x <- suppressMessages(as_crss_nf(x))
  x <- ism.crss_nf(x, ...)
  sac_year_type <- ism(sac_year_type, ...)
  
  # rbuild crssi
  x[["sac_year_type"]] <- sac_year_type
  x[["n_trace"]] <- n_trace(x)
  x[["scen_number"]] <- scen_number
  
  if (!is.null(scen_name))
    x[["scen_name"]] <- scen_name
  
  class(x) <- c("crssi", class(x))
  crssi_validate(x)
}

#' @export
#' @rdname ism
ism.crss_nf <- function(x, ...)
{
  x <- ism.nfd(x, ...)
  class(x) <- c("crss_nf", "nfd")
  crss_nf_validate(x)
  x
}

#' @export
#' @rdname ism
ism.nfd <- function(x, ...)
{
  # check that n_trace == 1; ISM does not really make sense otherwise
  assert_that(
    n_trace(x) == 1, 
    msg = paste0("x has ", n_trace(x), " traces of data.\n", 
                 "Cannot apply ISM to more than 1 trace.")
  )
  
  # check that years and months overlap, strictly, if monthly and annual data
  # exist
  if (has_annual(x) && has_monthly(x)) {
    assert_that(
      has_overlapping_ts(x), 
      msg = paste0(
        "x has annual and monthly data that do not overlap 'exactly'.\n",
        "Try using `nfd_trim()` first."
      )
    )
    
    args <- list(...)
    if (exists("is_monthly", args))
      stop("is_monthly should not be passed to ism when the object contains annual and monthly data.")
  }
  
  # for each flow_space/time_step, get the ism matrix
  mon_int <- nfd_ism(x, "intervening", "monthly", ...)
  mon_tot <- nfd_ism(x, "total", "monthly", ...)
  ann_int <- nfd_ism(x, "intervening", "annual", ...)
  ann_tot <- nfd_ism(x, "total", "annual", ...)
  
  # take all flow_space/time_step matrices and create a new nfd_object
  new_nfd(mon_int, mon_tot, ann_int, ann_tot, attr(x, "year"))
}

#' @export
#' @rdname ism
ism.xts <- function(x, ...)
{
  assert_that(
    ncol(x) == 1, 
    msg = "ism() can only be applied to one column of data."
  )
  
  args <- list(...)
  n_years_keep <- args[["n_years_keep"]]
  is_monthly <- args[["is_monthly"]]
  
  if (is.null(is_monthly) || isTRUE(is.na(is_monthly))) {
    is_monthly <- TRUE
    # if the number of rows is the same as the number of years, it is annual 
    # data
    if (xts::periodicity(x)[["scale"]] == "yearly") {
      is_monthly <- FALSE
    }
  }
  
  if (is.null(n_years_keep) || isTRUE(is.na(n_years_keep))) {
    if (is_monthly)
      n_years_keep <- nrow(x) / 12
    else 
      n_years_keep <- nrow(x)
  } else {
    if (is_monthly)
      tmp <- nrow(x) / 12
    else 
      tmp <- nrow(x)
    assert_that(
      n_years_keep <= tmp,
      msg = '`n_years_keep` is more than the number of years in `x`'
    )
  }
  
  # make the data not an xts object so we can call ism.matrix
  x_mat <- zoo::coredata(x)
  
  x_mat <- ism(x_mat, is_monthly, n_years_keep)
  
  # now convert back to xts object with monthly time step
  ism_xts <- xts::xts(x_mat, order.by = zoo::index(x)[1:nrow(x_mat)])
    
  ism_xts
}

#' @param is_monthly Boolean. `TRUE` if data are monthly. `FALSE` if data are 
#'   annual.
#'   
#' @export
#' @rdname ism
ism.matrix <- function(x, is_monthly, n_years_keep = NA)
{
  assert_that(
    ncol(x) == 1, 
    msg = "ism() can only be applied to one column of data."
  )
  
  if (is_monthly) {
    assert_that(
      nrow(x) %% 12 == 0, 
      msg = "Matrix must include full years of data, i.e., divisible by 12."
    )
    assert_that(
      nrow(x) >= 24,
      msg = "Must have at least 2 years of data to apply ISM."
    )
    
    if (is.na(n_years_keep)) {
      n_years_keep <- nrow(x) / 12
    } else {
      assert_that(n_years_keep <= nrow(x) / 12)
    }
    
    num_traces <- nrow(x) / 12
  } else {
    assert_that(
      nrow(x) >= 2,
      msg = "Must have at least 2 years of data to apply ISM."
    )
    if (is.na(n_years_keep)) {
      n_years_keep <- nrow(x)
    } else {
      assert_that(n_years_keep <= nrow(x))
    }
    num_traces <- nrow(x)
  }
  
  zz <- rbind(x, x) # now can easily loop through the data for ISM
  
  ism_matrix <- simplify2array(
    lapply(seq(num_traces), getSubsetOfData, zz, n_years_keep, is_monthly)
  )
  
  ism_matrix
}

# returns subset of a data
# assumes monthly data
getSubsetOfData <- function(startYear, zz, nYrs, monthly)
{
  if(monthly){
    startI <- startYear * 12 - 11
    zz <- zz[startI:(nYrs * 12 + startI - 1)]
  } else{
    # annual data
    zz <- zz[startYear:(nYrs + startYear - 1)]
  }
  
  zz
}

nfd_ism <- function(x, flow_space, time_step, ...)
{
  # grab trace 1 data
  # loop/apply by column, ism.xts
  # this will return a matrix that is time x traces
  tmp <- tryCatch(
    nfd_get_trace(x, 1, flow_space, time_step), 
    error = function(e) NULL, 
    finally = NULL
  )
  if (is.null(tmp)) return(NULL)
    
  # list. Each entry is 1 site. Each matrix is time x trace
  y <- lapply(seq(n_sites(x)), function(i) {
    ism(tmp[,i], ...)
  })
  
  # need to convert to list where each entry is a trace and each matrix
  # is time x site
  # ncol gets the number of traces after ism is applied
  y <- lapply(seq(ncol(y[[1]])), function(nt) {
    site_mat <- do.call(cbind, lapply(seq(n_sites(x)), function(ns) {
      y[[ns]][,nt]
    }))
    colnames(site_mat) <- sites(x)
    site_mat
  })
  
  y
}
