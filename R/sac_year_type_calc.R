#' Compute Sacramento Valley Water Year Index for CRSS
#' 
#' `sac_year_type_calc()` calculates the Sacramento Valley Water Year (WY) Index 
#' using either the Sacramento 4 river index water year volume, or Colorado 
#' River WY intervening natural flow data. **Note that these methods are 
#' implemented only for purposes of generating an index to use in CRSS.**
#' 
#' CRSS uses the Sacramento Valley Water Year Index to determine MWD's desired 
#' annual ICS activity. This method adds plausible variability to the CRSS 
#' projections based on MWD’s needs for storage or delivery of Colorado River
#' water based on the availability of their other California supplies. The index
#' is available for 1906-present (see [sac_year_type_get()]), which is used for
#' hydrology scenarios that rely on historical data. For other scenarios, e.g., 
#' paleo, or statistical generated, the index must be calculated from other 
#' available data.
#' 
#' If the Sacramento 4 river index water year (WY) volume is available (like it  
#' is going back to 900 C.E. based on paleo reconstructions), the WY volume can
#' be used to recreate the index values. In this case, the following formula is
#' used, which is derived from the official formula documented at
#' [http://cdec.water.ca.gov/cgi-progs/iodir/WSIHIST](http://cdec.water.ca.gov/cgi-progs/iodir/WSIHIST).
#' 
#' \deqn{index_t = 0.323 * RWY + 0.3 * min(index_{t-1}, 10.0)}
#' 
#' where `RWY` is the Sacramento 4 river WY volume **in million acre-feet**. 
#' Note that because the index
#' in the first year depends on the index value from the previous year, which is
#' unknown, the 0th index value is assumed to be an average year with an index
#' value of 7.8. 
#' 
#' When the Sacramento 4 river index WY volume is unknown, the index can be 
#' generated from the Colorado River natural flow data since these must always
#' be available for use in CRSS. The index is created based on a decision tree
#' that is fit to the water year intervening natural flow for all 29 sites in 
#' the Colorado River Basin. See XXXX for details on the development and tuning
#' of this decision tree.
#' 
#' @param wy_vol xts object containing Sacramento 4-river index water year 
#'   volumes in million acre-feet. xts object must have yearly periodicity. 
#'   
#' @param co_int_nf [nfd] object that has annual, intervening data and has the
#'   "wy" year type. It also must have 29 sites that are named the same as the
#'   expected site names in a [crss_nf] object. 
#'  
#' @return xts object. When using `wy_vol` will have the same number of columns
#'   as `wy_vol`. When using `co_int_nf` will have the same number of columns as
#'   there are traces in `co_int_nf`.
#'  
#' @export
sac_year_type_calc <- function(wy_vol = NULL, co_int_nf = NULL) {
  assert_that(
    xor(missing(wy_vol), missing(co_int_nf)),
    msg = "Only one of `wy_vol` or `co_int_nf` should be specified."
  )
  
  if (missing(co_int_nf)) {
    # use wy_vol. wy_vol should be xts object
    assert_that(
      xts::is.xts(wy_vol) && xts::periodicity(wy_vol)$label == "year",
      msg = "`wy_vol` must be a yearly xts object."
    )
    
    rr <- apply(wy_vol, 2, calc_yt_from_vol)
    rr <- xts::xts(rr, zoo::index(wy_vol))
    colnames(rr) <- colnames(wy_vol)
  } else {
    assert_that(
      is_nfd(co_int_nf) && has_intervening(co_int_nf) && 
        has_annual(co_int_nf) && attr(co_int_nf, "year") == "wy",
      msg = "`co_int_nf` must be a nfd object with annual (WY) intervening data."
    )
    
    assert_that(
      all(sites(co_int_nf) %in% nf_gage_abbrv()) && 
        all(nf_gage_abbrv() %in% sites(co_int_nf)), 
      msg = "`co_int_nf` must have named sites that match `nf_gage_abbrv()`"
    )

    rr <- calc_yt_from_co_nf(co_int_nf)
  }
  
  rr
}

calc_yt_from_vol <- function(x) {
  yt <- rep(NA, length(x))
  yt2 <- yt
  
  yt[1] <- 0.323 * x[1] + 0.3 * 7.8
  
  if (length(x) > 1) {
    for (i in 2:length(x)) {
      yt[i] <- 0.323 * x[i] + .3 * min(yt[i - 1], 10.0)
    }
  }
  
  yt2[yt >= 9.2] <- 5
  yt2[yt < 9.2 & yt > 7.8] <- 4
  yt2[yt <= 7.8 & yt > 6.5] <- 3
  yt2[yt <= 6.5 & yt > 5.4] <- 2
  yt2[yt <= 5.4] <- 1
  
  yt2
}

calc_yt_from_co_nf <- function(x) {
  dt <- sac_yt_dt()

  # for each trace in x, compute the sac year type based on the flow data
  sac_yt <- lapply(x$annual$intervening, function(nf) {
    as.numeric(stats::predict(dt, nf, type = "class"))
  })
  
  sac_yt <- do.call(cbind, sac_yt)
  sac_yt <- xts::xts(sac_yt, zoo::index(x$annual$intervening[[1]]))
  
  sac_yt
}

# fits a decision tree to co river intervening wy flow and sacramento year type
# data. Uses minsplit and minbucket parameters found using hyper parameter 
# tuning based on leave one out cross validation using both accuracy and f1 
# scores
sac_yt_dt <- function() {
  last_year <- 2019
  last_year <- paste0("/", last_year)
  best_minsplit <- 40
  best_maxdepth <- 8
  
  xx <- as.data.frame(CoRiverNF::wyAnnInt[last_year])
  sac <- as.data.frame(sac_year_type_get(TRUE)[last_year])
  
  df <- cbind(xx, sac)
  
  # fit the decision tree
  dt <- rpart::rpart(
    YrTypeNum~., data = df, method = "class", 
    control = rpart::rpart.control(
      minsplit = best_minsplit, minbucket = 1, maxdepth = best_maxdepth
    )
  )
  
  dt
}
