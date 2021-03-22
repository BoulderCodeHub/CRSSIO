#' CRSS Input Class (crssi)
#' 
#' `crssi()` constructs a `crssi` object that holds all of the necessary 
#' trace-based input data for CRSS. Namely, this includes the intervening, 
#' monthly natural flow for all 29 sites, the Sacremento Year Type index, 
#' and a scenario number.
#' 
#' `crssi()` inherits from [crss_nf], maintaining the same required structure 
#' for the intervening natural flows. The object also contains the Sacramento 
#' Year Type index, and a scenario number. Given this, all functions that work
#' on [crss_nf] and [nfd] objects work on `crssi` objects.
#' 
#' **Sacramento Year Type index:** Beginning in CRSS v2.6, input data for the 
#' Sacramento year type index are necessary. For historical values see
#' [sac_year_type_get()].
#' 
#' Overlapping years: `crssi()` checks to make sure that there at least some
#' overlappying yeras of data between `flow` and `sac_year_type`. It then trims
#' the data to be January, year1 - December, year2 for the overlapping period 
#' between `flow` and `sac_year_type`. For example, if `flow` contains data 
#' for March 2020 - December 2024 while `sac_year_type` contains data for 
#' December 2020 - December 2025, the returned object will contain monthly 
#' intervening flow for January 2021 - December 2024, and Sacremento year type
#' index values for December 2021 - December 2024.
#' 
#' @section Scenario Numbering Convention:
#' 
#' Scenario numbering can change faster than this package. For the latest 
#' numbering convention, check the package 
#' [wiki](https://github.com/BoulderCodeHub/CRSSIO/wiki/Scenario-Numbering-Convention).
#' 
#' The numbering convention uses the following for the ones place of the 
#' scenario number.
#' 
#' - 1 = Observed Resampled, i.e., ISM applied to the historical record.
#' - 2 = Direct Paleo Resampled
#' - 3 = Paleo-conditioned
#' - 4 = CMIP3 Downscaled GCM Projected- 
#' - 5 = CMIP5 Downscaled GCM Projected, BCSD downscaling, quantile mapping 
#' secondary bias correction
#' 
#' Then, for scenarios that use the observed resampled data, the decimal portion
#' should be set to reflect the years that ISM is applied to. For example, if 
#' you are using the 1988-2012 record, the decimal portion should be set to
#' 19882012, where the first 4 numbers represent the start year and the second 
#' four numbers represent the end year. Thus `scen_number` should be 1.19882012 
#' in this example. This tells the user of CRSS that the supply scenario is 
#' the observed historical natural flows with the ISM method applied to the 
#' 1988-2012 data. 
#' 
#' @param flow A `crss_nf` object.
#' 
#' @param sac_year_type An annual xts object with all time steps having a 
#'   December-some year time step. The number of columns in this object must 
#'   match the number of traces in `flow`. Additionally, there must be some
#'   overlapping years of data. See details.
#'   
#' @param scen_number The scenario number. See **Scenario Numbering Convention**
#'   section.
#'   
#' @param scen_name Optional. This is only used when printing in R to help the 
#' user quickly know what is stored in the `crssi` object.
#' 
#' @param drop_flow Boolean. If `TRUE`, and if `flow` contains flows other than
#'   monthly, intervening, these flows will be dropped. This is suggested as 
#'   the memory needed to store the object and the time needed to apply ISM to 
#'   the object will be drastically reduced. If `FALSE`, the other flows are 
#'   kept in the object.
#'   
#' @return `crssi()` returns an object of class `crssi`.
#' 
#' @examples 
#' 
#' # get natural flow and Sacremento Year Type data for 2000-2002
#' nf <- crss_nf(
#'   CoRiverNF::monthlyInt["2000/2002"],
#'   flow_space = "intervening",
#'   time_step = "monthly"
#' )
#' sac <- sac_year_type_get(internal = TRUE)["2000/2002"]
#' in_data <- crssi(nf, sac, scen_number = 1.20002002)
#' 
#' @seealso [crss_nf], [nfd], [write_crssi()], [sac_year_type_get()]
#' 
#' @export
crssi <- function(flow, sac_year_type, scen_number, scen_name = NULL, 
                  drop_flow = TRUE)
{
  assert_that(is_crss_nf(flow))
  assert_that(xts::is.xts(sac_year_type))
  assert_that(is.numeric(scen_number) && length(scen_number) == 1)
  
  nt <- n_trace(flow)
  
  assert_that(
    nt == ncol(sac_year_type),
    msg = paste0(
      "Number of traces for `flow` and `sac_year_type` should be the same.\n",
      "`flow` has: ", n_trace(flow), "\n",
      "`sac_year_type` has: ", ncol(sac_year_type)
    )
  )
  
  # sac_yt should only include December time steps
  sac_time <- zoo::index(sac_year_type)
  assert_that(
    all(format(sac_time, "%m") == "12"),
    msg = "`sac_year_type` should only include December timestep."
  )
  
  # check that there are at least some overlapping years of data
  flow_time <- zoo::index(nfd_get_trace(flow, 1, "intervening", "monthly"))
  assert_that(
    any(flow_time %in% sac_time), 
    msg = "`flow` and `sac_year_type` have no overlapping dates."
  )
  
  # if drop_flow == TRUE, delete the monthly total, and annual flow data from
  # flow
  if (drop_flow) {
    flow = new_nfd(
      flow$monthly$intervening, NULL, NULL, NULL, attr(flow, "year")
    )
    flow <- crss_nf(flow)
  }
  
  # compute the overlapping years of data, and trim to those overlapping years
  overlap <- find_overlap_years(flow_time, sac_time, "cy")
  sac_year_type <- sac_year_type[paste0(overlap[1],"/",overlap[2])]
  
  flow <- nfd_extract(flow, paste0(overlap[1], "-01/", overlap[2], "-12"))
  crss_nf_validate(flow)
  
  # add on to flow list structure
  flow[["sac_year_type"]] <- sac_year_type
  flow[["n_trace"]] <- nt
  flow[["scen_number"]] <- scen_number
  
  if (!is.null(scen_name))
    flow[["scen_name"]] <- scen_name
  
  class(flow) <- c("crssi", class(flow))
  attr(flow, "year") <- "cy"
  
  flow
}

#' @param x An object.
#' @export
#' @return `is_crssi()` returns `TRUE` if class inherits from `crssi`.
#' @rdname crssi
is_crssi <- function(x)
{
  inherits(x, "crssi")
}

#' @export
print.crssi <- function(x, ...)
{
  p <- paste0(
    "crssi: CRSS Input Data\n",
    "----------------------\n"
  )
  
  if (!is.null(x$scen_name)) {
    p <- paste0(p, 
      "scenario: ", x$scen_name, "\n" 
    )
  }
    
  p <- paste0(p, 
    "n traces: ", x$n_trace, "\n",
    "dates: ", as.character(start(x)), "-", as.character(end(x)), "\n"
  )
  
  cat(p)
  
  invisible(x)
}

crssi_validate <- function(x)
{
  crss_nf_validate(x)
  assert_that(!is.null(x[["n_trace"]]))
  assert_that(!is.null(x[["scen_number"]]))
  assert_that(xts::is.xts(x[["sac_year_type"]]))
  assert_that(end(x[["sac_year_type"]]) == end(x))
  assert_that(
    start(x) ==
      zoo::as.yearmon(paste0("Jan", year(start(x[["sac_year_type"]]))))
  )
  assert_that(
    x[["n_trace"]] == n_trace(x) && x[["n_trace"]] == ncol(x[["sac_year_type"]])
  )
  assert_that(attr(x, "year") == "cy")
  
  invisible(x)
}
