#' CRSS Input Class (crssi)
#' 
#' `crssi()` constructs a `crssi` object that holds all of the necessary 
#' trace-based input data for CRSS. Namely, this includes the intervening, 
#' monthly natural flow for all 29 sites, the Sacremento Year Type index, 
#' and a scenario number.
#' 
#' `crssi()` inherits from [crss_nf], maintaining the same required structure 
#' for the intervening natural flows. The object also contains the Sacremento 
#' Year Type index, and a scenario number. Given this, all functions that work
#' on [crss_nf] and [nfd] objects work on `crssi` objects.
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
#' @param sac_year_type An annual xts object with all timesteps having a 
#'   December-some year timestep. The number of columns in this object must 
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
#' @seealso [crss_nf], [nfd]
#' 
#' @export
crssi <- function(flow, sac_year_type, scen_number, scen_name = NULL, 
                  drop_flow = TRUE)
{
  assert_that(is_crss_nf(flow))
  assert_that(is.xts(sac_year_type))
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
  overlap <- find_overlap_years(flow_time, sac_time)
  sac_year_type <- sac_year_type[paste0(overlap[1],"/",overlap[2])]
  
  flow <- nfd_extract(flow, paste0(overlap[1], "-01/", overlap[2], "-12"))
  crss_nf_validate(flow)
  
  # add on to flow list structure
  flow[["sac_year_type"]] <- sac_year_type
  flow[["n_trace"]] <- n_trace
  flow[["scen_number"]] <- scen_number
  
  if (!is.null(scen_name))
    flow[["scen_name"]] <- scen_name
  
  class(flow) <- c("crssi", class(flow))
  
  flow
}

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
  cat(
    "crssi: CRSS Input Data\n",
    "----------------------\n"
  )
  
  if (!is.null(x$scen_name)) {
    cat(
      x$scen_name, "scenario\n"
    )
  }
    
  cat(
    "n traces:", x$n_trace, "\n",
    "dates:", as.character(start(x)), "-", as.character(end(x)), "\n"
  )
  
  invisible(x)
}

# find the January y1 - December y2 that exist given the dates
# from both flow and sac_year_type
find_overlap_years <- function(flow_time, sac_time)
{
  # start:
  start_flow <- min(flow_time)
  # if the start month isn't january, then increment to january of the next year
  if (month(start_flow, TRUE) != 1)
    start_flow <- zoo::as.yearmon(paste("Jan", year(start_flow, TRUE) + 1))
  
  start_sac <- min(sac_time)
  if (year(start_sac, TRUE) <= year(start_flow, TRUE))
    start_year <- year(start_flow)
  else
    start_year <- year(start_sac)
  
  # end
  end_flow <- max(flow_time)
  # if the end month is not December, then decrment to december of the previous
  # year
  if (month(end_flow, TRUE) != 12)
    end_flow <- zoo::as.yearmon(paste("Dec", year(end_flow, TRUE) - 1))
  
  end_sac <- max(sac_time)
  if (year(end_sac, TRUE) >= year(end_flow, TRUE))
    end_year <- year(end_flow)
  else
    end_year <- year(end_sac)

  assert_that(
    all(
      c(
        zoo::as.yearmon(paste("Jan", start_year)), 
        zoo::as.yearmon(paste("Dec", end_year))
      ) %in% flow_time
    ) && 
    all(
      c(
        zoo::as.yearmon(paste("Dec", start_year)), 
        zoo::as.yearmon(paste("Dec", end_year))
      ) %in% sac_time
    ),
    msg = "A full year of overlapping times does not exist in `flow` and `sac_year_type`"
  )

  assert_that(
    as.numeric(end_year) >= as.numeric(start_year),
    msg = "A full year of overlapping times does not exist in `flow` and `sac_year_type`"
  )
  
  c(start_year, end_year)
}
