#' CRSS Input Class (crssi)
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
  if (drop_flow)
  {
    flow = new_nfd(
      flow$monthly$intervening, NULL, NULL, NULL, attr(flow, "year")
    )
    flow <- crss_nf(flow)
  }
  
  # compute the overlapping years of data, and trim to those overlapping years
  overlap <- find_overlap_years(flow_time, sac_time)
  sac_year_type <- sac_year_type[paste0(overlap[1],"/",overlap[2])]
  
  flow <- nfd_extract(flow, paste0(overlap[1], "-01/", overlap[2], "-12"))
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
