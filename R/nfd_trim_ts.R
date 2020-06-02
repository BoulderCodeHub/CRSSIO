# See reindex.R for documentation
#' @rdname nfd_time_helpers
#' @export
nfd_trim_ts <- function(x)
{
  UseMethod("nfd_trim_ts")
}

#' @export
nfd_trim_ts.nfd <- function(x)
{
  if (has_overlapping_ts(x))
    return(x)
  
  assert_that(
    has_overlapping_ts(x, exact = FALSE), 
    msg = "There are no overlapping years, so it is not possible to trim the object."
  )
  
  assert_that(
    has_annual(x) && has_monthly(x), 
    msg = "nfd does not have annual and monthly data, so nothing to trim."
  )
  
  year_type <- attr(x, "year")
  
  overlap <- find_overlap_years(nfd_mon_index(x), nfd_ann_index(x), year_type)
  start_end <- start_end_by_yt(year_type)
  
  x <- nfd_extract(
    x, 
    paste0(overlap[1], "-", start_end[1], "/", overlap[2], "-", start_end[2])
  )
  
  x
}

#' @export
nfd_trim_ts.crssi <- function(x)
{
  # 1 trim the nfd data
  x_crss_nf <- nfd_trim_ts(suppressMessages(as_crss_nf(x)))
  
  # 2 find overlap years with sac yt data
  overlap <- find_overlap_years(
    nfd_mon_index(x_crss_nf), 
    zoo::index(x$sac_year_type), 
    "cy"
  )
  
  # 3 trim the nfd data (again?) and the sac_yt data
  x_crss_nf <- nfd_extract(
    x_crss_nf, 
    paste0(overlap[1], "-01/", overlap[2], "-12")
  )
  
  sac_yt <- x$sac_year_type[paste(overlap, collapse = "/")]
  
  # 4 update the crssi data structure
  x[["sac_year_type"]] <- sac_yt
  if (has_annual(x))
    x$annual <- x_crss_nf$annual
  
  x$monthly <- x_crss_nf$monthly
  
  x
}

start_end_by_yt <- function(year_type, numeric = FALSE)
{
  sm <- c("wy" = "10", "cy" = "01")
  em <- c("wy" = "9", "cy" = "12")
  
  sm <- sm[year_type]
  em <- em[year_type]
  
  se <- c(sm, em)
  
  if (numeric) {
    se <- as.numeric(se)
  }
  
  se
}

find_overlap_years <- function(mon_ts, year_ts, year_type)
{
  se <- start_end_by_yt(year_type, TRUE)
  sm <- se[1]
  em <- se[2]

  # start:
  start_mon <- min(mon_ts)
  
  if (month(start_mon, TRUE) != sm) {
    if (year_type == "wy") {
      # if before october, then increment to october of the current year. 
      # if after october, then increment to october of the following year.
      if (month(start_mon, TRUE) <= 9)
        start_mon <- zoo::as.yearmon(paste("Oct", year(start_mon)))
      else
        start_mon <- zoo::as.yearmon(paste("Oct", year(start_mon, TRUE) + 1))
    } else
      # if the start month isn't january, then increment to january of the next year
      start_mon <- zoo::as.yearmon(paste("Jan", year(start_mon, TRUE) + 1))
  }
  
  start_ann <- min(year_ts)
  i <- ifelse(year_type == "wy", 1, 0)
  if (year(start_ann, TRUE) <= year(start_mon, TRUE) + i)
    start_year <- year(start_mon)
  else
    start_year <- year(start_ann, TRUE) - i
  
  # end:
  end_mon <- max(mon_ts)
  if (month(end_mon, TRUE) != em) {
      
    if (year_type == "wy") {
      if (month(end_mon, TRUE) < 9)
        # decrement to september previous year
        end_mon <- zoo::as.yearmon(paste("Sep", year(end_mon, TRUE) - 1))
      else
        # decrement to september current year
        end_mon <- zoo::as.yearmon(paste("Sep", year(end_mon)))
    } else
      # if the end month is not December, then decrment to december of the
      # previou year
      end_mon <- zoo::as.yearmon(paste("Dec", year(end_mon, TRUE) - 1))
  }
  
  end_ann <- max(year_ts)
  if (year(end_ann, TRUE) >= year(end_mon, TRUE))
    end_year <- year(end_mon)
  else
    end_year <- year(end_ann)
  
  assert_that(
    all(
      c(
        zoo::as.yearmon(paste0(start_year, "-", sm)), 
        zoo::as.yearmon(paste0(end_year, "-", em))
      ) %in% mon_ts
    ) && 
      all(
        c(
          zoo::as.yearmon(paste0(as.numeric(start_year) + i, "-", em)), 
          zoo::as.yearmon(paste0(end_year, "-", em))
        ) %in% year_ts
      ),
    msg = "A full year of overlapping times was not found."
  )
  
  assert_that(
    as.numeric(end_year) >= as.numeric(start_year),
    msg = "A full year of overlapping times was not found"
  )
  
  c(start_year, end_year)
}

nfd_ann_index <- function(x)
{
  assert_that(has_annual(x))
  
  if (has_total(x, "annual"))
    r <- zoo::index(nfd_get_trace(x, 1, "total", "annual"))
  else
    r <- zoo::index(nfd_get_trace(x, 1, "intervening", "annual"))
  
  r
}

nfd_mon_index <- function(x)
{
  assert_that(has_monthly(x))
  
  
  if (has_total(x, "monthly"))
    r <- zoo::index(nfd_get_trace(x, 1, "total", "monthly"))
  else
    r <- zoo::index(nfd_get_trace(x, 1, "intervening", "monthly"))
  
  r
}