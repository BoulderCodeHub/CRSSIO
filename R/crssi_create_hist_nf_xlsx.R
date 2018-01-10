#' Create the HistoricalNaturalFlows.xlsx file
#' 
#' `crssi_create_hist_nf_xlsx()` creates the HistoricalNaturalFlows.xlsx that
#' CRSS relies on. This file needed to be manually updated before each new 
#' model start date before; this function takes care of the manual update.
#' 
#' HistoricalNaturalFlows.xlsx will contain monthly total natural flow above Lees 
#' Ferry for the entire historical natural flow record that exists. It will also 
#' contain monthly intevening natural flow for all gages at and below Hoover.
#' For these gages, data are appended from the end of the natural flow record
#' through the year before the CRSS model run starts (`modelStartYear`). 
#' The appended data use an
#' n-year average (`nYearAvg`) of the available historical natural flow. For
#' example, if the CRSS run begins in January 2019 and historical natural flow 
#' exists from 1906 - 2015, then January 2016 - December 2018 will be filled 
#' with the monthly average for 2011-2015 (if `nYearAvg == 5`).
#' 
#' The \code{\link[CoRiverNF]{CoRiverNF}} package is relied on for the 
#' historical natural flows. 
#' 
#' The filename defaults to HistoricalNaturalFlows.xlsx, but can be modified by
#' changing the `crssio.histNfFile` option.
#' 
#' @param modelStartYear The year the CRSS model run should begin.
#' @param nYearAvg The number of years to average when filling the intervening
#'   natural flow data.
#' @param oFolder The location to save the xlsx file. 
#' 
#' @return The filename that is created is invisibly returned. The function 
#'   creates the HistoricalNaturalFlows.xlsx file, which contains three worksheets:
#'   README, Intervening Natural Flow, Total Natural Flow. 
#'
#' @examples 
#' # create the file for a CRSS model run that begins in January 2019 and uses
#' # a 10-year average
#' \dontrun{
#' crssi_create_hist_nf_xlsx(2019, nYearAvg = 10)
#' }
#'    
#' @export

crssi_create_hist_nf_xlsx <- function(modelStartYear, nYearAvg = 5, oFolder = ".")
{
  # Lees Ferry total natural flow -----------------------
  lf <- nf_xts_to_df(CoRiverNF::monthlyTot, "LeesFerry") %>%
    tidyr::unite_("month", from = c("month", "year"), sep = "/1/") %>%
    dplyr::select_at(c("month", "LeesFerry")) %>%
    dplyr::rename_at(
      "LeesFerry", 
      function(x) "HistoricalNaturalFlow.AboveLeesFerry"
    )
  
  # LB nodes intervening natural flow -------------------
  lbSites <- c("Hoover", "Davis", "Alamo", "Parker", "Imperial")
  
  lb <- nf_xts_to_df(CoRiverNF::monthlyInt, lbSites)
  
  # fill the necessary years with avg data -------------------
  fillBegin <- max(lb$year) + 1
  fillEnd <- modelStartYear - 1
  # don't have to create fill data if the nf data extends up through or past
  # the beginning of the model run
  if (fillEnd >= fillBegin){
    # call get_monthly_average_by_site for all sites
    t2 <- lapply(
      lbSites, 
      function(site) get_monthly_average_by_site(lb, site, nYearAvg)
    ) %>%
      Reduce(function(dtf1, dtf2) dplyr::full_join(dtf1, dtf2, by = "month"), .)

        
    # for all the fill years, use t2, create the tmp year and bind it to lb
    fillYrs <- seq(fillBegin, fillEnd)
    
    for (tmpYr in fillYrs) {
      lb <- dplyr::bind_rows(
        lb, 
        t2 %>%
          tibble::add_column("year" = tmpYr) %>%
          dplyr::select_at(.vars = colnames(lb))
      )
    }
  }
  
  # prepare lb for formatting
  lb <- lb %>%
    dplyr::arrange_at(c("year", "month")) %>%
    tidyr::unite_("month", from = c("month", "year"), sep = "/1/") %>%
    dplyr::select_at(c("month", lbSites)) %>%
    dplyr::rename_at(
      lbSites, 
      .funs = dplyr::funs(paste("HistoricalNaturalFlow", ., sep = "."))
    )
  
  # write out the file -----------------------------
  oList <- list(
    "README" = get_hist_nf_readme(modelStartYear, nYearAvg),
    "Intervening Natural Flow" = lb,
    "Total Natural Flow" = lf
  )
  
  otxt <- writexl::write_xlsx(
    oList, 
    path = file.path(oFolder, getOption("crssio.histNfFile"))
  )
  invisible(otxt)
}

#' Convert natural flow xts data to data frame
#' 
#' `nf_xts_to_df` takes in the natural flow xts objects from CoRiverNF and 
#' converts them to a data frame with `"year"` and `"month"` columns. The function
#' also filters by the natural flow gage names (`nfGages`). The natural flow 
#' gages are left as columns (variables).
#' 
#' @param x The natural flow data as an xts matrix. Likely from the CoRiverNF 
#'   package.
#' @param nfGages A vector of strings that will limit the natural flow gages
#'   returned by this function.
#' 
#' @return A data frame with `"year"` and `"month"` columns, as well as columns for
#'   all of the natural flow gages specified by `nfGages`
#' 
#' @keywords internal
#' @noRd

nf_xts_to_df <- function(x, nfGages = nfShortNames())
{
  x %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "ym") %>%
    dplyr::select_at(.vars = c("ym", nfGages)) %>%
    dplyr::mutate_at(.vars = "ym", .funs = zoo::as.yearmon) %>%
    dplyr::mutate_at(.vars = "ym", .funs = dplyr::funs(
      "year" = as.numeric(format(., "%Y")),
      "month" = as.numeric(format(., "%m"))
    )) %>%
    dplyr::select(-dplyr::matches("ym"))
}

get_monthly_average_by_site <- function(x, site, nYearAvg)
{
  maxYear <- max(x$year)
  yrKeep <- seq(maxYear - nYearAvg + 1, maxYear)
  
  x %>%
    dplyr::select_at(.vars = c(site, "year", "month")) %>%
    dplyr::filter_at("year", dplyr::all_vars(. %in% yrKeep)) %>%
    tidyr::spread_("month", site) %>%
    dplyr::arrange_at("year") %>%
    dplyr::select(-dplyr::matches("year")) %>%
    dplyr::summarise_all(.funs = dplyr::funs(round(mean(.), 0))) %>%
    tidyr::gather_("month", site, as.character(1:12)) %>%
    dplyr::mutate_at("month", as.numeric)
}

get_hist_nf_readme <- function(modelStartYear, nYearAvg)
{
  yrs <- as.numeric(format(zoo::index(CoRiverNF::wyAnnTot), "%Y"))
  l1 <- paste0("- From the ", min(yrs), "-", max(yrs), 
               " Natural Flow record using the CoRiverNF package v", 
               utils::packageVersion("CoRiverNF"))
  
  fillBegin <- max(yrs) + 1
  fillEnd <- modelStartYear - 1
  
  if (fillEnd < fillBegin) {
    l2 <- "- No years of intervening flow are filled with average data"
  } else {
    if (fillEnd == fillBegin) {
      fillYrs <- fillEnd
    } else {
      fillYrs <- paste(fillBegin, fillEnd, sep = "-")
    }
    avgYrs <- paste(max(yrs) - nYearAvg + 1, max(yrs), sep = "-")
    
    l2 <- paste("-", fillYrs, "intervening natural flows are filled using the",
                avgYrs, "average")
  }
  
  data.frame("README" = c(l1, l2))
}
