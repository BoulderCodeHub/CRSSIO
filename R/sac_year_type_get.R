#' Get Sacramento Year Type Index Data
#' 
#' `sac_year_type_get()` gets the historical record of Sacramento Year Type 
#' indexes, which are required input to CRSS. The function will download them
#' from http://cdec.water.ca.gov/cgi-progs/iodir/WSIHIST and then parse the
#' required information. If an internet connection is not available, then the
#' internal data can be used, but it may not be up-to-date. Additionally, the 
#' index can be obtained from paleo data. For the paleo data, the index is 
#' created from the Sacramento Valley 4 river index volume as reconstructed in
#' Meko et al. (2018). The WY volume is passed to [sac_year_type_calc()] to 
#' compute the index value.
#' 
#' The "crssio.sac_yt_url" option determines where the function downloads the 
#' data from.
#' 
#' @param internal Boolean. If `TRUE`, will use the internal data saved with 
#'   the package instead of downloading the data. 
#' 
#' @param paleo Boolean. If `TRUE`, then ignore `internal` and get the index
#'   from paleo data. 
#' 
#' @return `xts` object with 1 column.
#' 
#' @export
sac_year_type_get <- function(internal = FALSE, paleo = FALSE)
{
  if (paleo) {
    sac <- sac_year_type_calc(wy_vol = sac_paleo_wy_vol / 1000000)
    return(sac)
  }
  
  if (internal)
    return(sacYT)
  
  site <- getOption("crssio.sac_yt_url")
  
  is_error <- tryCatch(
    httr::http_error(site), 
    error = function(e) TRUE, 
    finally = NULL
  )
  
  assert_that(
    !is_error, 
    msg = paste0("Could not connect to ", site, 
                 "\nTry again, or try using `internal` = TRUE.")
  )
  
  site <- xml2::read_html(site)
  
  # the html is inside the body and pre tags
  txt <- site %>% 
    rvest::html_nodes("body") %>%
    rvest::html_nodes("pre") %>%
    rvest::html_text(trim = TRUE)
  
  # convert to one row per line
  t2 <- scan(text = txt, sep = "\n", what = character(), quiet = TRUE)
  
  # find the line that has Sacramento Valley and San Joequin Valley
  m1 <- grepl("Sacramento Valley", t2)
  m2 <- grepl("San Joaquin Valley", t2)
  mm <- m1 & m2
  # get first occurrence 
  l1 <- which(mm)[1]
  
  # find the line that has Eight River Runoff
  l2 <- which(grepl("Eight River Runoff", t2))
  
  t2 <- t2[l1:l2]
  
  # now find lines that contain ----------------- and min
  l1 <- which(grepl("--------------------", t2))
  l2 <- which(grepl("min", t2))
  
  # one line before l1 is the headers
  header <- t2[l1-1]
  header <- scan(text = header, sep = "", what = character(), quiet = TRUE)
  
  # trim to only contain data now
  t2 <- t2[(l1 + 1):(l2 - 1)]
  
  # now collapse and the split based on white space
  t3 <- scan(text = paste(t2, collapse = " "), what = character(), quiet = TRUE)
  
  # we know data starts in 1906
  t3 <- t3[which(t3 == "1906"):length(t3)]
  t3 <- matrix(t3, ncol = length(header), byrow = TRUE)
  
  yrs <- t3[,1]
  yrs <- zoo::as.yearmon(paste("Dec", yrs))
  
  # the first year type is the sacramento
  yt <- t3[,which(header == "Yr-type")[1]]
  
  # convet from letter to numeric
  yr_type <- c('W' = 5, 'AN' = 4, 'BN' = 3, 'D' = 2, 'C' = 1)
  yt <- yr_type[yt]
  
  sac_yt <- xts::xts(yt, order.by = yrs)
  colnames(sac_yt) <- "YrTypeNum"
  
  sac_yt
}
