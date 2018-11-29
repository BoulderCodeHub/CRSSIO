library(dplyr)
#' parseSACData reads and parses the Sacramento year type data
#' 
#' It also converts from letter codes to number codes:
#' 
#' Wet = W = 5
#' Above normal = AN = 4
#' Below normal = BN = 3
#' Dry = D = 2
#' Critically Dry = C = 1
#' 
#' @param iFile the file downloaded from http://cdec.water.ca.gov/cgi-progs/iodir/WSIHIST 
#' @param histYrs The historical years to use
#' 
#' @example 
#' sacYT <- parseSACData("data-raw/sacrementoData.txt", 1906:2017)
#' devtools::use_data(sacYT, internal = TRUE, overwrite = TRUE)

parseSACData <- function(iFile, histYrs)
{
  zz <- matrix(scan(iFile, what = 'character', skip = 11), ncol = 11, byrow = T)
  
  cn <- scan(iFile, what = 'character', skip = 4, nlines = 1)
  
  # drop off the San Joequin Valley data
  zz <- zz[,1:6]
  cn <- cn[1:6]
  
  # add colnames
  colnames(zz) <- cn
  
  zz <- as.data.frame(zz)
  
  zz$WY <- as.numeric(as.character(zz$WY))
  
  # trim to only the years you need
  zz <- filter(zz, WY %in% histYrs)
  
  # convert the yr-type from a letter to a number
  zz$`Yr-type` <- as.character(zz$`Yr-type`)
  
  yrType <- c('W' = 5, 'AN' = 4, 'BN' = 3, 'D' = 2, 'C' = 1)
  
  zz <- zz %>% dplyr::mutate(YrTypeNum = yrType[`Yr-type`]) %>%
    select(WY, YrTypeNum) %>%
    mutate(WY = as.yearmon(paste("Dec", WY)))
  
  zz <- xts::xts(zz$YrTypeNum, zz$WY)
  colnames(zz) <- "YrTypeNum"
  
  zz
  
}