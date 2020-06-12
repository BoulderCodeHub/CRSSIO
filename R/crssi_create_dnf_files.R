#' Create CRSS Natural Flow Input Files From Historical Record
#' 
#' `crssi_create_dnf_files()` creates the natural flow input files used by the 
#' Colorado River Simulation System (CRSS) by applying the 
#' [Index Sequential Method](http://onlinelibrary.wiley.com/doi/10.1111/j.1752-1688.1997.tb03557.x/abstract)
#' (ISM) to the historical natural flow record (or a subset of the historical 
#' record). `crssi_create_dnf_files()` is a convenience function that wraps 
#' the process of getting/formatting data ([crss_nf()] + [sac_year_type_get()] + 
#' [crssi()]), applying ISM ([ism()]), changing the dates to CRSS' future start
#' year ([reindex()]), and then writing out the files ([write_crssi()]). 
#' 
#' **Input Data:** The input data can be read from the natural flow workbook 
#' available at 
#' \url{http://www.usbr.gov/lc/region/g4000/NaturalFlow/current.html} 
#' or from the [CoRiverNF](https://github.com/BoulderCodeHub/CoRiverNF) data 
#' package. It is much faster to use the data package rather than the Excel 
#' workbook, and the files created from the two sources will match identically 
#' if they are from the same natural flow release.
#' 
#' **Other Files:** In addition to creating the natural flow input files, data 
#' for two four other slots are also created. See [write_crssi()].  
#' 
#' **Scenario Number:** The scenario number in `crssi` objects provides the user 
#' with a numeric representation of which supply scenario is used. 
#' The observed historical natural flows (used by this 
#' function) are supply scenario 1. For this supply scenario, the decimals of 
#' the supply scenario number represent the start and end year that the ISM 
#' method are applied to. For example, if you set `recordToUse` to 
#' `c('1988-1','2012-12')`, the decimal portion will be 19882012, where the
#' first 4 numbers represent the start year and the second four numbers 
#' represent the end year. The supply scenario slot will be set to 1.19882012 in 
#' this example. This tells the user of CRSS that the  supply scenario is the 
#' observed historical natural flows with the ISM method applied to the 
#' 1988-2012 data. 
#' 
#' @param iFile Either the string `"CoRiverNF"``, or the relative or absolute 
#'   path to the excel workbook. When "CoRiverNF" is used, the data from the 
#'   [CoRiverNF](https://github.com/BoulderCodeHub/CoRiverNF) data package is 
#'   used. Otherwise, it should be a valid path to the natural flow Excel 
#'   workbook. 
#'   
#' @param oFolder Path to the top level directory where the trace folders and
#'   input files will be created. This folder should exist before using this
#'   function.
#'   
#' @param startYear The year to start the trace files in. Data will be trimmed 
#'   to start in this year.
#'   
#' @param endYear The final year of data the trace files will contain.
#' 
#' @param oFiles A matrix of the file names (input into CRSS). The default uses
#'   [nf_file_names()]. This must be specified in the correct 
#'   order, i.e., the same order as the nodes in the input Excel file.
#'   
#' @param recordToUse The start and end dates of the natural flow record to 
#'   perform ISM, if using something besides the full record. If it is `NA`, the 
#'   full record will be used. Otherwise, it should be a vector of length 2, 
#'   where the first entry is the start date and the second entry is the end 
#'   date. The vector should be of type [zoo::yearmon], and begin in January of
#'   some year, and end in December of some year.
#'   
#' @param overwriteFiles Boolean. Should existing files be overwritten.
#' 
#' @return Invisibly returns `crssi` object.
#' 
#' @examples
#' 
#' \dontrun{
#' # will create 20 years for 25 traces based on the 1988-2012 record:
#' crssi_create_dnf_files("CoRiverNF", 
#'   "tmp", 
#'   startYear = 2017, 
#'   endYear = 2036, 
#'   recordToUse = zoo::as.yearmon(c('1988-1','2012-12'))
#' )
#' 
#' # path to excel file
#' iFile <- 'user/docs/NaturalFlows1906-2012_withExtensions_1.8.15.xlsx'
#' # will create 50 years for 107 traces based on the full (1906-2012) record:
#' crssi_create_dnf_files(iFile, 
#'   'NFSinput/', 
#'   startYear = 2015, 
#'   endYear = 2064
#' )
#' 
#' # crssi_create_dnf_files() is a convenient wrapper around other functions
#' # in CRSSIO. The following should produce the same output. This would create
#' # the "stress test hydrology", which applies ISM to the 1988-present 
#' # hydrology
#' # wrapper: 
#' crssi_create_dnf_files(
#'   "CoRiverNF", 
#'   "folder1/", 
#'   startYear = 2020, 
#'   endYear = 2024, 
#'   recordToUse = zoo::as.yearmon(c('1988-1','2018-12'))
#' )
#' 
#' # individual steps:
#' # get data
#' flow <- CoRiverNF::monthlyInt["1988/2018"]
#' sac_yt <- sac_year_type_get()["1988/2018"]
#'
#' # create crssi object
#' nf <- crssi(crss_nf(flow), sac_yt, scen_number = 1.19882018)
#' 
#' # apply ism
#' nf <- ism(nf, n_years_keep = 5)
#' 
#' # change times to start in 2020
#' nf <- reindex(nf, 2020)
#' 
#' # write the files
#' write_crssi(nf, "folder2/")
#' 
#' }
#' @seealso [nf_file_names()], [crssi()], [write_crssi()] 
#' 
#' @export
crssi_create_dnf_files <- function(iFile, 
                                   oFolder, 
                                   startYear, 
                                   endYear, 
                                   oFiles = nf_file_names(),
                                   recordToUse = NA,
                                   overwriteFiles = FALSE)
{
  if (tools::file_ext(iFile) != "xlsx" & iFile != "CoRiverNF")
    stop(iFile, " does not appear to be valid.\n", 
      "It should be either an Excel (xlsx) file or 'CoRiverNF'")
  
  check_nf_oFolder(oFolder, overwriteFiles, "crssi_create_dnf_files")
  
  if (!anyNA(recordToUse))
    recordToUse_str <- check_recordToUse(recordToUse)
  
  # get nf data ------------------------------------------
  if (iFile == 'CoRiverNF') {
    # use the data in CoRiverNF::monthlyInt
    nf <- CoRiverNF::monthlyInt
    if (!anyNA(recordToUse)) {
      # if not NA, then trim data, otherwise use full data
      check_recordToUse_year2(recordToUse[2], nf)
      nf <- nf[paste(recordToUse_str[1], recordToUse_str[2],sep = '/')]
    } else {
      nf <- nf['1906-01/'] # trim off OND 1905
    }
  } else {
    # use the data in the Excel workbook, if it exists.
    if (!file.exists(iFile)) {
      stop('iFile does not exist')
    }
  
    nf <- read_and_format_nf_excel(iFile)

    if (!anyNA(recordToUse)) {
      check_recordToUse_year2(recordToUse[2], nf)
      # trim data
      nf <- nf[paste(recordToUse_str[1], recordToUse_str[2],sep = '/')]
    }
  }
  
  # convert nf to crss_nf
  nf <- crss_nf(nf)
  
  # set scenario number --------------------
  # get the years used before changing nf
  if (!anyNA(recordToUse)) {
    y1 <- year(recordToUse[1], numeric = TRUE)
    y2 <- year(recordToUse[2], numeric = TRUE)
    periodToUse <- paste0(y1, '-', y2)
    # this only deals with historical observed NF, so that is supply scenario 
    # 1.xxxxxxxx, where the .xxxxxxxx are the beginning and ending years used
    # for ISM
    supplyScenario <- as.numeric(paste0(1,'.',y1,y2))
  } else {
    # uses the full record, so it's 1906 - some year. figure out some year
    y1 <- 1906
    y2 <- year(end(nf), numeric = TRUE)
    periodToUse <- paste0(y1, '-', y2)
    supplyScenario <- as.numeric(paste0('1.1906',y2))
  }
  
  simYrs <- endYear - startYear + 1
  assert_that(
    simYrs <= (y2 - y1 + 1), 
    msg = "endYear-startYear+1 should be <= the length speciifed in recordToUse."
  )
  
  # get sac_yt_data -------------------------------------
  sac_yt <- sac_year_type_get()
  # trim to correct years
  if (!anyNA(recordToUse)) {
    check_recordToUse_year2(recordToUse[2], sac_yt)
    # trim data
    sac_yt <- sac_yt[paste(recordToUse_str[1], recordToUse_str[2], sep = '/')]
  }
  
  # create crssi ----------------------------------------
  scen_name <- paste0("ISM applied to ", y1, "-", y2, " historical hydrology.")
  nf <- crssi(nf, sac_yt, supplyScenario, scen_name)
  
  # reindex to start in specified year ------------------
  nf <- reindex(nf, startYear)
  
  # ism and trim ----------------------------------------
  nf <- ism(nf, n_years_keep = simYrs)
  
  # save files ------------------------------------------
  write_crssi(nf, path = oFolder, overwrite = overwriteFiles, readme = FALSE)
	
	# create the README
	write_nf_readme(
	  get_dnf_readme_vals(iFile, startYear, endYear, periodToUse), 
	  oFolder = oFolder
	)
	
	invisible(nf)
}
