
get_cc_readme_vals <- function(nc, iFile, startYear, endYear)
{
  simYrs <- endYear - startYear + 1
  startDate <- paste0(startYear, "-01-31")
  fName <- basename(iFile)
  
  vNum <- ncdf4::ncatt_get(nc, varid = 0, attname = "version")
  cmip <- ncdf4::ncatt_get(nc, varid = 0, attname = "cmip")
  ds <- ncdf4::ncatt_get(nc, varid = 0, attname = "downscale_method")
  bc2 <- ncdf4::ncatt_get(nc, varid = 0, attname = "secondary_bias_correction")
  rm <- ncdf4::ncatt_get(nc, varid = 0, attname = "routing_method")
  
  stopifnot(vNum$hasatt, cmip$hasatt, ds$hasatt, bc2$hasatt, rm$hasatt)
  
  list(
    intro = paste0(
      "Created from Downscaled GCM Projected data (", 
      paste(cmip$value, ds$value, bc2$value, 
            paste("and", rm$value), sep = ", "),
      ")"
    ),
    simYrs = simYrs,
    periodToUse = paste0(startYear, "-", endYear),
    startDate = startDate,
    createFrom = paste0(basename(iFile), " v", vNum$value)
  )
}

#' Read a single trace of data from netcdf file, and call function to write data
#' 
#' `write_nc_single_trace()` reads a single trace of data from the netcdf file, 
#'   formats the data as a `zoo` object, subsets the data based on `startYear` 
#'   and `endYear`, and writes the data to the correct natural flow file with
#'   the appropriate header text.
#'   
#' @param nc An object of class `ncdf4` that contains natural flow data.
#' @param tt A numeric specifying which trace of data will be selected. 
#' @param oFolder Path to the top level directory where the trace folders and
#'   input files will be created.
#' @param startYear The year to start the trace files in. Data will be trimmed 
#'   to start in this year. 
#' @param endYear The final year of data the trace files will contain.
#' @param oFiles The CRSS natural inflow file names to use for the individual
#'   traces files.
#' @param progressBar An object of class `txtProgressBar` that will be updated.
#' 
#' @return 
#' `nc` is returned invisibly because the function writes out 29 natural flow 
#'   files. 
#' 
#' @keywords internal
#' @noRd

write_nc_single_trace <- function(nc, 
                                  tt, 
                                  oFolder, 
                                  startYear, 
                                  endYear, 
                                  oFiles, progressBar)
{
  if (nc$var$natural_flow$dim[[1]]$name != "trace")
    stop("The netcdf file is not formated as expected.", "\n",
         "We expect the naturalFlow variable's first dimension to be 'trace'.")
  
  traceFolder <- file.path(oFolder, paste0("trace", tt))
  
  # create the trace folder if it does not exist
  if (!dir.exists(traceFolder))
    dir.create(traceFolder)
  
  # get the initial start month of the data
  mm <- zoo::as.yearmon(
    strsplit(nc$dim$time$units, "months since ")[[1]][2]
  )
  # and convert to zoo::yearmon series based on time dimension
  mm <- mm + ncdf4::ncvar_get(nc, "time")/12
  
  # get the single trace data
  traceData <- ncdf4::ncvar_get(nc, "natural_flow")[tt,,]
  # now each row is a gage, and the columns are the timesteps
  
  # and now create a zoo object of the trace data
  traceZoo <- zoo::zoo(t(traceData), mm)
  # and trim the zoo object based on startYear and endYear
  traceZoo <- traceZoo[get_yearmon_series(startYear, endYear),]
  
  # get the headerInfo for the trace file
  headerInfo <- get_trace_file_header(
    startYear, 
    units = nc$var$naturalFlow$units
  )
    
  nfSites <- ncdf4::ncvar_get(nc, "gage_number")
  
  # write out each site's natural flow data to the correct file name
  lapply(
    nfSites, 
    function(x) {
      utils::write.table(
        matrix(traceZoo[, x], ncol = 1, dimnames = list(NULL, headerInfo)),
        file = file.path(traceFolder, oFiles[x]),
        quote = FALSE,
        row.names = FALSE
      )
    }
  )
  
  # get the sacramento year type data and write it out -------------------
  sac <- t(ncdf4::ncvar_get(nc, varid = "sac_yt")) # t([trace, year])
  year <- ncdf4::ncvar_get(nc, varid = "year")
  # select the data to keep
  sac <- sac[year %in% seq.int(startYear, endYear), , drop = FALSE]
  startDate <- paste0(startYear, "-12-31")
  writeSacYT(tt, ytData = sac, startDate = startDate, folderPath = oFolder)
    
  utils::setTxtProgressBar(progressBar, tt)
  
  invisible(nc)
}

#' Create CRSS natural flow input files from CMIP data
#' 
#' `crssi_create_cmip_nf_files()` creates individual trace files for CRSS from
#'   a netcdf file containing CMIP based natural flow data. 
#' 
#' @details 
#' `crssi_create_cmip_nf_files()` will create individual trace files named by 
#' the `oFiles` argument for all traces that exist in `iFile`. Individual trace 
#' folders, e.g., trace1, trace2, traceN, are created for all traces found in 
#' `iFile`. `iFile` should be a netcdf file that contains a variable called 
#' `naturalFlow`. If it does not, then the function will error. The netcdf file 
#' should contain a 3-dimensional array in (trace, gageNumber, time) format. 
#' 
#' The CMIP data currently exists for 1950 - 2099. If the user specifies years 
#' outside of this range, the function will abort. Because CRSS needs data to
#' start in the year that the CRSS simulations begin, this function trims the
#' data based on `startYear` and `endYear`, and correctly formats the trace 
#' files to begin in January of `startYear` and end in December of `endYear`.
#' 
#' `scenarioNumber` is specified by the user and is saved to the slot specified
#' by the `"crssio.supplyScenarioSlot"` option. The following maps supply 
#' scenario names to the scenario numbers that should be used here. (See the 
#' CRSS model documentation for more details on the scenario number.) This 
#' function will error if the `scenarioNumber` provided is 1, 2, or 3 as these
#' data are not created with this function. 
#' 
#' - 1 = Observed Resampled
#' - 2 = Direct Paleo Resampled
#' - 3 = Paleo-conditioned
#' - 4 = CMIP3 Downscaled GCM Projected
#' - 5 = CMIP5 Downscaled GCM Projected, BCSD downscaling, quantile mapping 
#' secondary bias correction
#' 
#' In addition to the scenario number, the trace number, the hydrology 
#' increment, and the Sacramento Year Type index are created for each trace. 
#' The names of these slots are controlled by the `"crssio.traceNumberSlot"`,
#' `"crssio.hydroIncrement"`, and `"crssio.sacYTSlot"` options, respectively. 
#' The Sacramento Year Type index is stored in the netcdf file with the natural
#' flow files, and is created from a regression of a river gage in the 
#' Sacramento Basin. See the CRSS documentation for more details. Finally, 
#' a README file is created in `oFolder` that provides some metadata on the 
#' creation of the trace files.
#' 
#' `oFiles` sets the individual file names for the natural inflow locations. If
#' you do not use [nf_file_names()], oFiles should contain 29 strings:
#' one for each of the natural inflow locations, and should be specified in the
#' default order corresponding to the gages in [nf_gage_names()]. 
#' 
#' `overwriteFiles` allows the user to control whether existing files within the
#' trace folders should be overwritten (default is they are not). 
#' 
#' @param iFile Path to netcdf file containing the CMIP based natural inflow
#'   data. See 'Details'.
#' @param oFolder Path to the top level directory where the trace folders and
#'   input files will be created. This folder should exist before using this
#'   function.
#' @param startYear The year to start the trace files in. Data will be trimmed 
#'   to start in this year. 
#' @param endYear The final year of data the trace files will contain.
#' @param scenarioNumber The scenario number used as an identifier in CRSS. 
#'   See 'Details.'
#' @param oFiles The CRSS natural inflow file names to use for the individual
#'   traces files.
#' @param overwriteFiles A boolean that determines whether or not the function
#'   should overwrite existing files. See 'Details'.
#' 
#' @return 
#' `iFile` is invisibly returned as the main purpose of the function is to write
#' many files. 
#' 
#' @examples 
#' \dontrun{
#' crssi_create_cmip_nf_files(
#'   "cmip5_bcsd.nc",
#'   oFolder = "c:/model/CRSS/dmi/cmip5",
#'   startYear = 2018,
#'   endYear = 2060,
#'   overwriteFiles = TRUE
#' )
#' }
#' 
#' @export

crssi_create_cmip_nf_files <- function(iFile, 
                                      oFolder, 
                                      startYear, 
                                      endYear = 2060, 
                                      scenarioNumber,
                                      oFiles = nf_file_names(),
                                      overwriteFiles = FALSE)
{
  # check arguments are valid ---------------
  if (!file.exists(iFile))
    stop("iFile does not exist")
  
  if (tools::file_ext(iFile) != "nc")
    stop(iFile, " does not appear to be a netcdf file.")
  
  check_nf_oFolder(oFolder, overwriteFiles, "crssi_create_cmip_nf_files")
  
  if (endYear < startYear)
    stop("EndYear cannot be before startYear.")
  
  if (startYear < 1950)
    stop("startYear should not be before 1950")
  
  if (endYear > 2099)
    stop("endYear should not be after 2099")
  
  if (scenarioNumber %in% c(1:3))
    stop("Invalid scenario number for climate change natural flow files.")
  
  # get the data from the netcdf file
  nc <- ncdf4::nc_open(iFile, write = FALSE)
  on.exit(ncdf4::nc_close(nc))
  
  # loop over each trace and write out all of the locations
  tt <- ncdf4::ncvar_get(nc, "trace")
  simYrs <- endYear - startYear + 1
  startDate <- paste0(startYear, "-1-31")
  
  message("Starting to create files. Creating ", max(tt), " traces of data...")
  processProgress <- utils::txtProgressBar(min = 0, max = max(tt) , style = 3)
  
  lapply(
    tt, 
    function(x){
      # write out the nf data
      write_nc_single_trace(
        nc, x, oFolder, startYear, endYear, oFiles, processProgress
      )
      # write out the supply scenario and trace files
      writeTraceSupplyNumbers(x, scenarioNumber, oFolder)
      # save the hydrologyIncrement data
      writeHydroIncrement(x, simYrs, startDate, oFolder)
    } 
  )
  
  # create the README
  vals <- get_cc_readme_vals(nc, iFile, startYear, endYear)
  write_nf_readme(vals, oFolder = oFolder)
  
  invisible(iFile)
}
  