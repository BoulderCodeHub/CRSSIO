
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

write_nc_single_trace <- function(nc, tt, oFolder, startYear, endYear, oFiles, progressBar)
{
  if (nc$var$naturalFlow$dim[[1]]$name != "trace")
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
  traceData <- ncdf4::ncvar_get(nc, "naturalFlow")[tt,,]
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
    
  nfSites <- ncdf4::ncvar_get(nc, "gageNumber")
  
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
  
  setTxtProgressBar(progressBar, tt)
  
  invisible(nc)
}

#' Create CRSS natural flow input files from CMIP data
#' 
#' `crssi_create_cmip_nf_files()` creates individual trace files for CRSS from
#'   a netcdf file containing CMIP based natural flow data. 
#' 
#' @details 
#' `crssi_create_cmip_nf_files()` will create individual trace files named by the
#' `oFiles` argument for all traces that exist in `iFile`. Individual trace 
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
#' `oFiles` sets the individual file names for the natural inflow locations. If
#' you do not use `\link{CRSSNFInputNames}()`, oFiles should contain 29 strings:
#' one for each of the natural inflow locations, and should be specified in the
#' default order corresponding to the gages in `\link{nfGageNames}()`. 
#' 
#' `overwriteFiles` allows the user to control whether existing files within the
#' trace folders should be overwritten (default is they are not). 
#' 
#' @param iFile Path to netcdf file containing the CMIP based natural inflow
#'   data. See 'Details'.
#' @param oFolder Path to the top level directory where the trace folders and
#'   input files will be created.
#' @param startYear The year to start the trace files in. Data will be trimmed 
#'   to start in this year. 
#' @param endYear The final year of data the trace files will contain.
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
                                      oFiles = CRSSNFInputNames(),
                                      overwriteFiles = FALSE)
{
  # check arguments are valid ---------------
  if (!file.exists(iFile))
    stop("iFile does not exist")
  
  if (tools::file_ext(iFile) != "nc")
    stop(iFile, " does not appear to be a netcdf file.")
  
  if (!dir.exists(oFolder))
    stop(oFolder, " folder does not exist.", "\n",
         "Create the directory before calling create_crss_cmip_nf_files()")
  
  # check that all folders are empty. If they are empty, then proceed no matter
  # what; if they are not empty, then check the overwrite argument. if overwrite
  # is false the throw error, if overwrite is true, then proceed
  if (length(list.files(oFolder, recursive = TRUE)) != 0 & !overwriteFiles){
    stop("Trace files exist in ", oFolder, "\n",
         "  Choose a different folder, delete the files, or use overwriteFiles = TRUE")
  }
  
  if (endYear < startYear)
    stop("In trimCCNFFiles, endYear cannot be before startYear.")
  
  if (startYear < 1950)
    stop("startYear should not be before 1950")
  
  if (endYear > 2099)
    stop("endYear should not be after 2099")
  
  # get the data from the netcdf file
  nc <- ncdf4::nc_open(iFile, write = FALSE)
  on.exit(ncdf4::nc_close(nc))
  
  # loop over each trace and write out all of the locations
  tt <- ncdf4::ncvar_get(nc, "trace")
  
  message("Starting to create files. Creating ", max(tt), " traces of data...")
  processProgress <- utils::txtProgressBar(min = 0, max = max(tt) , style = 3)
  
  lapply(
    tt, 
    function(x) write_nc_single_trace(
      nc, x, oFolder, startYear, endYear, oFiles, processProgress
    )
  )
  
  invisible(iFile)
}
  