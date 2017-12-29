
#' @keywords internal
#' @noRd

format_and_write <- function(traceData, headerTxt, oFile)
{
  traceData <- as.matrix(traceData)
  colnames(traceData) <- headerTxt
  # writes out to the same folder it reads in from
  utils::write.table(traceData, file = oFile, quote = FALSE, row.names = FALSE)
  invisible(traceData)
}

#' @keywords internal
#' @noRd

nc_to_single_trace <- function(nc, tt, oFolder, startYear, endYear, oFiles, progressBar)
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
  
  lapply(
    nfSites, 
    function(x) format_and_write(
      traceZoo[, x], 
      headerTxt = headerInfo,
      oFile = file.path(traceFolder, oFiles[x])
    )
  )
  setTxtProgressBar(progressBar, tt)
}

create_crss_cmip_nf_files <- function(iFile, 
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
    stop(iFile, " is not a netcdf.")
  
  if (!dir.exists(oFolder))
    stop(oFolder, " folder does not exist.", "\n",
         "Create the directory before calling create_crss_cmip_nf_files()")
  
  # check that all folders are empty. If they are empty, then proceed no matter
  # what; if they are not empty, then check the overwrite argument. if overwrite
  # is false the throw error, if overwrite is true, then proceed
  if (length(list.files(oFolder, recursive = TRUE)) != 0 & !overwriteFiles){
    stop("Trace files exist in ", oFolder, "\n",
         "Choose a different folder, delete the files, or use overwriteFiles = TRUE")
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
  
  lapply(tt, function(x) nc_to_single_trace(
    nc, x, oFolder, startYear, endYear, oFiles, processProgress
  ))
  
  invisible(iFile)
}
  