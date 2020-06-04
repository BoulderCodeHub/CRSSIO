#' Create CRSS Input Trace Files
#' 
#' `write_crssi()` creates the required CRSS input files from a [crssi] object.
#' In addition to the files for the 29 natural flow inputs (specified by 
#' `file_names`), four additional files are written for each trace. See 
#' *Details* for a description of these slots.
#' 
#' The four additional slots are:
#' 
#' - **Trace number:** The trace numbers are intuitive, i.e., they are integers 
#' from 1 to N where N is the trace number.
#' - **Scenario number:** The scenario number provides the user with a numeric 
#' representation of which supply scenario is used. See the 
#' *Scenario Numbering Convention* section in [crssi] or the package's 
#' [wiki](https://github.com/BoulderCodeHub/CRSSIO/wiki/Scenario-Numbering-Convention) 
#' for details on the scenario number.
#' - **"Hydrology increment" number:** The hydrology increment data sets the random number 
#' generator in CRSS. This seed value for each year and trace is created for 
#' each trace folder. 
#' - **Sacramento year type index:** The Sacramento Year type index for each year
#' and trace (stored in the `crssi` object.) See [crssi].
#' 
#' See [CRSSIO] for a description of the package options that determine these 
#' slots' file names.
#' 
#' @param x [crssi] object.
#' 
#' @param path Directory to save files to.
#' 
#' @param file_names The file names that will be used for each natural flow 
#'   site. See [nf_file_names()].
#' 
#' @param overwrite Boolean. If `TRUE` and files already exists in 
#'   `path`/traceN, the files will be overwritten.
#'   
#' @param readme Boolean. If `TRUE` also create a README file in `path`.
#' 
#' @export
write_crssi <- function(x, path, file_names = nf_file_names(), 
                        overwrite = FALSE, readme = TRUE)
{
  assert_that(is_crssi(x))
  check_nf_oFolder(path, overwrite, "crssi_create_dnf_files")
  assert_that(length(file_names) == 29)
  
  startYear <- year(start(x), numeric = TRUE)
  endYear <- year(end(x), numeric = TRUE)
  startDate <- paste0(startYear, "-01-31")
  simYrs <- endYear - startYear + 1

  # number of traces
  nT <- n_trace(x) 

  headerInfo <- get_trace_file_header(startYear)
  
  # create all traceN folders
  lapply(
    1:nT,
    function(x){
      fold <- paste(path, '/trace', x, sep = '')
      if(!dir.exists(fold))
        dir.create(fold)
    }
  )
  
  # for each node, write out all of the trace files
  lapply(
    1:29, 
    function(s) writeNFFilesByNode(
      nfd_get_site(x, s, "intervening", "monthly"), 
      file_names[s], path, headerInfo
    )
  )
  
  # get sacramento year type ism data ----------
  eoyDate <- paste0(startYear, "-12-31")
  ytData <- x[["sac_year_type"]]
  
  scen_num <- x[["scen_number"]]
  
  # write out additional trace files ----------
  message("Beginning to write additional trace files")
  lapply(1:nT, function(i) {
    # write out all of the trace and supply scenario number files
    writeTraceSupplyNumbers(i, scen_num, path)
    
    # write out the hydrology increment slot
    writeHydroIncrement(i, simYrs, startDate, path)
    
    # write out the sacramento year type file
    writeSacYT(i, ytData, eoyDate, path)
  })
  
  # create the README
  if (readme)
    write_nf_readme(
      get_crssi_readme_vals(x, startYear, endYear), 
      oFolder = path
    )
  
  invisible(x)
}

get_crssi_readme_vals <- function(x, startYear, endYear)
{
  simYrs <- endYear - startYear + 1
  startDate <- paste0(startYear, "-01-31")
  
  scen_name <- x[["scen_name"]]
  if (is.null(scen_name))
    scen_name <- "NULL"
  
  scen_number <- x[["scen_number"]]
  list(
    intro = paste0(
      "Created from crssi object.\n", 
      "scenario name: ", scen_name, "\n",
      "scenario number: ", scen_number, "\n"
    ),
    simYrs = simYrs,
    periodToUse = paste0(startYear, "-", endYear),
    startDate = startDate,
    createFrom = "crssi object using write_crssi()"
  )
}
