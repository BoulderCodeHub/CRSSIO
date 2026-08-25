
#' @name CRSSIO-package
#' @rdname CRSSIO-package
#' @section Package Options:
#' The first four options dictate file names that are created by 
#' [crssi_create_dnf_files()] and [crssi_create_cmip_nf_files()]:
#' \describe{
#' \item{`crssio.supplyScenarioSlot`}{The file name (slot name) used for the 
#' supply scenario number. Default: `"HydrologyParameters.SupplyScenario"`}
#' \item{`crssio.traceNumberSlot`}{The file name (slot name) used for the 
#' trace number. Default: `"HydrologyParameters.TraceNumber"`}
#' \item{`crssio.hydroIncrement`}{The file name (slot name) used for the 
#' hydrology increment value. Default: 
#' `"MeadFloodControlData.hydrologyIncrement"`}
#' \item{`crssio.sacYTSlot`}{The file name (slot name) used for the 
#' Sacramento year type index. Default: `"MWD_ICS.SacWYType"`}
#' #' \item{`crssio.StVrainSlot`}{The file name (slot name) used for St. 
#' Vrain natural flow. Default: `"TMD_East_Slope_Supply.St_Vrain_Annual_Flow"`}
#' }
#' 
#' Additional options for file I/O:
#' \describe{
#' \item{`crssio.histNfFile`}{The file name that will be created by
#' `crssi_create_hist_nf_xlsx()`. Default: `"HistoricalNaturalFlow.xlsx"`}
#' 
#' \item{`crssio.nf_sheet_name`}{The sheet name to read from the natural flow 
#' Excel file if using Excel instead of the CoRiverNF package in 
#' [crssi_create_dnf_files()]. Default: `"InterveningNaturalFlow"`}
#' 
#' \item{`crssio.sac_yt_url`}{The website to download the Sacramento Year Type
#' index data from. Default: http://cdec.water.ca.gov/cgi-progs/iodir/WSIHIST}
#' }
NULL

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.crssio <- list(
    crssio.supplyScenarioSlot = 'HydrologyParameters.SupplyScenario',
    crssio.traceNumberSlot = 'HydrologyParameters.TraceNumber',
    crssio.hydroIncrement = 'MeadFloodControlData.hydrologyIncrement',
    crssio.sacYTSlot = "MWD_ICS.SacWYType",
    crssio.StVrainSlot= "TMD_East_Slope_Supply.St_Vrain_Annual_Flow",
    crssio.histNfFile = "HistoricalNaturalFlow.xlsx",
    crssio.nf_sheet_name = "InterveningNaturalFlow",
    crssio.sac_yt_url = "http://cdec.water.ca.gov/cgi-progs/iodir/WSIHIST"
  )
  toset <- !(names(op.crssio) %in% names(op))
  if(any(toset)) options(op.crssio[toset])
  
  invisible()
}
