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
#' }
#' 
#' Additional options for file I/O:
#' \describe{
#' \item{`crssio.histNfFile`}{The file name that will be created by
#' `crssi_create_hist_nf_xlsx()`. Default: `"HistoricalNaturalFlow.xlsx"`}
#' \item{`crssio.nf_sheet_name`}{The sheet name to read from the natural flow 
#' Excel file if using Excel instead of the CoRiverNF package in 
#' [crssi_create_dnf_files()]. Default: `"InterveningNaturalFlow"`}
#' }

#' @keywords internal
"_PACKAGE"

#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @importFrom assertthat assert_that
NULL
