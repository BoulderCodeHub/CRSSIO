
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.crssio <- list(
    crssio.supplyScenarioSlot = 'HydrologyParameters.SupplyScenario',
    crssio.traceNumberSlot = 'HydrologyParameters.TraceNumber',
    crssio.hydroIncrement = 'MeadFloodControlData.hydrologyIncrement',
    crssio.sacYTSlot = "MWD_ICS.SacWYType",
    crssio.histNfFile = "HistoricalNaturalFlow.xlsx",
    crssio.nf_sheet_name = "InterveningNaturalFlow"
  )
  toset <- !(names(op.crssio) %in% names(op))
  if(any(toset)) options(op.crssio[toset])
  
  invisible()
}
