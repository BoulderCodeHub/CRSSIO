
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.crssio <- list(
    crssio.supplyScenarioSlot = 'HydrologyParameters.SupplyScenario',
    crssio.traceNumberSlot = 'HydrologyParameters.TraceNumber',
    crssio.hydroIncrement = 'MeadFloodControlData.hydrologyIncrement',
    crssio.sacYTSlot = "MWD ICS.SacWYType"
  )
  toset <- !(names(op.crssio) %in% names(op))
  if(any(toset)) options(op.crssio[toset])
  
  invisible()
}
