
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.crssio <- list(
    crssio.supplyScenarioSlot = 'HydrologyParameters.SupplyScenario',
    crssio.traceNumberSlot = 'HydrologyParameters.TraceNumber'
  )
  toset <- !(names(op.crssio) %in% names(op))
  if(any(toset)) options(op.crssio[toset])
  
  invisible()
}
