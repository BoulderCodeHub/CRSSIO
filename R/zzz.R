
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.crssio <- list(
    crssio.supplyScenarioSlot = 'Supply.Number',
    crssio.traceNumberSlot = 'Trace.Number'
  )
  toset <- !(names(op.crssio) %in% names(op))
  if(any(toset)) options(op.crssio[toset])
  
  invisible()
}
