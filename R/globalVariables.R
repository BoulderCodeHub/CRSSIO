# global variables added so that there are no notes when running R CMD check

if(getRversion() >= "2.15.1"){
  # global variables necessary because of createSysCondTable
  from_createSysCondTable <- c('Year','Variable','Value')
  from_gg_ribbon_cloud <- c("q05", "q25", "q50", "q75", "q95")
  utils::globalVariables(c(from_createSysCondTable, ".", from_gg_ribbon_cloud))
}
