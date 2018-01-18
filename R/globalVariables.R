# global variables added so that there are no notes when running R CMD check

if(getRversion() >= "2.15.1"){
  # global variables necessary because of createSysCondTable
  from_createSysCondTable <- c('Year','Variable','Value')
  utils::globalVariables(c(from_createSysCondTable, "."))
}
