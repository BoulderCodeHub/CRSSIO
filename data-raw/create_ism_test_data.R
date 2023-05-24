# create the ism based test traces used by testthat
# typically this should not have to change unless the 1950s data change
# like hapened in 2020 release when we removed lb phreatophytes

library(CRSSIO)

crssi_create_dnf_files(
  'C:/alan/NaturalFlow/current/NaturalFlows1906-2020_20221215.xlsx',
  oFolder = 'tests/',
  startYear = 2017,
  endYear = 2021,
  recordToUse = c(zoo::as.yearmon('Jan 1950'), zoo::as.yearmon('Dec 1954')),
  overwriteFiles = TRUE
)
