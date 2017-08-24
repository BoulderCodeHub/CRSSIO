library(RWDataPlyr)

context('Check system condition table creation')

slotAggList <- RWDataPlyr::createSlotAggList(CRSSIO::sysCondSALMatrix())
scenFolder <- 'ISM1988_2014,2007Dems,IG,Most'
scenName <- 'scen1'
scenPath <- system.file('extdata','Scenario/',package = 'RWDataPlyr')
sysData <- RWDataPlyr::getDataForAllScens(scenFolder, scenName, slotAggList,
                                          scenPath, 'tmp.feather', TRUE)
yrs <- 2018:2022
sysCondTable <- createSysCondTable(sysData, yrs)

test_that('object dimensions and attributes are correct', {
  expect_equal(length(sysCondTable), 2)
  expect_equal(names(sysCondTable), c("fullTable", "limitedTable"))
  expect_equal(dim(sysCondTable$fullTable), c(length(CRSSIO:::slotNames())+4,length(yrs)))
  expect_equal(dim(sysCondTable$limitedTable), c(length(CRSSIO:::slotNames())+1,length(yrs)))
})
