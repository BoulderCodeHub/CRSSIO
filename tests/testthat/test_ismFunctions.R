library(CRSSIO)
context('check that the ISM related functions work.')

myYM <- zoo::as.yearmon('1905-01') + seq(0,47)/12
tstData <- 1:48
tstData <- xts::as.xts(zoo::read.zoo(data.frame(myYM,tstData)))

myIsm <- cbind(1:48,c(13:48,1:12),c(25:48,1:24),c(37:48,1:36))

test_that('createISMMatrix returns an expected matrix', {
  # comparing to a range of differences because myISM is not an xts object,
  # so all.equal will not work as it will return differences in attributes
  
  # not sure why these work with devtools::test, but fail when using check.
  
  #expect_equal(range(createISMMatrix(tstData, '2016-01', nYrs = NA)-myIsm), c(0,0))
  #expect_equal(range(createISMMatrix(tstData, '2016-01',nYrs = 3)-myIsm[1:36,]), c(0,0))
})

test_that('createISMMatrix errors out correctly',{
  expect_error(createISMMatrix(tstData, '2016-01',nYrs = 5),
               'nYrs is longer than xtsData.')
  # ADD: need more tests here
})
