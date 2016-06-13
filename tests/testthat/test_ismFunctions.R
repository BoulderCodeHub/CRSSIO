library(CRSSIO)
library(xts)
context('check that the ISM related functions work.')

myYM <- zoo::as.yearmon('1905-01') + seq(0,47)/12
tstData <- 1:48
tstData <- xts::as.xts(zoo::read.zoo(data.frame(myYM,tstData)))

myIsm <- cbind(1:48,c(13:48,1:12),c(25:48,1:24),c(37:48,1:36))

test_that('createISMMatrix returns currect dimensions',{
  expect_equal(dim(createISMMatrix(tstData,'2016-01',nYrs = NA)),c(48,4))
  expect_equal(dim(createISMMatrix(tstData,'2016-01',nYrs = 3)),c(36,4))
  expect_equal(dim(createISMMatrix(tstData[1:36],'2016-01',nYrs = 2)),c(24,3))
})

test_that('createISMMatrix returns an expected matrix', {
  # comparing to a range of differences because myISM is not an xts object,
  # so all.equal will not work as it will return differences in attributes
  
  expect_equal(range(createISMMatrix(tstData, '2016-01', nYrs = NA)-myIsm), c(0,0))
  expect_equal(range(createISMMatrix(tstData, '2016-01',nYrs = 3)-myIsm[1:36,]), c(0,0))
})

test_that('createISMMatrix timestamps are correct', {
  expect_equal(index(createISMMatrix(tstData, '2016-01', nYrs = NA))[1], 
               zoo::as.yearmon('2016-01'))
  expect_equal(index(createISMMatrix(tstData, '2016-01', nYrs = NA))[48], 
               zoo::as.yearmon('2019-12'))
  expect_equal(index(createISMMatrix(tstData, '2016-01', nYrs = 3))[36], 
               zoo::as.yearmon('2018-12'))
})

test_that('createISMMatrix messages',{
  expect_error(createISMMatrix(tstData, '2016-01',nYrs = 5),
               'nYrs is longer than xtsData.')
  expect_error(createISMMatrix(myIsm, '2016-01'),
               'xtsData is not of type xts')
})
