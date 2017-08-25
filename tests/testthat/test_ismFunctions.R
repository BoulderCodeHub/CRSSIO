library(CRSSIO)
library(xts)
context('check that the ISM related functions work.')

myYM <- zoo::as.yearmon('1905-01') + seq(0,47)/12
tstData <- xts::xts(1:48, myYM)
xts::indexTZ(tstData) <- 'UTC'
t2 <- xts::xts(1:6, zoo::as.yearmon("Dec 2000") + 0:5)
xts::indexTZ(t2) <- "UTC"

myIsm <- xts::xts(
  cbind(1:48,c(13:48,1:12),c(25:48,1:24),c(37:48,1:36)),
  zoo::as.yearmon('1905-01') + seq(0,47)/12
)
myIsm2 <- xts::xts(
  cbind(1:6, c(2:6, 1), c(3:6, 1:2), c(4:6, 1:3), c(5:6, 1:4), c(6, 1:5)),
  zoo::as.yearmon("Dec 2000") + 0:5
)

test_that('createISMMatrix returns currect dimensions',{
  expect_equal(dim(createISMMatrix(tstData,'2016-01',nYrs = NA)),c(48,4))
  expect_equal(dim(createISMMatrix(tstData,'2016-01',nYrs = 3)),c(36,4))
  expect_equal(dim(createISMMatrix(tstData[1:36],'2016-01',nYrs = 2)),c(24,3))
  # test the annual data
  expect_equal(dim(createISMMatrix(t2[1:5], "2016-12", nYrs = NA, monthly = FALSE)),
               c(5,5))
  expect_equal(dim(createISMMatrix(t2, "2016-12", nYrs = 5, monthly = FALSE)),
               c(5,6))
  expect_equal(dim(createISMMatrix(t2[1:4], "2016-12", nYrs = 3, monthly = FALSE)),
               c(3,4))
})

test_that('createISMMatrix returns an expected matrix', {
  # comparing to a range of differences because myISM is not an xts object,
  # so all.equal will not work as it will return differences in attributes
  
  expect_equivalent(createISMMatrix(tstData, '2016-01', nYrs = NA), myIsm)
  expect_equivalent(createISMMatrix(tstData, '2016-01',nYrs = 3), myIsm[1:36,])
  expect_equivalent(createISMMatrix(t2, "2016-12", nYrs = NA, monthly = FALSE),
                    myIsm2)
  expect_equivalent(createISMMatrix(t2, "2016-12", nYrs = 5, monthly = FALSE),
                    myIsm2[1:5,])
})

tmp <- createISMMatrix(t2, "2016-12", monthly = FALSE)
test_that('createISMMatrix timestamps are correct', {
  expect_equal(index(createISMMatrix(tstData, '2016-01', nYrs = NA))[1], 
               zoo::as.yearmon('2016-01'))
  expect_equal(index(createISMMatrix(tstData, '2016-01', nYrs = NA))[48], 
               zoo::as.yearmon('2019-12'))
  expect_equal(index(createISMMatrix(tstData, '2016-01', nYrs = 3))[36], 
               zoo::as.yearmon('2018-12'))
  expect_equal(index(tmp)[1], zoo::as.yearmon("2016-12"))
  expect_equal(index(tmp)[6], zoo::as.yearmon("2021-12"))
})

test_that('createISMMatrix messages',{
  expect_error(createISMMatrix(tstData, '2016-01',nYrs = 5),
               'nYrs is longer than xtsData.')
  expect_error(createISMMatrix(as.matrix(myIsm), '2016-01'),
               'xtsData is not of type xts')
})

tstMat <- xts::as.xts(zoo::read.zoo(
  data.frame(myYM,matrix(rep(tstData,29), ncol = 29, byrow = F))))
xts::indexTZ(tstMat) <- 'UTC'

test_that('getAllISMMatrices works', {
  expect_error(getAllISMMatrices(cbind(tstData,tstData), '2016-01', 3),
               'nfMat does not contain 29 columns')
  expect_equal(getAllISMMatrices(tstMat,'2016-01', 3)[[1]], 
               createISMMatrix(tstData, '2016-01', nYrs = 3))
  expect_equal(getAllISMMatrices(tstMat,'2016-01', NA)[[1]],
               getAllISMMatrices(tstMat,'2016-01', NA)[[29]])
  expect_equal(length(getAllISMMatrices(tstMat,'2016-01', NA)),29)
})
