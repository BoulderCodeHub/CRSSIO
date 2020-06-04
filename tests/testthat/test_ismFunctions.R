library(xts)

# tests ism_get_site_matrix(), and compares its results to ism()

# setup ----------------------------------------------
myYM <- zoo::as.yearmon('1905-01') + seq(0,47)/12
tstData <- xts::xts(1:48, myYM)
t2 <- xts::xts(1:6, zoo::as.yearmon("Dec 2000") + 0:5)

myIsm <- xts::xts(
  cbind(1:48,c(13:48,1:12),c(25:48,1:24),c(37:48,1:36)),
  zoo::as.yearmon('1905-01') + seq(0,47)/12
)
myIsm2 <- xts::xts(
  cbind(1:6, c(2:6, 1), c(3:6, 1:2), c(4:6, 1:3), c(5:6, 1:4), c(6, 1:5)),
  zoo::as.yearmon("Dec 2000") + 0:5
)


# check dimension --------------------------
test_that('ism_get_site_matrix returns currect dimensions',{
  expect_equal(dim(expect_warning(ism_get_site_matrix(tstData,'2016-01',nYrs = NA))),c(48,4))
  expect_equal(dim(expect_warning(ism_get_site_matrix(tstData,'2016-01',nYrs = 3))),c(36,4))
  expect_equal(
    dim(expect_warning(ism_get_site_matrix(tstData[1:36],'2016-01',nYrs = 2))),
    c(24,3)
  )
  # test the annual data
  expect_equal(
    dim(expect_warning(ism_get_site_matrix(t2[1:5], "2016-12", nYrs = NA, monthly = FALSE))),
    c(5,5)
  )
  expect_equal(
    dim(expect_warning(ism_get_site_matrix(t2, "2016-12", nYrs = 5, monthly = FALSE))),
    c(5,6)
  )
  expect_equal(
    dim(expect_warning(ism_get_site_matrix(t2[1:4], "2016-12", nYrs = 3, monthly = FALSE))),
    c(3,4)
  )
})

# check values -----------------------------
test_that('ism_get_site_matrix returns an expected matrix', {
  # comparing to a range of differences because myISM is not an xts object,
  # so all.equal will not work as it will return differences in attributes
  
  expect_equivalent(expect_warning(ism_get_site_matrix(tstData, '2016-01', nYrs = NA)), myIsm)
  expect_equivalent(
    expect_warning(ism_get_site_matrix(tstData, '2016-01',nYrs = 3)), 
    myIsm[1:36,]
  )
  expect_equivalent(
    expect_warning(ism_get_site_matrix(t2, "2016-12", nYrs = NA, monthly = FALSE)),
    myIsm2
  )
  expect_equivalent(
    expect_warning(ism_get_site_matrix(t2, "2016-12", nYrs = 5, monthly = FALSE)),
    myIsm2[1:5,]
  )
})

tmp <- expect_warning(ism_get_site_matrix(t2, "2016-12", monthly = FALSE))
test_that('ism_get_site_matrix timestamps are correct', {
  expect_equal(index(expect_warning(ism_get_site_matrix(tstData, '2016-01', nYrs = NA)))[1], 
               zoo::as.yearmon('2016-01'))
  expect_equal(index(expect_warning(ism_get_site_matrix(tstData, '2016-01', nYrs = NA)))[48], 
               zoo::as.yearmon('2019-12'))
  expect_equal(index(expect_warning(ism_get_site_matrix(tstData, '2016-01', nYrs = 3)))[36], 
               zoo::as.yearmon('2018-12'))
  expect_equal(index(tmp)[1], zoo::as.yearmon("2016-12"))
  expect_equal(index(tmp)[6], zoo::as.yearmon("2021-12"))
})

test_that('ism_get_site_matrix messages',{
  expect_error(expect_warning(ism_get_site_matrix(tstData, '2016-01',nYrs = 5)),
               'nYrs is longer than xtsData.')
  expect_error(expect_warning(ism_get_site_matrix(as.matrix(myIsm), '2016-01')),
               'xtsData is not of type xts')
})

# ism_get_site_matrix() vs ism() ------------------------
test_that("ism_get_site_matrix() == ism()", {
  expect_equivalent(
    coredata(expect_warning(ism_get_site_matrix(tstData,'2016-01',nYrs = NA))),
    coredata(ism(tstData))
  )
  expect_equivalent(
    coredata(expect_warning(ism_get_site_matrix(tstData,'2016-01',nYrs = 3))),
    coredata(ism(tstData, n_years_keep = 3))
  )
  expect_equivalent(
    coredata(expect_warning(ism_get_site_matrix(tstData[1:36],'2016-01',nYrs = 2))),
    coredata(ism(tstData[1:36], n_years_keep = 2))
  )
  
  # test the annual data
  expect_equivalent(
    coredata(expect_warning(ism_get_site_matrix(t2[1:5], "2016-12", nYrs = NA, monthly = FALSE))),
    coredata(ism(t2[1:5]))
  )
  expect_equivalent(ism(t2[1:5]), ism(t2[1:5], is_monthly = FALSE))
  expect_equivalent(
    coredata(expect_warning(ism_get_site_matrix(t2, "2016-12", nYrs = 5, monthly = FALSE))),
    coredata(ism(t2, n_years_keep = 5, is_monthly = FALSE))
  )
  expect_equivalent(
    coredata(expect_warning(ism_get_site_matrix(t2[1:4], "2016-12", nYrs = 3, monthly = FALSE))),
    coredata(ism(t2[1:4], n_years_keep = 3))
  )
})
