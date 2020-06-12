test_that("sac_year_type_get() works", {
  expect_is(x <- sac_year_type_get(), "xts")
  expect_identical(start(x), zoo::as.yearmon("Dec 1906"))
  expect_equal(x["/2018"], CRSSIO:::sacYT)
})
