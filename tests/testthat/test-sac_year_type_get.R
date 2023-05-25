test_that("sac_year_type_get() works", {
  expect_is(x <- sac_year_type_get(), "xts")
  expect_identical(start(x), zoo::as.yearmon("Sep 1906"))
  expect_equal(x["/2022"], CRSSIO:::sacYT)
})

test_that("sac_year_type_get() works with paleo.", {
  expect_s3_class(x <- sac_year_type_get(paleo = TRUE), "xts")
  expect_identical(start(x), zoo::as.yearmon("Dec 900"))
  expect_equal(nrow(x), nrow(CRSSIO:::sac_paleo_wy_vol))
})
