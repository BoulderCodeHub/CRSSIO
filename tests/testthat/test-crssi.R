library(zoo)
library(xts)
library(CoRiverNF)
# crssi -------------------------------
mon_array <- array(
  50, 
  c(36, 5, 29, 2),
  dimnames = list(NULL, NULL, nf_gage_abbrv(), NULL)
)
ann_array <- array(40, c(3, 5, 29, 2), dimnames = list(NULL, NULL, nf_gage_abbrv(), NULL))
sac_mat <- matrix(sample.int(5, 3 * 5, replace = TRUE), ncol = 5)
yrs <- zoo::as.yearmon("Dec 2000") + 0:2
sac_xts <- xts(sac_mat, order.by = yrs)

test_that("crssi constructor works", {
  expect_is(
    x <- crssi(
      crss_nf(monthlyInt, flow_space = "intervening", time_step = "monthly"),
      CRSSIO:::sacYT, 1.19062018, "ISM 1906-2018"
    ),
    "crssi"
  )

  expect_length(x$monthly$intervening, 1)
  expect_true(CRSSIO:::has_intervening(x, "monthly"))
  expect_false(CRSSIO:::has_annual(x))
  expect_false(CRSSIO:::has_total(x, "monthly"))

  # print returns data invisibly
  expect_length(capture.output(expect_identical(x, print(x))), 5)

  expect_error(crssi(
    crss_nf(ann_array, time_step = "annual", start_yearmon = "Jan 2000"),
    sac_xts, 1.567, "sample"
  ))

  expect_error(crssi(
    crss_nf(mon_array, time_step = "monthly", start_yearmon = "Jan 2000", n_trace = 5, flow_space = "both"),
    sac_mat, 1.2000
  ))

  expect_is(x <- crssi(
    crss_nf(mon_array, time_step = "monthly", start_yearmon = "Jan 2000", n_trace = 5, flow_space = "both"),
    sac_xts, 1.2000,
    drop = FALSE
  ), "crssi")

  expect_equal(CRSSIO:::n_trace(x), 5)
  expect_true(CRSSIO:::has_intervening(x, "monthly"))
  expect_true(CRSSIO:::has_total(x, "monthly"))
  expect_false(CRSSIO:::has_total(x))
  expect_length(capture.output(expect_identical(x, print(x))), 4)
})
