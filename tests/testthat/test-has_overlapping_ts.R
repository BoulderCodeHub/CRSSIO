mon_array <- array(
  1, 
  c(36, 5, 29, 2), 
  dimnames = list(NULL, NULL, nf_gage_abbrv(), NULL)
)

ann_array <- array(
  2,
  c(3, 5, 29, 2),
  dimnames = list(NULL, NULL, nf_gage_abbrv(), NULL)
)

# has_overlapping_ts() -----------------------------
#has_overlapping_ts(x, exact = TRUE)
x1 <- nfd(1, n_months = 36, n_trace = 5, n_sites = 29, flow_space = "both",
          time_step = "both", start_yearmon = "Jan 2020", 
          site_names = nf_gage_abbrv())
x2 <- nfd(1, n_months = 36, n_trace = 5, n_sites = 29, flow_space = "both",
          time_step = "both", start_yearmon = "Oct 2020", 
          site_names = nf_gage_abbrv(), year = "wy")
x3 <- nfd(1, n_months = 36, n_trace = 5, n_sites = 29, flow_space = "both",
          time_step = "both", start_yearmon = "June 2020", 
          site_names = nf_gage_abbrv())
test_that("has_overlapping_ts() works with exact = TRUE", {
  expect_true(has_overlapping_ts(x1))
  expect_true(has_overlapping_ts(x2))
  # should fail b/c data do not start/end at beginning/end of cy
  expect_false(has_overlapping_ts(x3))
  # should be true for nfd of only monthly/annual data
  expect_true(
    has_overlapping_ts(expect_warning(nfd(mon_array, time_step = "monthly")))
  )
  expect_true(
    has_overlapping_ts(expect_warning(nfd(ann_array, time_step = "annual")))
  )
})

# exact = FALSE ------------------------------------------
test_that("has_overlapping_ts(exact = FALSE)", {
  expect_true(has_overlapping_ts(x1, exact = FALSE))
  expect_true(has_overlapping_ts(x2, exact = FALSE))
  # should pass this time because exact = FALSE
  expect_true(has_overlapping_ts(x3, exact = FALSE))
  
  # now hard code some different time steps
  
  expect_warning(
    m1 <- nfd(mon_array, time_step = "monthly", start_yearmon = "Jan 2022")
  )
  x2$monthly <- m1$monthly
  x3$monthly <- m1$monthly
  expect_true(has_overlapping_ts(x2, exact = FALSE))
  expect_true(has_overlapping_ts(x3, exact = FALSE))
  expect_warning(
    y1 <- nfd(ann_array, time_step = "annual", start_yearmon = "Dec 2024")
  )
  x2$annual <- y1$annual
  expect_true(has_overlapping_ts(x2, exact = FALSE))
})