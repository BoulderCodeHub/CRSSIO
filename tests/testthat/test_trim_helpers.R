
context("Check the trimming helper functions")

# get_yearmon_series() ------------------
test_that("get_yearmon_series() works", {
  expect_s3_class(CRSSIO:::get_yearmon_series(2014,2015), "yearmon")
  expect_length(CRSSIO:::get_yearmon_series(2014,2016), 36)
  expect_length(CRSSIO:::get_yearmon_series(2014,2014), 12)
  expect_error(
    CRSSIO:::get_yearmon_series(2014,2013),
    "endYear cannot be before startYear"
  )
  expect_equal(length(CRSSIO:::get_yearmon_series(2014,2016)) %% 12, 0)
  expect_equal(length(CRSSIO:::get_yearmon_series(2014,2014)) %% 12, 0)
  expect_equal(format(CRSSIO:::get_yearmon_series(2014,2015)[1], "%b"), "Jan")
  expect_equal(
    format(tail(CRSSIO:::get_yearmon_series(2014,2015), 1), "%b"), 
    "Dec"
  )
  expect_equal(format(CRSSIO:::get_yearmon_series(2014,2015)[1], "%Y"), "2014")
  expect_equal(
    format(tail(CRSSIO:::get_yearmon_series(2014,2020), 1), "%Y"), 
    "2020"
  )
})

# get_trace_file_header() ------------------
test_that("get_trace_file_header() works", {
  expect_type(CRSSIO:::get_trace_file_header(2018), "character")
  expect_equal(
    CRSSIO:::get_trace_file_header(2018),
    "start_date: 2018-1-31 24:00\nunits: acre-ft/month"
  )
  expect_equal(
    CRSSIO:::get_trace_file_header(2025),
    "start_date: 2025-1-31 24:00\nunits: acre-ft/month"
  )
  expect_equal(
    CRSSIO:::get_trace_file_header(2018, units = "cfs"),
    "start_date: 2018-1-31 24:00\nunits: cfs"
  )
  expect_equal(
    CRSSIO:::get_trace_file_header(2025, units = "mg/L"),
    "start_date: 2025-1-31 24:00\nunits: mg/L"
  )
})
