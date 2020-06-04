library(ggplot2)

# get_decimals -----------------------------------
test_that("get_decimals returns expected values", {
  expect_identical(CRSSIO:::get_decimals("1"), 0)
  expect_identical(CRSSIO:::get_decimals(c("1", "4", "3434")), 0)
  expect_identical(CRSSIO:::get_decimals("1.1234"), 4)
  expect_identical(CRSSIO:::get_decimals(c("1","1.1")), 1)
  expect_identical(CRSSIO:::get_decimals(c("1.123", "1.123", "334332.122")), 3)
  expect_identical(CRSSIO:::get_decimals(c("1.3456", "1","1.1")), 4)
})

# to_metric_labels ------------------------------------
test_that("to_metric_labels returns as expected", {
  expect_identical(CRSSIO:::to_metric_labels(123, 0), "123")
  expect_identical(CRSSIO:::to_metric_labels(1234, 0), "1,234")
  expect_identical(CRSSIO:::to_metric_labels(1234, 1), "1,234.0")
  expect_identical(CRSSIO:::to_metric_labels(
    c(12, 1234, 1234567), 2), 
    c("12.00", "1,234.00", "1,234,567.00")
  )
  expect_identical(CRSSIO:::to_metric_labels(123.4, 0), "123")
  expect_identical(CRSSIO:::to_metric_labels(123.4, 1), "123.4")
  expect_identical(
    CRSSIO:::to_metric_labels(c(123.4, 4567.789), 1), 
    c("123.4", "4,567.8")
  )
})

# add_secondary_y_conversion() errors ----------------------------
df <- data.frame(year = 2020:2029, pe = rnorm(10, 3580, 15))
gg <- ggplot(df, aes(year, pe)) + geom_line()

test_that("add_secondary_y_conversion errors expectedly", {
  expect_error(
    add_secondary_y_conversion(df, "meters", "feet"),
    "`gg` does not inherit from c('gg', 'ggplot')",
    fixed = TRUE
  )
  expect_error(
    add_secondary_y_conversion(gg, "meters", "feet", digits = c("a", "b")),
    "`digits` should be a single numeric value or single string."
  )
  
  expect_error(
    add_secondary_y_conversion(gg, "meters", "feet", digits = c(5,6)),
    "`digits` should be a single numeric value or single string."
  )
  
  expect_error(
    add_secondary_y_conversion(gg, "meters", "feet", digits = "a"),
    paste0(
      "`digits` should be an allowable function name.\n", 
      "See ?add_secondary_metric"
    ),
    fixed = TRUE
  )
})

# add_secondary_y_conversion builds --------------------------------
test_that("add_secondary_y_conversion builds", {
  expect_s3_class(
    ggplot_build(add_secondary_y_conversion(gg, "feet", "meters")), 
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(add_secondary_y_conversion(gg, "feet", "meters", digits = 0)), 
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(add_secondary_y_conversion(gg, "feet", "meters", digits = 3)), 
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(add_secondary_y_conversion(
      gg, "feet", "meters", digits = "get_decimals"
    )), 
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(add_secondary_y_conversion(gg, "acre_feet", "m^3")), 
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(
      add_secondary_y_conversion(gg, "feet", "meters", sec_name = "(m)")
    ), 
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(add_secondary_y_conversion(gg, "acre_feet/month", "km^3/s")), 
    "ggplot_built"
  )
})
