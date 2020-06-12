context("stat_boxplot_custom")

library(ggplot2)

p <- ggplot(mpg, aes(class, hwy))

test_that("errors post for invalid `qs`", {
  expect_error(
    p + stat_boxplot_custom(qs = as.character(c(0, .25, 0.5, 0.75, 1))),
    "`qs` should be a numeric vector with 5 values."
  )
  
  expect_error(
    p + stat_boxplot_custom(qs = c(0, .05, .25, .5, .75, .95)),
    "`qs` should be a numeric vector with 5 values."
  )
  
  expect_error(
    p + stat_boxplot_custom(qs = c(-1, .25, .5, .75, .95)),
    "`qs` should only span values [0, 1].",
    fixed = TRUE
  )
  expect_error(
    p + stat_boxplot_custom(qs = c(0, .25, .5, .75, 1.1)),
    "`qs` should only span values [0, 1].",
    fixed = TRUE
  )
  
  expect_error(
    p + stat_boxplot_custom(qs = c(.1, .25, .5, .95, .75)),
    "`qs` should be provided in ascending order."
  )
})


test_that("custom plot works with all examples from ggplot site", {
  expect_s3_class(ggplot_build(p + stat_boxplot_custom()), "ggplot_built")
  expect_s3_class(
    ggplot_build(p + stat_boxplot_custom(qs = c(0, .25, 0.5, 0.75, 1))), 
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(p + stat_boxplot_custom(qs = c(.1, .25, 0.5, 0.75, .9))), 
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(p + stat_boxplot_custom(notch = TRUE)), 
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(p + stat_boxplot_custom(fill = "grey20", color = "blue")), 
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(p + stat_boxplot_custom(outlier.shape = NA)), 
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(p + stat_boxplot_custom(aes(color = drv))), 
    "ggplot_built"
  )
  expect_s3_class(
    expect_warning(
      ggplot_build(ggplot(diamonds, aes(carat, price)) + stat_boxplot_custom())
    ), 
    "ggplot_built"
  )
  expect_s3_class(
    expect_warning(
      ggplot_build(
        ggplot(diamonds, aes(carat, price)) + 
          stat_boxplot_custom(aes(group = cut_width(carat, 0.25)))
      )
    ), 
    "ggplot_built"
  )
  expect_s3_class(
    expect_warning(
      ggplot_build(
        ggplot(diamonds, aes(carat, price)) +
          stat_boxplot_custom(
            aes(group = cut_width(carat, 0.25)), 
            outlier.alpha = 0.1
          )
      )
    ), 
    "ggplot_built"
  )
})
