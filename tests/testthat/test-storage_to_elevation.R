context("storage_to_elevation")

test_that("warnings and errors work", {
  expect_error(storage_to_elevation(5000, "hoover"), "Invalid reservoir")
  expect_error(storage_to_elevation(3434, "shasta"), "Invalid reservoir")
  expect_true(is.na(storage_to_elevation(35000000, "mead")))
  expect_true(is.na(storage_to_elevation(-1000, "powell")))
  expect_error(
    storage_to_elevation(5000, c("mead", "powell")),
    "`reservoir` should be a character vector with length = 1."
  )
  expect_error(
    storage_to_elevation(5000, 50),
    "`reservoir` should be a character vector with length = 1."
  )
})

test_that("Values are the same when they should be the same", {
  expect_equal(
    storage_to_elevation(100000, "Mead"), 
    storage_to_elevation(100000, "mead")
  )
  
  expect_equal(
    storage_to_elevation(600000, "Flaming Gorge"), 
    storage_to_elevation(600000, "flaminggorge")
  )
  
  expect_equal(
    storage_to_elevation(500025, "Blue Mesa"), 
    storage_to_elevation(500025, "blueMesa")
  )
})

test_that("return values are correct length and type", {
  expect_length(storage_to_elevation(c(1000, 1050, 1075) * 10000, "Mead"), 3)
  expect_length(storage_to_elevation(c(1000, 1050, 1075) * 1e6, "Powell"), 3)
  expect_length(
    storage_to_elevation(c(3490, 3525, 3575, 3576, 3578) * 1000, "Powell"), 
    5
  )
  expect_identical(
    class(storage_to_elevation(c(1000, 1050, 1075), "Powell")), 
    "numeric"
  )
  expect_identical(
    class(storage_to_elevation(c(1000, 1050, 1075), "Mead")), 
    "numeric"
  )
  expect_identical(
    class(storage_to_elevation(c(1000, 3490) * 10000, "Mead")), 
    "numeric"
  )
  expect_true(anyNA(storage_to_elevation(c(1000, 3490) * 10000, "Mead")))
  expect_true(!anyNA(storage_to_elevation(c(1000, 1050, 1075) * 10000, "Mead")))
})

test_that("elevation_to_storage and storage_to_elevation are inverses", {
  expect_equal(
    elevation_to_storage(storage_to_elevation(10000500, "mead"), "mead"),
    10000500
  )
  expect_equal(
    elevation_to_storage(storage_to_elevation(10000500, "powell"), "powell"),
    10000500
  )
  expect_equal(
    elevation_to_storage(storage_to_elevation(400010, "bluemesa"), "blue mesa"),
    400010
  )
  expect_equal(
    storage_to_elevation(elevation_to_storage(1097, "mead"), "mead"),
    1097
  )
  expect_equal(
    storage_to_elevation(elevation_to_storage(3588, "powell"), "powell"),
    3588
  )
  expect_equal(
    storage_to_elevation(elevation_to_storage(5992, "navajo"), "navajo"),
    5992
  )
})
