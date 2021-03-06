context("Check elevation_to_storage")

test_that("warnings and errors work", {
  expect_error(elevation_to_storage(5000, "hoover"), "Invalid reservoir")
  expect_error(elevation_to_storage(3434, "shasta"), "Invalid reservoir")
  expect_true(is.na(elevation_to_storage(3490, "mead")))
  expect_true(is.na(elevation_to_storage(1000, "powell")))
  expect_error(
    elevation_to_storage(5000, c("mead", "powell")),
    "`reservoir` should be a character vector with length = 1."
  )
  expect_error(
    elevation_to_storage(5000, 50),
    "`reservoir` should be a character vector with length = 1."
  )
})

test_that("Values are the same when they should be the same", {
  expect_equal(
    elevation_to_storage(1000, "Mead"), 
    elevation_to_storage(1000, "mead")
  )
  
  expect_equal(
    elevation_to_storage(6000, "Flaming Gorge"), 
    elevation_to_storage(6000, "flaminggorge")
  )
  
  expect_equal(
    elevation_to_storage(7400, "Blue Mesa"), 
    elevation_to_storage(7400, "blueMesa")
  )
})

test_that("return values are correct length and type", {
  expect_length(elevation_to_storage(c(1000, 1050, 1075), "Mead"), 3)
  expect_length(elevation_to_storage(c(1000, 1050, 1075), "Powell"), 3)
  expect_length(
    elevation_to_storage(c(3490, 3525, 3575, 3576, 3578), "Powell"), 
    5
  )
  expect_identical(
    class(elevation_to_storage(c(1000, 1050, 1075), "Powell")), 
    "numeric"
  )
  expect_identical(
    class(elevation_to_storage(c(1000, 1050, 1075), "Mead")), 
    "numeric"
  )
  expect_identical(
    class(elevation_to_storage(c(1000, 3490), "Mead")), 
    "numeric"
  )
  expect_true(anyNA(elevation_to_storage(c(1000, 3490), "Mead")))
  expect_true(!anyNA(elevation_to_storage(c(1000, 1050, 1075), "Mead")))
})
