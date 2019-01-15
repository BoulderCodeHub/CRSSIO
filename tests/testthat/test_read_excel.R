context("test reading Excel")

message("xlsx file exists: ", file.exists("../NaturalFlows_Sample.xlsx"))

test_that("can read Excel", {
  expect_type(readxl::read_excel("../NaturalFlows_Sample.xlsx"), "list")
})

