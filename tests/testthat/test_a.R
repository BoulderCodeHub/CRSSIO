context("test reading Excel")

message("xlsx file exists: ", file.exists("../NaturalFlows_Sample.xlsx"))

test_that("can read Excel", {
  expect_type(readxl::read_excel("../NaturalFlows_Sample.xlsx"), "list")
  
  expect_warning(expect_s3_class(
    tmp <- CRSSIO:::read_and_format_nf_excel("../NaturalFlows_Sample.xlsx"),
    c("xts", "zoo")
  ))
  
  expect_equal(ncol(tmp), 29)
  expect_equal(nrow(tmp) %% 12, 0)
  expect_equal(zoo::index(tmp)[1], zoo::as.yearmon("Jan 1906"))
  expect_equal(format(tail(zoo::index(tmp),1), "%b"), "Dec")
})
