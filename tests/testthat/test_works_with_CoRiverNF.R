context("check files created by CoRiverNF and by excel file")

library(CoRiverNF)

tmpDir <- tempdir()
m1Folder <- file.path(tmpDir, "method1")
m2Folder <- file.path(tmpDir, "method2")
testthat::setup(dir.create(m1Folder))
testthat::setup(dir.create(m2Folder))
teardown({
  unlink(m1Folder, recursive = TRUE)
  unlink(m2Folder, recursive = TRUE)
})

test_that("created files from package and Excel match", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_on_ci()
  
  xlIn <- "C:/alan/NaturalFlow/current/NaturalFlows1906-2019_20210415.xlsx"
  if (!file.exists(xlIn))
    stop("You need to update the xlIn variable in test_data.R.")
  
  crssi_create_dnf_files(
    "CoRiverNF",
    oFolder = file.path(tmpDir, "method1"),
    startYear = 2018, 
    endYear = 2060
  )
  
  expect_warning(crssi_create_dnf_files(
    xlIn,
    oFolder = file.path(tmpDir, "method2"),
    startYear = 2018, 
    endYear = 2060
  ))
  
  dirs <- list.dirs(m1Folder, recursive = FALSE, full.names = FALSE)
  
  for(curDir in dirs){
    allFiles <- list.files(file.path(m1Folder, curDir))
    for(ff in allFiles){
      test_that("all files are the same", {
        expect_identical(
          scan(
            file.path(m1Folder, curDir, ff), 
            what = "character", 
            quiet = TRUE
          ),
          scan(
            file.path(m2Folder, curDir, ff), 
            what = "character", 
            quiet = TRUE
          ),
          info = paste(curDir, ff)
        )
      })
    }
  }
  
})
