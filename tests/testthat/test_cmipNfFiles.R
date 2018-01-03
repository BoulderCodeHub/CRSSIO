
library(CRSSIO)
context('Check that CMIP natural flow files are created correctly.')

cmipDir <- file.path(system.file("tests", package = "CRSSIO"), "cmip5", "tmp")

dir.create(cmipDir)
on.exit(unlink(cmipDir, recursive = T))

# unzip the example nc file
ncPath <- system.file("tests", "cmip5", package = "CRSSIO")
unzip(file.path(ncPath, "test_cmip5_bcsd.zip"), exdir = ncPath)
nc <- file.path(ncPath, "test_cmip5_bcsd.nc")
on.exit(file.remove(nc), add = TRUE)

fBad <- system.file("tests", "trace1", "DoloresRiver.Inflow", package = "CRSSIO")

test_that('correct errors post',{
  expect_error(
    crssi_create_cmip_nf_files("bad.file","",2018,2020), 
    "iFile does not exist"
  )
  
  expect_error(
    crssi_create_cmip_nf_files(fBad, "", 2018, 2020),
    paste(fBad, "does not appear to be a netcdf file.")
  )
  
  expect_error(
    crssi_create_cmip_nf_files(nc, file.path(cmipDir, "noExist"), 2018, 2020),
    paste0(file.path(cmipDir, "noExist"), " folder does not exist.", "\n", 
           "Create the directory before calling create_crss_cmip_nf_files()")
  )
  
  expect_error(
    crssi_create_cmip_nf_files(nc, cmipDir, 2025, 2020),
    "EndYear cannot be before startYear."
  )
  
  expect_error(
    crssi_create_cmip_nf_files(nc, cmipDir, 1940, 2020),
    "startYear should not be before 1950"
  )
  
  expect_error(
    crssi_create_cmip_nf_files(nc, cmipDir, 2018, 2100),
    "endYear should not be after 2099"
  )
})

allFiles <- c(CRSSNFInputNames(), "MWD ICS.SacWYType", 
              "MeadFloodControlData.hydrologyIncrement", 
              "HydrologyParameters.TraceNumber",
              "HydrologyParameters.SupplyScenario")

test_that("all files exist", {

})