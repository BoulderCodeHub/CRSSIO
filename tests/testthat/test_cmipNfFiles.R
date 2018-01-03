
library(xts)
library(zoo)
context('Check that CMIP natural flow files are created correctly')

cmipDir <- "../cmip5/tmp"

dir.create(cmipDir)
on.exit(unlink(cmipDir, recursive = T))

# unzip the example nc file
ncPath <- "../cmip5"
unzip(file.path(ncPath, "test_cmip5_bcsd.zip"), exdir = ncPath)
nc <- file.path(ncPath, "test_cmip5_bcsd.nc")
on.exit(file.remove(nc), add = TRUE)

fBad <- "../trace1/DoloresRiver.Inflow"

# test the errors ------------------------------

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
           "Create the directory before calling crssi_create_cmip_nf_files()")
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

# test creation of files -----------------------

# ** uncomment after we update the function to create these supplementary files
allFiles <- c(CRSSNFInputNames()) #, "MWD ICS.SacWYType", 
              # "MeadFloodControlData.hydrologyIncrement", 
              # "HydrologyParameters.TraceNumber",
              # "HydrologyParameters.SupplyScenario")

dmi1 <- file.path(cmipDir, "dmi1")
dmi2 <- file.path(cmipDir, "dmi2")
dir.create(dmi1)
dir.create(dmi2)

# create two sets of output, 1950-2099 and 2020-2029
test_that("output exists and all files exist", {
  expect_equal(
    crssi_create_cmip_nf_files(
      nc,
      oFolder = dmi1, 
      startYear = 1950,
      endYear = 2099
    ),
    nc
  )
  
  expect_equal(
    crssi_create_cmip_nf_files(
      nc,
      oFolder = dmi2, 
      startYear = 2020,
      endYear = 2029
    ),
    nc
  )
  
  # all files exist in both trace folders, and only files that should exist are
  # created
  expect_true(all(allFiles %in% list.files(file.path(dmi1, "trace1"))))
  expect_true(all(allFiles %in% list.files(file.path(dmi1, "trace2"))))
  expect_true(all(allFiles %in% list.files(file.path(dmi2, "trace1"))))
  expect_true(all(allFiles %in% list.files(file.path(dmi2, "trace2"))))
  expect_true(all(list.files(file.path(dmi1, "trace1")) %in% allFiles))
  expect_true(all(list.files(file.path(dmi1, "trace2")) %in% allFiles))
  expect_true(all(list.files(file.path(dmi2, "trace1")) %in% allFiles))
  expect_true(all(list.files(file.path(dmi2, "trace2")) %in% allFiles))
})

# check trimmed output matches full output -------------------------
# check that subset of the full output is the same as the trimmed output

# function to read in the file and create a zoo object
file2zoo <- function(iFile)
{
  zz <- scan(iFile, skip = 2, sep = "\n", quiet = TRUE)
  sm <- zoo::as.yearmon(
    scan(iFile, nlines = 1, what = "character", quiet = TRUE)[2]
  )
  mm <- sm + (seq_len(length(zz)) - 1)/12
  zz <- zoo::zoo(zz, mm)
  zz
}

rr <- sample(1:29, 4) # get 4 random nodes
message(cat("\n4 random nodes are:" ,rr),"\n")
rr <- file.path(
  c("trace1", "trace1", "trace2", "trace2"), 
  CRSSNFInputNames()[rr]
)
f1 <- file.path(dmi1, rr) # full data
f2 <- file.path(dmi2, rr) # trimmed data
trimMonths <- CRSSIO:::get_yearmon_series(2020, 2029)

test_that("Trimmed output matches full output", {
  expect_identical(file2zoo(f1[1])[trimMonths], file2zoo(f2[1]))
  expect_identical(file2zoo(f1[2])[trimMonths], file2zoo(f2[2]))
  expect_identical(file2zoo(f1[3])[trimMonths], file2zoo(f2[3]))
  expect_identical(file2zoo(f1[4])[trimMonths], file2zoo(f2[4]))
})

# compare to original data -------------------------
# check that full output matches original files that ncdf was created from 
# get 4 more random nodes
rr <- sample(1:29, 4) # get 4 random nodes
message(cat("\n4 random nodes are:" ,rr),"\n")
rr <- file.path(
  c("trace1", "trace1", "trace2", "trace2"), 
  CRSSNFInputNames()[rr]
)
f1 <- file.path(dmi1, rr) # full data
f2 <- file.path("../ccOrig", rr) # trimmed data

test_that("Full output matches orignal data", {
  expect_identical(file2zoo(f1[1]), file2zoo(f2[1]))
  expect_identical(file2zoo(f1[2]), file2zoo(f2[2]))
  expect_identical(file2zoo(f1[3]), file2zoo(f2[3]))
  expect_identical(file2zoo(f1[4]), file2zoo(f2[4]))
})
