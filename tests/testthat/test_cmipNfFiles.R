
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

myScen <- 5
# test the errors ------------------------------

test_that('correct errors post',{
  expect_error(
    crssi_create_cmip_nf_files("bad.file", "", 2018, 2020, myScen), 
    "iFile does not exist"
  )
  
  expect_error(
    crssi_create_cmip_nf_files(fBad, "", 2018, 2020, myScen),
    paste(fBad, "does not appear to be a netcdf file.")
  )
  
  expect_error(
    crssi_create_cmip_nf_files(
      nc, 
      file.path(cmipDir, "noExist"), 
      2018, 
      2020, 
      myScen
    ),
    paste0(file.path(cmipDir, "noExist"), " folder does not exist.", "\n", 
           "Create the directory before calling crssi_create_cmip_nf_files()")
  )
  
  expect_error(
    crssi_create_cmip_nf_files(nc, cmipDir, 2025, 2020, myScen),
    "EndYear cannot be before startYear."
  )
  
  expect_error(
    crssi_create_cmip_nf_files(nc, cmipDir, 1940, 2020, myScen),
    "startYear should not be before 1950"
  )
  
  expect_error(
    crssi_create_cmip_nf_files(nc, cmipDir, 2018, 2100, myScen),
    "endYear should not be after 2099"
  )
  
  expect_error(
    crssi_create_cmip_nf_files(nc, cmipDir, 2018, 2020, 1),
    "Invalid scenario number for climate change natural flow files."
  )
})

# test creation of files -----------------------

# ** uncomment after we update the function to create these supplementary files
sacFile <- getOption("crssio.sacYTSlot")
allFiles <- c(nf_file_names(), sacFile, 
              "MeadFloodControlData.hydrologyIncrement", 
              "HydrologyParameters.TraceNumber",
              "HydrologyParameters.SupplyScenario")

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
      endYear = 2099, 
      scenarioNumber = myScen
    ),
    nc
  )
  
  expect_equal(
    crssi_create_cmip_nf_files(
      nc,
      oFolder = dmi2, 
      startYear = 2020,
      endYear = 2029,
      scenarioNumber = myScen
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
  expect_true(file.exists(file.path(dmi1, "README.txt")))
  expect_true(file.exists(file.path(dmi2, "README.txt")))
  
  # function will fail if you try to create files again with overwrite == FALSE
  expect_error(
    crssi_create_cmip_nf_files(
      nc,
      oFolder = dmi2, 
      startYear = 2020,
      endYear = 2029,
      scenarioNumber = myScen
    ),
    paste0(
      "Trace files exist in ", dmi2, "\n",
      "  Choose a different folder, delete the files, or use overwriteFiles = TRUE"
    )
  )
})

# check trimmed output matches full output -------------------------
# check that subset of the full output is the same as the trimmed output

# function to read in the file and create a zoo object
file2zoo <- function(iFile, monthly = TRUE)
{
  zz <- scan(iFile, skip = 2, sep = "\n", quiet = TRUE)
  sm <- zoo::as.yearmon(
    scan(iFile, nlines = 1, what = "character", quiet = TRUE)[2]
  )
  if (monthly){
    mm <- sm + (seq_len(length(zz)) - 1)/12
  } else {
    # annual so create time series based on december only
    mm <- sm + (seq_len(length(zz)) - 1)
  }
  zz <- zoo::zoo(zz, mm)
  zz
}

rr <- sample(1:29, 4) # get 4 random nodes
message(cat("\n4 random nodes are:" ,rr),"\n")
rr <- file.path(
  c("trace1", "trace1", "trace2", "trace2"), 
  nf_file_names()[rr]
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
  nf_file_names()[rr]
)
f1 <- file.path(dmi1, rr) # full data
f2 <- file.path("../ccOrig", rr) # trimmed data

test_that("Full output matches orignal data", {
  expect_identical(file2zoo(f1[1]), file2zoo(f2[1]))
  expect_identical(file2zoo(f1[2]), file2zoo(f2[2]))
  expect_identical(file2zoo(f1[3]), file2zoo(f2[3]))
  expect_identical(file2zoo(f1[4]), file2zoo(f2[4]))
})

# check scenario number --------------------------
test_that("Scenario number is correctly output", {
  expect_identical(
    scan(
      file.path(dmi1, "trace1/HydrologyParameters.SupplyScenario"),
      nlines = 1,
      what = "character",
      skip = 1,
      quiet = TRUE
    ),
    "5"
  )
  expect_identical(
    scan(
      file.path(dmi1, "trace2/HydrologyParameters.SupplyScenario"),
      nlines = 1,
      what = "character",
      skip = 1,
      quiet = TRUE
    ),
    "5"
  )
  expect_identical(
    scan(
      file.path(dmi2, "trace1/HydrologyParameters.SupplyScenario"),
      nlines = 1,
      what = "character",
      skip = 1,
      quiet = TRUE
    ),
    "5"
  )
  expect_identical(
    scan(
      file.path(dmi2, "trace2/HydrologyParameters.SupplyScenario"),
      nlines = 1,
      what = "character",
      skip = 1,
      quiet = TRUE
    ),
    "5"
  )
})

# check trace number -------------------------
test_that("Trace number is correctly output", {
  expect_identical(
    scan(
      file.path(dmi1, "trace1/HydrologyParameters.TraceNumber"),
      nlines = 1,
      what = "character",
      skip = 1,
      quiet = TRUE
    ),
    "1"
  )
  expect_identical(
    scan(
      file.path(dmi1, "trace2/HydrologyParameters.TraceNumber"),
      nlines = 1,
      what = "character",
      skip = 1,
      quiet = TRUE
    ),
    "2"
  )
  expect_identical(
    scan(
      file.path(dmi2, "trace1/HydrologyParameters.TraceNumber"),
      nlines = 1,
      what = "character",
      skip = 1,
      quiet = TRUE
    ),
    "1"
  )
  expect_identical(
    scan(
      file.path(dmi2, "trace2/HydrologyParameters.TraceNumber"),
      nlines = 1,
      what = "character",
      skip = 1,
      quiet = TRUE
    ),
    "2"
  )
})

# check hydrologyIncrement ----------------------------
test_that("hydrologyIncrement is correctly saved", {
  expect_identical(
    as.vector(unclass(
      file2zoo(file.path(dmi1, "trace1", getOption("crssio.hydroIncrement")))
    )),
    as.numeric(rep(1:150, each = 12))
  )
  expect_identical(
    as.vector(unclass(
      file2zoo(file.path(dmi1, "trace2", getOption("crssio.hydroIncrement")))
    )),
    as.numeric(rep(2:151, each = 12))
  )
  expect_identical(
    as.vector(unclass(
      file2zoo(file.path(dmi2, "trace1", getOption("crssio.hydroIncrement")))
    )),
    as.numeric(rep(1:10, each = 12))
  )
  expect_identical(
    as.vector(unclass(
      file2zoo(file.path(dmi2, "trace2", getOption("crssio.hydroIncrement")))
    )),
    as.numeric(rep(2:11, each = 12))
  )
})

trimYears <- as.yearmon(paste("Dec", 2020:2029))
# check sacramento year type --------------------------
test_that("sacramento year type data is correctly saved", {
  expect_identical(
    file2zoo(file.path(dmi1, "trace1", sacFile), monthly = FALSE),
    file2zoo(file.path("../ccOrig/trace1", sacFile), monthly = FALSE)
  )
  expect_identical(
    file2zoo(file.path(dmi1, "trace2", sacFile), monthly = FALSE),
    file2zoo(file.path("../ccOrig/trace2", sacFile), monthly = FALSE)
  )
  expect_identical(
    file2zoo(file.path(dmi2, "trace1", sacFile), monthly = FALSE),
    file2zoo(file.path("../ccOrig/trace1", sacFile), monthly = FALSE)[trimYears]
  )
  expect_identical(
    file2zoo(file.path(dmi2, "trace2", sacFile), monthly = FALSE),
    file2zoo(file.path("../ccOrig/trace2", sacFile), monthly = FALSE)[trimYears]
  )
})
