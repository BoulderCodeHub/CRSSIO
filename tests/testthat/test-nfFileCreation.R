
# setup temp folders ---------------------------
temp_folder <- tempdir()
dir2 <- file.path(temp_folder, "tmp2")
dir3 <- file.path(temp_folder, "tmp3")

setup({
  dir.create(dir2)
  dir.create(dir3)
})
teardown({
  unlink(dir2, recursive = TRUE)
  unlink(dir3, recursive = TRUE)
})

p1 <- ".."
rr <- sample(1:29, 4) # get 4 random nodes
message('\n4 random nodes are: ',paste(rr, collapse = " "))
r2u <- zoo::as.yearmon(c('1950-01','1954-12'))

# check errors -------------------
test_that("Upfront errors post correctly", {
  expect_error(
    crssi_create_dnf_files(
      'CoRiverNF', 
      oFolder = 'doesNotExist', 
      startYear = 2017, 
      endYear = 2021, 
      recordToUse = r2u
    ),
    paste0(file.path("doesNotExist"), " folder does not exist.", "\n", 
           "Create the directory before calling crssi_create_dnf_files()")
  )
  expect_error(
    crssi_create_dnf_files(
      "CoRiverNF.txt", 
      oFolder = 'doesNotExist', 
      startYear = 2017, 
      endYear = 2021, 
      recordToUse = r2u
    ),
    paste0("CoRiverNF.txt does not appear to be valid.\n", 
         "It should be either an Excel (xlsx) file or 'CoRiverNF'"),
    fixed = TRUE
  )
  
  expect_error(
    crssi_create_dnf_files(
      'CoRiverNF', 
      oFolder = dir2, 
      startYear = 2017, 
      endYear = 2021, 
      recordToUse = c("1906-01", "1997-12")
    ),
    "recordToUse must be class 'yearmon'."
  )
  
  expect_error(
    crssi_create_dnf_files(
      'CoRiverNF', 
      oFolder = dir2, 
      startYear = 2017, 
      endYear = 2021, 
      recordToUse = zoo::as.yearmon(c("1906-01", "1997-12", "1999-12"))
    ),
    "recordToUse should only contain two entries, or be 'NA'."
  )
  
  expect_error(
    crssi_create_dnf_files(
      'CoRiverNF', 
      oFolder = dir2, 
      startYear = 2017, 
      endYear = 2021, 
      recordToUse = zoo::as.yearmon(c("1906-2", "1997-12"))
    ),
    "The first entry to recordToUse should be January of some year."
  )
  
  expect_error(
    crssi_create_dnf_files(
      'CoRiverNF', 
      oFolder = dir2, 
      startYear = 2017, 
      endYear = 2021, 
      recordToUse = zoo::as.yearmon(c("1906-1", "1997-11"))
    ),
    "The second entry to recordToUse should be December of some year."
  )
  
  expect_error(
    crssi_create_dnf_files(
      'CoRiverNF', 
      oFolder = dir2, 
      startYear = 2017, 
      endYear = 2021, 
      recordToUse = zoo::as.yearmon(c("1905-1", "1997-12"))
    ),
    "Years in recordToUse should not be before 1906."
  )
  
  expect_error(
    crssi_create_dnf_files(
      'CoRiverNF', 
      oFolder = dir2, 
      startYear = 2017, 
      endYear = 2021, 
      recordToUse = zoo::as.yearmon(c("1906-1", "1905-12"))
    ),
    "Years in recordToUse should not be before 1906."
  )
  
  expect_error(
    crssi_create_dnf_files(
      'CoRiverNF', 
      oFolder = dir2, 
      startYear = 2017, 
      endYear = 2021, 
      recordToUse = zoo::as.yearmon(c("1988-1", "1980-12"))
    ),
    "The second entry in recordToUse should be after the first entry."
  )
  
  expect_error(
    crssi_create_dnf_files(
      'CoRiverNF', 
      oFolder = dir2, 
      startYear = 2017, 
      endYear = 2021, 
      recordToUse = zoo::as.yearmon(c("1988-1", "2100-12"))
    ),
    paste(
      "The end year in `recordToUse` must be <=", 
      as.integer(format(tail(zoo::index(CoRiverNF::monthlyInt), 1), "%Y"))
    )
  )
})

# check the two different fucntions create data -------
# because we are using pre- 1971 data, we do not need to regenerate the data
# in the provided trace folders each time the natural flow are updated
test_that('can create files',{
  sink('nul')
  expect_message(
    crssi_create_dnf_files(
      'CoRiverNF', 
      oFolder = dir2, 
      startYear = 2017, 
      endYear = 2021, 
      recordToUse = r2u
    )
  )
  
  expect_warning(expect_message(
    crssi_create_dnf_files(
      "../NaturalFlows_Sample.xlsx",
      oFolder = dir3,
      startYear = 2017,
      endYear = 2021,
      recordToUse = r2u
    )
  ))
  sink()
})

# check that all files in the three directories are the same -------------

dirs <- list.dirs(dir2, recursive = FALSE, full.names = FALSE)
test_that("all files are the same", {
  for(curDir in dirs){
    allFiles <- list.files(file.path(dir2, curDir))
    for(ff in allFiles){
      #message(curDir, "/", ff)
      expect_identical(
        scan(file.path(dir2, curDir, ff), what = "character", quiet = TRUE),
        scan(file.path(dir3, curDir, ff), what = "character", quiet = TRUE),
        info = paste(curDir, ff)
      )
    }
  }
})

allFiles <- c(nf_file_names(6), "MWD_ICS.SacWYType", 
              "MeadFloodControlData.hydrologyIncrement", 
              "HydrologyParameters.TraceNumber",
              "HydrologyParameters.SupplyScenario")

test_that("all files exist", {
  expect_setequal(allFiles, list.files(file.path(dir2, "trace1")))
  expect_setequal(allFiles, list.files(file.path(dir2, "trace3")))
})

test_that('files created from "CoRiverNF" are the same as from Excel', {
  expect_equal(
    as.matrix(read.csv(
      file.path(dir2, 'trace1',nf_file_names(6)[rr[1]]),
      skip = 1
    )),
    as.matrix(read.csv(
      file.path(p1,'trace1', nf_file_names(6)[rr[1]]), 
      skip = 1
    ))
  )
  expect_equal(
    as.matrix(read.csv(
      file.path(dir2, 'trace5',nf_file_names(6)[rr[1]]),
      skip = 1
    )),
    as.matrix(read.csv(
      file.path(p1,'trace5', nf_file_names(6)[rr[1]]),
      skip = 1
    ))
  )
  expect_equal(
    as.matrix(read.csv(
      file.path(dir2, 'trace2',nf_file_names(6)[rr[2]]),
      skip = 1
    )),
    as.matrix(read.csv(
      file.path(p1,'trace2', nf_file_names(6)[rr[2]]),
      skip = 1
    ))
  )
  expect_equal(
    as.matrix(read.csv(
      file.path(dir2, 'trace3',nf_file_names(6)[rr[3]]),
      skip = 1
    )),
    as.matrix(read.csv(
      file.path(p1,'trace3', nf_file_names(6)[rr[3]]),
      skip = 1
    ))
  )
  expect_equal(
    as.matrix(read.csv(
      file.path(dir2, 'trace4',nf_file_names(6)[rr[4]]),
      skip = 1
    )),
    as.matrix(read.csv(
      file.path(p1,'trace4', nf_file_names(6)[rr[4]]),
      skip = 1
    ))
  )
})

test_that('ism files match each other as expected', {
  expect_equal(
    as.matrix(read.csv(
      file.path(dir2, 'trace1',nf_file_names(6)[rr[1]]),
      skip = 1
    ))[13:24],
    as.matrix(read.csv(
      file.path(p1,'trace2', nf_file_names(6)[rr[1]]),
      skip = 1
    ))[1:12]
  )
  expect_equal(
    as.matrix(read.csv(
      file.path(dir2, 'trace1',nf_file_names(6)[rr[2]]),
      skip = 1
    ))[49:60],
    as.matrix(read.csv(
      file.path(p1,'trace5', nf_file_names(6)[rr[2]]),
      skip = 1
    ))[1:12]
  )
  expect_equal(
    as.matrix(read.csv(
      file.path(dir2, 'trace1',nf_file_names(6)[rr[2]]),
      skip = 1
    ))[1:12],
    as.matrix(read.csv(
      file.path(p1,'trace4', nf_file_names(6)[rr[2]]),
      skip = 1
    ))[25:36]
  )
})

# internal format excel function -------------------

test_that("Excel formatting works", {
  expect_warning(expect_s3_class(
    tmp <- CRSSIO:::read_and_format_nf_excel("../NaturalFlows_Sample.xlsx"),
    c("xts", "zoo")
  ))
  expect_equal(ncol(tmp), 29)
  expect_equal(nrow(tmp) %% 12, 0)
  expect_equal(zoo::index(tmp)[1], zoo::as.yearmon("Jan 1906"))
  expect_equal(format(tail(zoo::index(tmp),1), "%b"), "Dec")
})

