library(CRSSIO)
context('check that Natural Flow files are created correctly.')

setup({
  dir.create('tmp')
  dir.create("tmp2")
})
teardown({
  unlink('tmp',recursive = TRUE)
  unlink("tmp2", recursive = TRUE)
})

p1 <- '..'
rr <- sample(1:29, 4) # get 4 random nodes
message(cat('\n4 random nodes are:',rr))

test_that("Upfront errors post correctly", {
  expect_error(
    crssi_create_dnf_files(
      'CoRiverNF', 
      oFolder = 'doesNotExist', 
      startYear = 2017, 
      endYear = 2021, 
      recordToUse = c('1950-01','1954-12')
    ),
    paste0(file.path("doesNotExist"), " folder does not exist.", "\n", 
           "Create the directory before calling crssi_create_dnf_files()")
  )
})

# because we are using pre- 1971 data, we do not need to regenerate the data
# in the provided trace folders each time the natural flow are updated
test_that('can create files',{
  expect_warning(expect_message(
    createCRSSDNFInputFiles(
      'CoRiverNF', 
      oFolder = 'tmp', 
      startDate = '2017-1-31', 
      simYrs = 5, 
      recordToUse = c('1950-01','1954-12')
    )
  ))
  
  expect_message(
    crssi_create_dnf_files(
      'CoRiverNF', 
      oFolder = 'tmp2', 
      startYear = 2017, 
      endYear = 2021, 
      recordToUse = c('1950-01','1954-12')
    )
  )
})

# check that all files in the two directories are the same

dirs <- list.dirs('tmp', recursive = FALSE, full.names = FALSE)
for(curDir in dirs){
  allFiles <- list.files(file.path("tmp", curDir))
  for(ff in allFiles){
    message(curDir, "/", ff)
    test_that("all files are the same", {
      expect_identical(
        scan(file.path("tmp", curDir, ff), what = "character", quiet = TRUE),
        scan(file.path("tmp2", curDir, ff), what = "character", quiet = TRUE),
        info = paste(curDir, ff)
      )
    })
  }
}

allFiles <- c(CRSSNFInputNames(), "MWD_ICS.SacWYType", 
              "MeadFloodControlData.hydrologyIncrement", "HydrologyParameters.TraceNumber",
              "HydrologyParameters.SupplyScenario")

test_that("all files exist", {
  expect_true(all(allFiles %in% list.files("tmp/trace1")))
  expect_true(all(allFiles %in% list.files("tmp/trace3")))
  expect_true(all(list.files("tmp/trace1") %in% allFiles))
  expect_true(all(list.files("tmp/trace3") %in% allFiles))
})

test_that('files created from "CoRiverNF" are the same as from Excel', {
  expect_equal(
    as.matrix(read.csv(
      file.path('tmp/trace1/',CRSSNFInputNames()[rr[1]]),
      skip = 1
    )),
    as.matrix(read.csv(
      file.path(p1,'trace1/', CRSSNFInputNames()[rr[1]]), 
      skip = 1
    ))
  )
  expect_equal(
    as.matrix(read.csv(
      file.path('tmp/trace5/',CRSSNFInputNames()[rr[1]]),
      skip = 1
    )),
    as.matrix(read.csv(
      file.path(p1,'trace5/', CRSSNFInputNames()[rr[1]]),
      skip = 1
    ))
  )
  expect_equal(
    as.matrix(read.csv(
      file.path('tmp/trace2/',CRSSNFInputNames()[rr[2]]),
      skip = 1
    )),
    as.matrix(read.csv(
      file.path(p1,'trace2/', CRSSNFInputNames()[rr[2]]),
      skip = 1
    ))
  )
  expect_equal(
    as.matrix(read.csv(
      file.path('tmp/trace3/',CRSSNFInputNames()[rr[3]]),
      skip = 1
    )),
    as.matrix(read.csv(
      file.path(p1,'trace3/', CRSSNFInputNames()[rr[3]]),
      skip = 1
    ))
  )
  expect_equal(
    as.matrix(read.csv(
      file.path('tmp/trace4/',CRSSNFInputNames()[rr[4]]),
      skip = 1
    )),
    as.matrix(read.csv(
      file.path(p1,'trace4/', CRSSNFInputNames()[rr[4]]),
      skip = 1
    ))
  )
})

test_that('ism files match each other as expected', {
  expect_equal(
    as.matrix(read.csv(
      file.path('tmp/trace1/',CRSSNFInputNames()[rr[1]]),
      skip = 1
    ))[13:24],
    as.matrix(read.csv(
      file.path(p1,'trace2/', CRSSNFInputNames()[rr[1]]),
      skip = 1
    ))[1:12]
  )
  expect_equal(
    as.matrix(read.csv(
      file.path('tmp/trace1/',CRSSNFInputNames()[rr[2]]),
      skip = 1
    ))[49:60],
    as.matrix(read.csv(
      file.path(p1,'trace5/', CRSSNFInputNames()[rr[2]]),
      skip = 1
    ))[1:12]
  )
})

