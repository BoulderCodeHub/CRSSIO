library(CRSSIO)
context('check that Natural Flow files are created correctly.')

dir.create('tmp')
p1 <- '..'
rr <- sample(1:29, 4) # get 4 random nodes
message(cat('4 random nodes are:',rr))

# because we are using pre- 1971 data, we do not need to regenerate the data
# in the provided trace folders each time the natural flow are updated
test_that('can create files',{
  expect_message(createCRSSDNFInputFiles('CoRiverNF', oFolder = 'tmp', 
                                         startDate = '2017-1-31', simYrs = 5, 
                                         recordToUse = c('1950-01','1954-12')))
})

test_that('files created from "CoRiverNF" are the same as from Excel', {
  expect_equal(as.matrix(read.csv(file.path('tmp/trace1/',CRSSNFInputNames()[rr[1]]),skip=1)),
               as.matrix(read.csv(file.path(p1,'trace1/', CRSSNFInputNames()[rr[1]]),skip=1)))
  expect_equal(as.matrix(read.csv(file.path('tmp/trace5/',CRSSNFInputNames()[rr[1]]),skip=1)),
               as.matrix(read.csv(file.path(p1,'trace5/', CRSSNFInputNames()[rr[1]]),skip=1)))
  expect_equal(as.matrix(read.csv(file.path('tmp/trace2/',CRSSNFInputNames()[rr[2]]),skip=1)),
               as.matrix(read.csv(file.path(p1,'trace2/', CRSSNFInputNames()[rr[2]]),skip=1)))
  expect_equal(as.matrix(read.csv(file.path('tmp/trace3/',CRSSNFInputNames()[rr[3]]),skip=1)),
               as.matrix(read.csv(file.path(p1,'trace3/', CRSSNFInputNames()[rr[3]]),skip=1)))
  expect_equal(as.matrix(read.csv(file.path('tmp/trace4/',CRSSNFInputNames()[rr[4]]),skip=1)),
               as.matrix(read.csv(file.path(p1,'trace4/', CRSSNFInputNames()[rr[4]]),skip=1)))
})

test_that('ism files match each other as expected', {
  expect_equal(as.matrix(read.csv(file.path('tmp/trace1/',CRSSNFInputNames()[rr[1]]),skip=1))[13:24],
               as.matrix(read.csv(file.path(p1,'trace2/', CRSSNFInputNames()[rr[1]]),skip=1))[1:12])
  expect_equal(as.matrix(read.csv(file.path('tmp/trace1/',CRSSNFInputNames()[rr[2]]),skip=1))[49:60],
               as.matrix(read.csv(file.path(p1,'trace5/', CRSSNFInputNames()[rr[2]]),skip=1))[1:12])
})

unlink('tmp',recursive = T) # delete the files that were created in the tests
