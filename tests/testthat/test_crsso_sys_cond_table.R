library(RWDataPlyr)
library(dplyr)

context('Check system condition table creation')

rwa <- sys_cond_rwa()
scenFolder <- "ISM1988_2014,2007Dems,IG,Most"
scenName <- "scen1"
scenPath <- system.file('extdata','Scenario/',package = 'RWDataPlyr')
sysData <- RWDataPlyr::rdf_aggregate(
  rwa,
  rdf_dir = file.path(scenPath, scenFolder),
  scenario = scenName
)

yrs <- 2018:2022
sysCondTable <- crsso_get_sys_cond_table(sysData, yrs)

test_that('object dimensions and attributes are correct', {
  expect_equal(length(sysCondTable), 2)
  expect_equal(names(sysCondTable), c("fullTable", "limitedTable"))
  expect_equal(
    dim(sysCondTable$fullTable), 
    c(length(CRSSIO:::slotNames())+4,length(yrs))
  )
  expect_equal(
    dim(sysCondTable$limitedTable), 
    c(length(CRSSIO:::slotNames())+1,length(yrs))
  )
  # test that when using too few years, you only get back one year of data
  # warning text is checked below
  expect_warning(s2 <- crsso_get_sys_cond_table(sysData, 2016:2019)) 
  expect_equal(dim(s2$fullTable), c(length(CRSSIO:::slotNames()) + 4, 2))
})

s2 <- sysData %>%
  mutate(Scenario = "scen2") %>%
  bind_rows(sysData)

test_that("warnings and errors are as expected", {
  expect_warning(
    crsso_get_sys_cond_table(s2, yrs),
    paste(
      "There are 2 Scenarios in the data.\n",
      "Please note, these scenarios will be averaged together when creating the system conditions table."
    )
  )
  expect_error(
    sysData %>% filter(Variable != "mer748") %>% crsso_get_sys_cond_table(yrs),
    "The following variables are not found in the data frame passed to crsso_get_sys_cond_table():\nmer748",
    fixed = TRUE
  )
  expect_error(
    sysData %>% 
      filter(Variable != "mer748", Variable != "eq") %>% 
      crsso_get_sys_cond_table(yrs),
    "The following variables are not found in the data frame passed to crsso_get_sys_cond_table():\nmer748, eq",
    fixed = TRUE
  )
  expect_warning(
    crsso_get_sys_cond_table(sysData, 2016:2018),
    paste(
      "All years (yrs) are not in the data frame passed to crsso_get_sys_cond_table()",
      "Will only evaluate for the years that are in the data frame",
      sep = "\n"
    ),
    fixed = TRUE
  )
  expect_error(
    crsso_get_sys_cond_table(sysData, 1999:2005), 
    "None of the yrs exist in the data"
  )
})

expVals <- matrix(
  c(
    rep(0, 5), # EQ
    rep(0, 5), # EQ > 8.23
    rep(0, 5), # EQ = 8.23
    c(100, 100, 25, 25, 50), # UEB
    c(100, 100, 0, 0, 50), # UEB > 8.23
    c(0, 0, 25, 25, 0),  # UEB = 8.23
    rep(0, 5), # UEB < 8.23
    c(0, 0, 75, 75, 25), # Mid
    rep(0, 5), # Mid = 8.23
    c(0, 0, 75, 75, 25), # Mid = 7.48
    rep(0, 4), 25, # LEB
    rep(0, 5), # LEB > 8.23
    rep(0, 5), # LEB = 8.23
    rep(0, 4), 25, # LEB < 8.23
    rep(0, 5), # surplus
    rep(0, 5), # FC Surplus
    c(100, 50, 25, 25, 0), # Normal
    c(0, 50, 75, 75, 100), # short any
    c(0, 50, 75, 25, 50), # short 1
    c(0, 0, 0, 50, 50), # short 2
    rep(0, 5) # short 3
  ),
  ncol = 5, 
  byrow = T
)

# think that checking against the orig values, and then selecting a few values
# to recompute should test it enough
# check UEB Total, Shortage 1, MER 7.48 and normal year
r1 <- apply(
  {
    rdf_get_slot(
      RWDataPlyr::sysRdf, 
      "SummaryOutputData.UpperBalancingAbove823"
    ) +
    rdf_get_slot(
      RWDataPlyr::sysRdf, 
      "SummaryOutputData.UpperBalancingAt823"
    ) +
    rdf_get_slot(
      RWDataPlyr::sysRdf, 
      "SummaryOutputData.UpperBalancingBelow823"
    )
  },
  1,
  mean
) * 100

r2 <- apply(
  rdf_get_slot(RWDataPlyr::sysRdf, "SummaryOutputData.LBShortageStep1"),
  1,
  mean
) * 100

r3 <- apply(
  rdf_get_slot(
    RWDataPlyr::sysRdf, 
    "SummaryOutputData.MidElevationReleaseAt748"
  ),
  1,
  mean
) * 100

r4 <- apply(
  rdf_get_slot(RWDataPlyr::sysRdf, "SummaryOutputData.LBNormalCondition"),
  1,
  mean
) * 100

test_that("computations of chances are correct", {
  expect_equivalent(expVals, sysCondTable$fullTable)
  # UEB Total
  expect_equivalent(sysCondTable$fullTable[4,], r1)
  # Shortage 1
  expect_equivalent(sysCondTable$fullTable[19,], r2)
  # MER 7.48
  expect_equivalent(sysCondTable$fullTable[10,], r3)
  # normal year
  expect_equivalent(sysCondTable$fullTable[17,], r4)
})

test_that("rows sum together correctly", {
  # EQ
  expect_equal(
    sysCondTable$fullTable[1,], 
    apply(sysCondTable$fullTable[2:3,], 2, sum)
  )
  # UEB
  expect_equal(
    sysCondTable$fullTable[4,], 
    apply(sysCondTable$fullTable[5:7,], 2, sum)
  )
  # MER
  expect_equal(
    sysCondTable$fullTable[8,], 
    apply(sysCondTable$fullTable[9:10,], 2, sum)
  )
  # LEB
  expect_equal(
    sysCondTable$fullTable[11,], 
    apply(sysCondTable$fullTable[12:14,], 2, sum)
  )
  # Shortage
  expect_equal(
    sysCondTable$fullTable[18,], 
    apply(sysCondTable$fullTable[19:21,], 2, sum)
  )
  # All Powell Tiers should sum to 100
  expect_equivalent(
    rep(100, 5), 
    apply(sysCondTable$fullTable[c(1, 4, 8, 11),], 2, sum)
  )
  # All Lower Basin Tiers should sum to 100
  expect_equivalent(
    rep(100, 5), 
    apply(sysCondTable$fullTable[c(15, 17, 18),], 2, sum)
  )
  # FC surplus should always be <= the total surplus
  expect_true(all(sysCondTable$fullTable[16,] <= sysCondTable$fullTable[15,]))
})

# compare both versions of the function ---------------------
test_that("both functions are equal", {
  expect_warning(tmp <- createSysCondTable(sysData, yrs))
  expect_identical(crsso_get_sys_cond_table(sysData, yrs), tmp)
})
