library(zoo)
# find_overlap_years ------------------

test_that("find_overlap_years returns expected values", {
  foy <- CRSSIO:::find_overlap_years
  #find_overlap_years <- function(flow_time, sac_time)
  
  # Feb 2020 - Dec 2021; Dec 2020 - Dec 2022
  expect_identical(
    foy(as.yearmon("Feb 2020") + 0:23/12, as.yearmon("Dec 2020") + 0:1),
    c("2021", "2021")
  )
  
  # Jan 2020 - Dec 2022; Dec 2020 - Dec 2022
  expect_identical(
    foy(as.yearmon("Jan 2020") + 0:35/12, as.yearmon("Dec 2020") + 0:2),
    c("2020", "2022")
  )
  
  # Jan 2020 - Dec 2023; Dec 2020 - Dec 2022
  expect_identical(
    foy(as.yearmon("Jan 2020") + 0:47/12, as.yearmon("Dec 2020") + 0:2),
    c("2020", "2022")
  )
  
  # Jan 2020- Dec 2022; Dec 2019 - Dec 2023
  expect_identical(
    foy(as.yearmon("Jan 2020") + 0:35/12, as.yearmon("Dec 2019") + 0:4),
    c("2020", "2022")
  )
  
  # Jan 2020 - Dec 2029; Dec 2022 - Dec 2025
  expect_identical(
    foy(as.yearmon("Jan 2020") + 0:119/12, as.yearmon("Dec 2022") + 0:3),
    c("2022", "2025")
  )
  
  # March 2020 - Feb 2023; Dec 2020 - Dec 2023
  expect_identical(
    foy(as.yearmon("Mar 2020") + 0:35/12, as.yearmon("Dec 2020") + 0:3),
    c("2021", "2022")
  )
  
  # March 2020 - Dec 2020; Dec 2020
  expect_error(
    foy(as.yearmon("Mar 2020") + 0:35/12, as.yearmon("Dec 2020"))
  )
})
