library(CoRiverNF)
library(dplyr)
library(tidyr)

context("test the internal fuctions for crssi_create_hist_nf_xlsx()")

# by only using pre 1971 data, the test results should not change with updated
# version of CoRiverNF

zz <- CoRiverNF::monthlyInt["/1971"]
nf1 <- sample(nf_gage_abbrv(), 8, replace = FALSE)
nf2 <- sample(nf_gage_abbrv(), 12, replace = FALSE)

test_that("nf_xts_to_df() returns correct data.frame", {
  expect_s3_class(CRSSIO:::nf_xts_to_df(zz), "data.frame")
  expect_length(colnames(CRSSIO:::nf_xts_to_df(zz)), 29 + 2)
  expect_length(
    colnames(CRSSIO:::nf_xts_to_df(zz, nf_gage_abbrv()[1:10])),
    10 + 2
  )
  expect_true(
    all(colnames(CRSSIO:::nf_xts_to_df(zz, nf1)) %in% c(nf1, "year", "month")) &
      all(c(nf1, "year", "month") %in% colnames(CRSSIO:::nf_xts_to_df(zz, nf1)))
  )
  expect_true(
    all(colnames(CRSSIO:::nf_xts_to_df(zz, nf2)) %in% c(nf2, "year", "month")) &
      all(c(nf2, "year", "month") %in% colnames(CRSSIO:::nf_xts_to_df(zz, nf2)))
  )
  expect_equal(nrow(zz), nrow(CRSSIO:::nf_xts_to_df(zz)))
  expect_equal(nrow(zz), nrow(CRSSIO:::nf_xts_to_df(zz, nf1)))
  expect_error(CRSSIO:::nf_xts_to_df(zz, "someBadName"))
})

zz <- CRSSIO:::nf_xts_to_df(zz)
zz1 <- zz %>% select_at(c(nf1, "year", "month"))
zz1Comp <- zz1 %>%
  tail(24) %>%
  select_at(c(nf1[1], "year", "month")) %>%
  spread_("month", nf1[1]) %>%
  summarise_at(.vars = as.character(1:12), .funs = funs(round(mean(.), 0))) %>%
  gather_("month", nf1[1], gather_cols = as.character(1:12)) %>%
  mutate(month = as.numeric(month))

zz2Comp <- zz1 %>%
  tail(120) %>%
  select_at(c(nf1[2], "year", "month")) %>%
  spread_("month", nf1[2]) %>%
  summarise_at(.vars = as.character(1:12), .funs = funs(round(mean(.), 0))) %>%
  gather_("month", nf1[2], gather_cols = as.character(1:12)) %>%
  mutate(month = as.numeric(month))
  

test_that("fill_nf_data_with_avg() returns correct average", {
  # if model start year is at or before the end of the nf data, or just one year
  # after, then nothing should be done, and it just returns the original data 
  # frame
  expect_equal(CRSSIO:::fill_nf_data_with_avg(zz, nf_gage_abbrv(), 1972, 5), zz)
  expect_equal(CRSSIO:::fill_nf_data_with_avg(zz, nf1, 1965, 10), zz)
  # check the data frame returned for a 10-year average
  expect_s3_class(
    tmp <- CRSSIO:::fill_nf_data_with_avg(zz1, nf1, 1973, 2), 
    "data.frame"
  )
  expect_equal(nrow(tmp), nrow(zz1) + 12)
  expect_equivalent(tail(tmp, 12) %>% select_at(c("month", nf1[1])), zz1Comp)
  expect_equal(max(tmp$year), 1972)
  expect_equal(ncol(tmp), ncol(zz1))
  # check the data frame returned for a 10-year average and 3 years of fill 
  expect_s3_class( 
    tmp <- CRSSIO:::fill_nf_data_with_avg(zz1, nf1, 1975, 10), 
    "data.frame"
  )
  expect_equivalent(tail(tmp, 12) %>% select_at(c("month", nf1[2])), zz2Comp)
  # and the last three years should all by equal to one another except for the
  # year column
  expect_equivalent(
    tail(tmp, 12) %>% select(-year),
    tail(tmp, 24) %>% head(12) %>% select(-year)
  )
  expect_equivalent(
    tail(tmp, 12) %>% select(-year),
    tail(tmp, 36) %>% head(12) %>% select(-year)
  )
  expect_equal(max(tmp$year), 1974)
  expect_equal(ncol(tmp), ncol(zz1))
})

context("check that natural flow excel file is correctly created")

setup(crssi_create_hist_nf_xlsx(2015))
nfFile <- getOption("crssio.histNfFile")
teardown(unlink(nfFile))

test_that("Excel file includes proper columns and sheets", {
  expect_error(readxl::read_xlsx(
    "HistoricalNaturalFlow.xlsx", 
    sheet = "badname"
  ))
  expect_equal(ncol(tmp <- readxl::read_xlsx(nfFile, sheet = "README")), 1)
  expect_equal(
    ncol(tmp <- readxl::read_xlsx(nfFile, sheet = "Intervening Natural Flow")),
    6
  )
  expect_equal(
    colnames(tmp), 
    c("month", 
      paste0("HistoricalNaturalFlow.", 
        c("Hoover", "Davis", "Alamo", "Parker", "Imperial"))
    )
  )
  expect_equal(
    ncol(tmp <- readxl::read_xlsx(nfFile, sheet = "Total Natural Flow")),
    2
  )
  expect_equal(
    colnames(tmp), 
    c("month", "HistoricalNaturalFlow.AboveLeesFerry")
  )
})
