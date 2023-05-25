library(zoo)
# trim_ts nfd ----------------------------
sink('nul')
x <- nfd(5, n_months = 15, n_trace = 3, n_sites = 29, flow_space = "both",
         time_step = "both", start_yearmon = "Jan 2020", 
         site_names = nf_gage_abbrv())
x2 <- nfd(5, n_months = 37, n_trace = 1, n_sites = 4, flow_space = "total",
          time_step = "both", start_yearmon = "Nov 2020", year = "wy")
sink()

test_that("trim_ts.nfd works", {
  sink('nul')
  expect_is(x_trim <- nfd_trim_ts(x), "nfd")
  expect_identical(start(x_trim), as.yearmon("Jan 2020"))
  expect_identical(end(x_trim), as.yearmon("Dec 2020"))
  expect_equal(CRSSIO:::n_years(x_trim), 1)
  expect_equal(CRSSIO:::n_months(x_trim), 12)
  expect_equal(CRSSIO:::n_trace(x_trim), 3)
  expect_true(CRSSIO:::has_annual(x_trim))
  expect_true(CRSSIO:::has_monthly(x_trim))
  expect_true(CRSSIO:::has_intervening(x_trim, "annual"))
  expect_true(CRSSIO:::has_intervening(x_trim, "monthly"))
  expect_true(CRSSIO:::has_total(x_trim, "annual"))
  expect_true(CRSSIO:::has_total(x_trim, "monthly"))
  
  expect_is(x_trim <- nfd_trim_ts(x2), "nfd")
  expect_identical(start(x_trim), as.yearmon("Oct 2021"))
  expect_identical(end(x_trim), as.yearmon("Sep 2023"))
  expect_equal(CRSSIO:::n_years(x_trim), 2)
  expect_equal(CRSSIO:::n_months(x_trim), 24)
  expect_equal(CRSSIO:::n_trace(x_trim), 1)
  expect_true(CRSSIO:::has_annual(x_trim))
  expect_true(CRSSIO:::has_monthly(x_trim))
  expect_false(CRSSIO:::has_intervening(x_trim, "annual"))
  expect_false(CRSSIO:::has_intervening(x_trim, "monthly"))
  expect_true(CRSSIO:::has_total(x_trim, "annual"))
  expect_true(CRSSIO:::has_total(x_trim, "monthly"))
  sink()
})

# trim_ts crssi --------------------------
# unsure how a user would get a crssi object that is not exactly trimmed, but
# this function is used in the crssi constructor, so we'll test it out. 

sink('nul')
x <- crssi(crss_nf(CoRiverNF::monthlyInt), CRSSIO:::sacYT, 1.1, "ok")
sac2 <- CRSSIO:::sacYT
sac2 <- CRSSIO:::reindex.xts(sac2, "2000")
# this is done inside crssi() (usually)
zoo::index(sac2) <- zoo::index(sac2) + 3/12 

x2 <- x
x2$sac_year_type <- sac2
x$annual$intervening[[1]] <- CoRiverNF::cyAnnTot["1996/"]
sink()

test_that("nfd_trim.crssi() works", {
  sink('nul')
  expect_is(x_trim <- nfd_trim_ts(x), "crssi")
  expect_identical(start(x_trim), as.yearmon("Jan 1996"))
  expect_identical(end(x_trim), min(end(CRSSIO:::sacYT), end(x)))
  expect_equal(CRSSIO:::n_trace(x), 1)
  
  expect_is(x_trim <- nfd_trim_ts(x2), "crssi")
  expect_identical(start(x_trim), as.yearmon("Jan 2000"))
  expect_identical(end(x_trim), min(end(CRSSIO:::sacYT), end(x)))
  expect_equal(CRSSIO:::n_trace(x), 1)
  sink()
})

# find_overlap_years cy ------------------
test_that("find_overlap_years returns expected values - calendar year", {
  sink('nul')
  foy <- CRSSIO:::find_overlap_years
  # find_overlap_years <- function(mon_ts, ann_ts, year_type)
  
  # Feb 2020 - Dec 2021; Dec 2020 - Dec 2022
  expect_identical(
    foy(as.yearmon("Feb 2020") + 0:23 / 12, as.yearmon("Dec 2020") + 0:1, "cy"),
    c("2021", "2021")
  )
  
  # Jan 2020 - Dec 2022; Dec 2020 - Dec 2022
  expect_identical(
    foy(as.yearmon("Jan 2020") + 0:35 / 12, as.yearmon("Dec 2020") + 0:2, "cy"),
    c("2020", "2022")
  )
  
  # Jan 2020 - Dec 2023; Dec 2020 - Dec 2022
  expect_identical(
    foy(as.yearmon("Jan 2020") + 0:47 / 12, as.yearmon("Dec 2020") + 0:2, "cy"),
    c("2020", "2022")
  )
  
  # Jan 2020- Dec 2022; Dec 2019 - Dec 2023
  expect_identical(
    foy(as.yearmon("Jan 2020") + 0:35 / 12, as.yearmon("Dec 2019") + 0:4, "cy"),
    c("2020", "2022")
  )
  
  # Jan 2020 - Dec 2029; Dec 2022 - Dec 2025
  expect_identical(
    foy(as.yearmon("Jan 2020") + 0:119 / 12, as.yearmon("Dec 2022") + 0:3, "cy"),
    c("2022", "2025")
  )
  
  # March 2020 - Feb 2023; Dec 2020 - Dec 2023
  expect_identical(
    foy(as.yearmon("Mar 2020") + 0:35 / 12, as.yearmon("Dec 2020") + 0:3, "cy"),
    c("2021", "2022")
  )
  
  # March 2020 - Dec 2020; Dec 2020
  expect_error(
    foy(as.yearmon("Mar 2020") + 0:35 / 12, as.yearmon("Dec 2020"), "cy")
  )
  sink()
})

# find_overlap_years wy ------------------
test_that("find_overlap_years works with water year", {
  sink('nul')
  foy <- CRSSIO:::find_overlap_years
  
  # Feb 2020 - Dec 2021; Sep 2020 - Sep 2022
  expect_identical(
    foy(as.yearmon("Feb 2020") + 0:23 / 12, as.yearmon("Sep 2020") + 0:1, "wy"),
    c("2020", "2021")
  )
  
  # October 2019 - Sep 2022; Sep 2020 - Sep 2022
  expect_identical(
    foy(as.yearmon("Oct 2019") + 0:35 / 12, as.yearmon("Sep 2020") + 0:2, "wy"),
    c("2019", "2022")
  )
  
  # Oct 2020 - Sep 2024; Sep 2021 - Sep 2023
  expect_identical(
    foy(as.yearmon("Oct 2020") + 0:47 / 12, as.yearmon("Sep 2021") + 0:2, "wy"),
    c("2020", "2023")
  )
  
  # Oct 2020- Sep 2023; Sep 2019 - Sep 2022
  expect_identical(
    foy(as.yearmon("Oct 2020") + 0:35 / 12, as.yearmon("Sep 2019") + 0:4, "wy"),
    c("2020", "2023")
  )
  
  # Oct 2020 - Sep 2030; Sep 2022 - Dec 2025
  expect_identical(
    foy(as.yearmon("Oct 2020") + 0:119 / 12, as.yearmon("Sep 2022") + 0:3, "wy"),
    c("2021", "2025")
  )
  
  # March 2020 - Feb 2023; Sep 2020 - Sep 2023
  expect_identical(
    foy(as.yearmon("Mar 2020") + 0:35 / 12, as.yearmon("Sep 2020") + 0:3, "wy"),
    c("2020", "2022")
  )
  
  # March 2020 - Dec 2020; Dec 2020
  expect_error(
    foy(as.yearmon("Mar 2020") + 0:35 / 12, as.yearmon("Dec 2020"), "wy")
  )
  sink()
})

# only one timestep of data ---------------------------------------
sink('nul')
xx_mon <- crss_nf(CoRiverNF::monthlyInt["/2000"])
xx_mon2 <- crss_nf(CoRiverNF::monthlyInt["/2000"], year = "wy")
xx_ann <- nfd(50, n_months = 10*12, start_yearmon = "Jan 2000")
xx_ann2 <- nfd(20, n_months = 144, flow_space = "both", year = "wy")
sink()

test_that("nfd_trim_ts() works on data with only one timestep", {
  sink('nul')
  expect_is(x <- nfd_trim_ts(xx_mon), "crss_nf")
  expect_identical(start(x), zoo::as.yearmon("Jan 1906"))
  expect_identical(end(x), zoo::as.yearmon("Dec 2000"))
  
  expect_is(x <- nfd_trim_ts(xx_mon2), "crss_nf")
  expect_identical(start(x), zoo::as.yearmon("Oct 1905"))
  expect_identical(end(x), zoo::as.yearmon("Sep 2000"))
  
  expect_identical(xx_ann, nfd_trim_ts(xx_ann))
  expect_identical(xx_ann2, nfd_trim_ts(xx_ann2))
  sink()
})

# nfd_trim_ts -----------------------------------------------------
x1 <- CoRiverNF::monthlyInt["2010-01/2013-12"]$LeesFerry
x2 <- CoRiverNF::monthlyTot["2010-01/2013-12"]
test_that("nfd_trim_ts.xts() works", {
  sink('nul')
  expect_equal(nfd_trim_ts(x1), x1)
  expect_equal(nfd_trim_ts(x1["2010-03/"]), x1["2011/"])
  expect_equal(nfd_trim_ts(x1["/2013-11"]), x1["/2012"])
  expect_equal(nfd_trim_ts(x1, year = "wy"), x1["2010-10/2013-09"])
  expect_error(nfd_trim_ts(x1["2013"], year = "wy"))
  expect_error(nfd_trim_ts(x1["2013-2/"]))
  
  expect_equal(nfd_trim_ts(x2), x2)
  expect_equal(nfd_trim_ts(x2["2010-03/"]), x2["2011/"])
  expect_equal(nfd_trim_ts(x2["/2013-11"]), x2["/2012"])
  expect_equal(nfd_trim_ts(x2, year = "wy"), x2["2010-10/2013-09"])
  expect_error(nfd_trim_ts(x2["2013"], year = "wy"))
  expect_error(nfd_trim_ts(x2["2013-2/"]))
  
  expect_message(
    expect_equal(nfd_trim_ts(CoRiverNF::cyAnnTot), CoRiverNF::cyAnnTot)
  )
  expect_message(
    expect_equal(nfd_trim_ts(CoRiverNF::wyAnnInt), CoRiverNF::wyAnnInt)
  )
  sink()
})
