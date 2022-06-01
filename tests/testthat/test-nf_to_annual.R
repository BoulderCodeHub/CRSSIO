library(CoRiverNF)

t1 <- monthlyTot["2010-01/2011-12"]$LeesFerry

# nf_to_annual.xts ------------------------------------------------
test_that("nf_to_annual.xts() works", {
  expect_s3_class(t1_sum <- nf_to_annual(t1), "xts")
  expect_equivalent(t1_sum, cyAnnTot["2010/2011"]$LeesFerry)
  
  expect_equivalent(
    nf_to_annual(monthlyTot["1906-01/"]), 
    cyAnnTot
  )
  expect_equivalent(
    nf_to_annual(monthlyInt["/2018-09"], year = "wy"),
    wyAnnInt["/2018"]
  )
  
  expect_equivalent(nf_to_annual(monthlyInt), nf_to_annual(monthlyInt["1906/"]))
  expect_s3_class(tmp <- nf_to_annual(monthlyInt, full_year = FALSE), "xts")
  expect_equal(start(tmp), zoo::as.yearmon("Dec 1905"))
  expect_equivalent(tmp["1906/"], cyAnnInt)
  expect_s3_class(
    tmp <- nf_to_annual(monthlyInt, year = "wy", full_year = FALSE), "xts"
  )
  expect_equal(
    end(tmp), 
    zoo::as.yearmon(paste("Sep", CRSSIO:::year(end(monthlyInt), TRUE) + 1))
  )
  expect_equivalent(tmp[paste0("/", CRSSIO:::year(end(monthlyInt)))], wyAnnInt)
})

# nf_to_annual.nfd ----------------------------------------------

test_that("nf_to_annual.nfd() works", {
  n1 <- nfd(
    CoRiverNF::monthlyInt, flow_space = "intervening", time_step = "monthly"
  )
  expect_s3_class(n2 <- nf_to_annual(n1), "nfd")
  expect_equivalent(n2$annual$intervening[[1]], CoRiverNF::cyAnnInt)
  
  n2 <- nfd(
    monthlyInt["2000/2005"], flow_space = "intervening", time_step = "monthly", 
    year = "wy"
  )
  expect_equivalent(
    nf_to_annual(n2)$annual$intervening[[1]], 
    CoRiverNF::wyAnnInt["2001/2005"]
  )
  
  n2 <- ism(nfd_trim_ts(n2))
  expect_s3_class(n2 <- nf_to_annual(n2), "nfd")
  expect_equal(
    ism(nfd(
      wyAnnInt["2001/2005"], flow_space = "intervening", time_step = "annual", 
      year = "wy"
    ))$annual$intervening, 
    n2$annual$intervening
  )
})

test_that("nf_to_annual.nfd() handles different year types correctly.", {
  x <- nfd(
    matrix(1:40, ncol = 1), 
    flow_space = 'total', 
    time_step = 'monthly', 
    year = 'cy'
  )
  expect_error(nf_to_annual(x, full_year = TRUE, year = 'wy'))
  expect_error(nf_to_annual(x, full_year = TRUE, year = 'abc'))
  expect_s3_class(nf_to_annual(x, keep_monthly = FALSE, year = 'wy'), 'nfd')
})

# nf_to_annual.crssi ------------------------------------------------
test_that("nf_to_annual.crssi() works", {
  nf <- crss_nf(
    CoRiverNF::monthlyInt["2000/2002"],
    flow_space = "intervening",
    time_step = "monthly"
  )
  sac <- sac_year_type_get(internal = TRUE)["2000/2002"]
  nf <- crssi(nf, sac, scen_number = 1.20002002, scen_name = "abc")
  
  expect_s3_class(nf2 <- nf_to_annual(nf), "crssi")
  expect_equivalent(nf2$annual$intervening[[1]], cyAnnInt["2000/2002"])
  expect_identical(nf2$sac_year_type, nf$sac_year_type)
  expect_identical(nf2$n_trace, nf$n_trace)
  expect_identical(nf2$scen_number, nf$scen_number)
  expect_identical(nf2$scen_name, nf$scen_name)
})
