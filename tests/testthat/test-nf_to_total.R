cy_nf <- nfd(
  CoRiverNF::cyAnnInt, 
  flow_space = "intervening", 
  time_step = "annual",
  n_sites = 29
)

int_nf <- nfd(
  CoRiverNF::monthlyInt["1906/"], 
  flow_space = "intervening", 
  time_step = "monthly", 
  n_sites = 29
)

zz_exp <- ism(nfd(
  CoRiverNF::monthlyTot["2000/2004"],
  flow_space = "total", 
  time_step = "monthly",
  n_sites = 29
))

# nfd --------------------------------------------------------
test_that("can convert to total nf correctly for nfd object", {
  expect_equal(
    zoo::coredata(CoRiverNF::cyAnnTot), 
    zoo::coredata(nfd_get_trace(nf_to_total(cy_nf), 1, "total", "annual"))
  )
  
  expect_equal(
    zoo::coredata(CoRiverNF::monthlyTot["1906/"]),
    zoo::coredata(nfd_get_trace(nf_to_total(int_nf), 1, "total", "monthly"))
  )
  
  # check it works when ism has been applied
  int_nf <- ism(nfd(
    CoRiverNF::monthlyInt["1988/"],
    flow_space = "intervening",
    time_step = "monthly",
    n_sites = 29
  ))
  tot_nf <- ism(nfd(
    CoRiverNF::monthlyTot["1988/"], 
    flow_space = "total", 
    time_step = "monthly", 
    n_sites = 29
  ))
  
  expect_equal(tot_nf, nf_to_total(int_nf, keep_intervening = FALSE))
  
  # keep_intervening = TRUE works
  zz <- nf_to_total(int_nf, keep_intervening = TRUE)
  
  expect_equal(zz$monthly$intervening, int_nf$monthly$intervening)
  
  # works for annual and monthly included in same nfd object
  zz <- ism(nfd(
    CoRiverNF::monthlyInt["2000/2004"],
    flow_space = "intervening", 
    time_step = "monthly",
    n_sites = 29
  ))

  z2 <- ism(nfd(
    CoRiverNF::cyAnnInt["2000/2004"],
    flow_space = "intervening",
    time_step = "annual",
    n_sites = 29
  ))
  
  z2_exp <- ism(nfd(
    CoRiverNF::cyAnnTot["2000/2004"],
    flow_space = "total",
    time_step = "annual",
    n_sites = 29
  ))
  
  zz$annual$intervening <- z2$annual$intervening
  zz <- nf_to_total(zz)
  
  expect_equal(zz_exp$monthly$total, zz$monthly$total)
  
  expect_equal(z2_exp$annual$total, zz$annual$total)
  
  # fails to work with default recompute, but succeeds with recompute = true
  expect_error(nf_to_total(zz))
  expect_identical(zz, nf_to_total(zz, recompute = TRUE))
})

# crss_nf -----------------------------------------------------------------
int_nf <- ism(crss_nf(
  CoRiverNF::monthlyInt["2000/2004"], 
  flow_space = "intervening", 
  time_step = "monthly"
))

test_that("nf_to_total.crss_nf works", {
  tmp <- nf_to_total(int_nf)
  expect_equal(zz_exp$monthly$total, tmp$monthly$total)
  expect_true(CRSSIO:::has_intervening(tmp, "monthly"))
  expect_true(CRSSIO:::has_total(tmp, "monthly"))
  expect_s3_class(tmp, "crss_nf")
  
  expect_warning(tmp <- nf_to_total(int_nf, keep_intervening = FALSE))
  expect_equal(zz_exp$monthly$total, tmp$monthly$total)
  expect_s3_class(tmp, "nfd")
})

# crssi -------------------------------------------------------------------
sac <- ism(sac_year_type_get(TRUE)["2000/2004"])
int_nf <- crssi(int_nf, sac, 1)
test_that("nf_to_total.crssi works", {
  tmp <- nf_to_total(int_nf)
  expect_s3_class(tmp, "crssi")
  expect_equal(zz_exp$monthly$total, tmp$monthly$total)
})

# xts ---------------------------------------------------------------------
test_that("nf_to_total.xts works", {
  expect_equal(
    zoo::coredata(CoRiverNF::cyAnnTot), 
    zoo::coredata(nf_to_total(CoRiverNF::cyAnnInt))
  )
  expect_equal(
    zoo::coredata(CoRiverNF::monthlyTot["2000/2005"]), 
    zoo::coredata(nf_to_total(CoRiverNF::monthlyInt["2000/2005"]))
  )
})
