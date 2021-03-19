cy_nfd <- nfd(
  CoRiverNF::cyAnnTot["2000/2005"],
  flow_space = "total", 
  time_step = "annual",
  n_sites = 29
)

mon_nfd <- nfd(
  CoRiverNF::monthlyTot["1950/1954"],
  flow_space = "total", 
  time_step = "monthly",
  n_sites = 29
)

ism_nfd_exp <- ism(nfd(
  CoRiverNF::monthlyInt["1950/1954"],
  flow_space = "intervening",
  time_step = "monthly",
  n_sites = 29
))

# nfd -----------------------------------------------------
test_that("can convert from total to interveing for nfd object.", {
  expect_equal(
    zoo::coredata(CoRiverNF::cyAnnInt["2000/2005"]),
    zoo::coredata(nfd_get_trace(
      nf_to_intervening(cy_nfd), 1, "intervening", "annual"
    ))
  )
  
  expect_equal(
    zoo::coredata(CoRiverNF::monthlyInt["1950/1954"]),
    zoo::coredata(nfd_get_trace(
      nf_to_intervening(mon_nfd), 1, "intervening", "monthly"
    ))
  )
  
  # check w/ISM
  ism_tot <- ism(mon_nfd)
  expect_equal(ism_nfd_exp, nf_to_intervening(ism_tot, keep_total = FALSE))
  
  # keep_total = TRUE works
  tmp <- nf_to_intervening(ism_tot, keep_total = TRUE)
  expect_equal(tmp$monthly$total, ism_tot$monthly$total)
  
  # works for annual and monthly included in same nfd object
  tmp <- ism_tot
  tmp$annual$total <- ism(nfd(
    CoRiverNF::cyAnnTot["1950/1954"], 
    flow_space = "total",
    time_step = "annual", 
    n_sites = 29
  ))$annual$total
  
  tmp_exp <- ism(nfd(
    CoRiverNF::cyAnnInt["1950/1954"],
    flow_space = "intervening",
    time_step = "annual",
    n_sites = 29
  ))
  
  tmp <- nf_to_intervening(tmp, keep_total = TRUE)
  expect_equal(ism_nfd_exp$monthly$intervening, tmp$monthly$intervening)
  expect_equal(tmp_exp$annual$intervening, tmp$annual$intervening)
  expect_equal(tmp$monthly$total, ism_tot$monthly$total)
  
  # fails to work with recompute = FALSE but works with recompute = true
  expect_error(nf_to_intervening(tmp))
  expect_identical(tmp, nf_to_intervening(tmp, recompute = TRUE))
})

# crss_nf -----------------------------------------------------------

nf <- as_crss_nf(nf_to_intervening(mon_nfd, keep_total = TRUE))

test_that("nf_to_intervening.crss_nf works", {
  # should fail with default recompute
  expect_error(nf_to_intervening(nf))
  
  # should match input with recompute = TRUE and keep_total = TRUE
  expect_identical(
    tmp <- nf_to_intervening(nf, keep_total = TRUE, recompute = TRUE), 
    nf
  )
  expect_s3_class(tmp, "crss_nf")
  
  # and should match input, though not have total with default keep_total
  expect_equal(
    nf_to_intervening(nf, recompute = TRUE)$monthly$intervening,
    nf$monthly$intervening
  )
})

# crssi ---------------------------------------------------------------
sac <- ism(sac_year_type_get(TRUE)["1950/1954"])
nf <- crssi(ism(nf), sac, 222, drop_flow = FALSE)
test_that("nf_to_intervening.crssi works", {
  tmp <- nf_to_intervening(nf, recompute = TRUE)
  expect_s3_class(tmp, "crssi")
  expect_equal(tmp$monthly$intervening, nf$monthly$intervening)
})

# xts -----------------------------------------------------------------
test_that("nf_to_intervening.xts works", {
  expect_equal(
    zoo::coredata(CoRiverNF::cyAnnInt),
    zoo::coredata(nf_to_intervening(CoRiverNF::cyAnnTot))
  )
  
  expect_equal(
    zoo::coredata(CoRiverNF::monthlyInt["1935/1940"]),
    zoo::coredata(nf_to_intervening(CoRiverNF::monthlyTot["1935/1940"]))
  )
})
