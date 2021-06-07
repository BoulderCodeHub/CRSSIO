# setup ---------------
mon_nfd <- as_nfd(CoRiverNF::monthlyInt, flow_space = "intervening", 
                  time_step = "monthly")

# from scalar -----------------------------
test_that("crss_nf() works with scalar", {
  expect_is(
    x <- crss_nf(40, n_months = 24, n_trace = 5, flow_space = "both", 
                 start_yearmon = "Jan 2020"),
    "crss_nf"
  )
  
  expect_true(CRSSIO:::has_intervening(x, "monthly"))
  expect_true(CRSSIO:::has_total(x, "monthly"))
  expect_error(crss_nf(40, flow_space = "total"))
})

# from nfd() ------------------------------
test_that("crss_nf() works with nfd", {
  expect_is(x <- crss_nf(mon_nfd), "crss_nf")
  expect_error(crss_nf(as_nfd(CoRiverNF::cyAnnTot)))
})

# from array  -----------------------------
test_that("crss_nf() works with array", {
  nf_array <- array(
    40, c(37, 12, 29), dimnames = list(NULL, NULL, nf_gage_abbrv())
  )
  
  expect_warning(expect_is(x <- crss_nf(nf_array), "crss_nf"))
  expect_identical(
    crss_nf(nf_array, flow_space = "intervening", 
            time_step = "monthly", n_trace = 12), 
    x
  )
  expect_identical(
    as_crss_nf(as_nfd(
      array(40, c(37, 12, 29)), 
      flow_space = "intervening", 
      time_step = "monthly",
      site_names = nf_gage_abbrv()
    )), 
    x
  )
  # TODO: this array is unnamed, so it should throw an error
  expect_error(expect_warning(crss_nf(array(40, c(37, 12, 1)))))
  expect_error(expect_warning(crss_nf(array(40, c(37, 12, 29)), n_trace = 12)))
})

# from matrix -----------------------------
test_that("crss_nf() works with matrix", {
  nf_mat <- matrix(1:2900, ncol = 29, dimnames = list(NULL, nf_gage_abbrv()))
  expect_is(x <- crss_nf(nf_mat), "crss_nf")
  expect_identical(
    crss_nf(
      nf_mat, 
      flow_space = "intervening", 
      time_step = "monthly"
    ), 
    x
  )
  expect_identical(
    as_crss_nf(nfd(
      matrix(1:2900, ncol = 29), 
      flow_space = "intervening", 
      time_step = "monthly", 
      site_names = nf_gage_abbrv(),
      n_sites = 29
    )),
    x
  )
  expect_error(expect_warning(crss_nf(matrix(1:100, ncol = 10))))
  expect_error(crss_nf(matrix(1:2900, ncol = 29)))
})

# from xts --------------------------------
test_that("crss_nf() works with xts", {
  tmp <- CoRiverNF::monthlyInt
  expect_is(x <- crss_nf(tmp), "crss_nf")
  expect_identical(
    crss_nf(
      tmp, 
      flow_space = "intervening", 
      time_step = "monthly"
    ),
    x
  )
  expect_error(expect_warning(crss_nf(tmp[,"LeesFerry"])))
  colnames(tmp) <- NULL
  expect_error(expect_warning(crss_nf(tmp)))
})

# from data.frame ----------------------------------
test_that("crss_nf() works with data.frame", {
  tmp_df <- as.data.frame(CoRiverNF::monthlyInt["1906/"])
  tmp_df$tmp <- zoo::as.yearmon(rownames(tmp_df))
  tmp_df$month <- month.abb[as.numeric(CRSSIO:::month(tmp_df$tmp))]
  tmp_df$year <- as.numeric(CRSSIO:::year(tmp_df$tmp))
  tmp_df$tmp <- NULL
  
  expect_s3_class(
    expect_output(tmp <- as_crss_nf(
      tmp_df, flow_space = "intervening", time_step = "monthly"
    )), 
    "crss_nf"
  )
  expect_equal(
    as_nfd(tmp), 
    expect_output(nfd(tmp_df, flow_space = "intervening", time_step = "monthly"))
  )
  
  tmp_df$Cameo <- NULL
  expect_error(
    as_crss_nf(tmp_df, flow_space = "intervening", time_step = "monthly")
  )
})
