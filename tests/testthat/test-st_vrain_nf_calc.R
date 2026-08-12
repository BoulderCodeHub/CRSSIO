# cy_vol specified -------------------
vv <- c(
  3574922,
  1158101,
  1738419,
  2849181,
  2219415,
  2106977,
  2181450,
  1474316,
  2550862,
  1714823
  )
yy <- 2011:2020

yy <- zoo::as.yearmon(paste("Dec", yy))

cy_vol <- xts::xts(vv, yy)

test_that("st_vrain_nf_calc() works with xts object.", {
  expect_s3_class(tmp <- st_vrain_nf_calc(cy_vol = cy_vol), "xts")

  # values are the same if multiple columns are specified
  cy2 <- cbind(cy_vol, cy_vol, cy_vol)
  expect_s3_class(tmp <- st_vrain_nf_calc(cy_vol = cy2), "xts")
  expect_equal(ncol(tmp), 3)
  expect_true(all(tmp[,1] == tmp[,2]))
  expect_true(all(tmp[,2] == tmp[,3]))
})

# co_tot_nf specified ---------------------

test_that("st_vrain_nf_calc() works with nfd", {
  sink('nul')
  nf <- nfd(CoRiverNF::cyAnnTot, flow_space = "total", 
            time_step = "annual",
            year = "cy")
  
  expect_s3_class(tmp <- st_vrain_nf_calc(co_tot_nf = nf), "xts")
  expect_equal(ncol(tmp), 1)
  expect_equal(nrow(tmp), CRSSIO:::n_years(nf))

  # and it works with ism
  nf2 <- nfd(CoRiverNF::cyAnnTot["1988/"], flow_space = "total", 
             time_step = "annual", year = "cy")
  nf2 <- ism(nf2)
  
  expect_s3_class(tmp <- st_vrain_nf_calc(co_tot_nf = nf2), "xts")
  expect_equal(ncol(tmp), CRSSIO:::n_trace(nf2))
  expect_equal(nrow(tmp), CRSSIO:::n_years(nf2))

  sink()
})

