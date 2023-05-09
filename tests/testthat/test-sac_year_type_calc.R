# wy_vol specified -------------------
vv <- c(16.04, 18.55, 32.09, 10.28, 10.28, 13.02, 16.01, 25.21, 11.84, 
        12.19, 7.46, 9.23, 17.48, 37.82, 12.86)
yy <- 2004:2018

yy <- zoo::as.yearmon(paste("Dec", yy))

wy_vol <- xts::xts(vv, yy)

test_that("sac_year_type_calc() works with WY volume.", {
  expect_s3_class(tmp <- sac_year_type_calc(wy_vol = wy_vol), "xts")
  expect_true(all(tmp == CRSSIO:::sacYT["2004/2018"]))
  
  # values are the same if multiple columns are specified
  wy2 <- cbind(wy_vol, wy_vol, wy_vol)
  expect_s3_class(tmp <- sac_year_type_calc(wy_vol = wy2), "xts")
  expect_equal(ncol(tmp), 3)
  expect_true(all(tmp[,1] == tmp[,2]))
  expect_true(all(tmp[,2] == tmp[,3]))
})

# co_int_nf specified ---------------------

test_that("sac_year_type_calc() works with WY volume.", {
  nf <- nfd(CoRiverNF::wyAnnInt, flow_space = "intervening", 
            time_step = "annual",
            year = "wy")
  
  expect_s3_class(tmp <- sac_year_type_calc(co_int_nf = nf), "xts")
  expect_equal(ncol(tmp), 1)
  expect_equal(nrow(tmp), CRSSIO:::n_years(nf))
  expect_true(all(unique(tmp) %in% 1:5))
  expect_true(all(1:5 %in% unique(tmp)))
  
  # and it works with ism
  nf2 <- nfd(CoRiverNF::wyAnnInt["1988/"], flow_space = "intervening", 
             time_step = "annual", year = "wy")
  nf2 <- ism(nf2)
  
  expect_s3_class(tmp <- sac_year_type_calc(co_int_nf = nf2), "xts")
  expect_equal(ncol(tmp), CRSSIO:::n_trace(nf2))
  expect_equal(nrow(tmp), CRSSIO:::n_years(nf2))
  expect_true(all(unique(tmp) %in% 1:5))
  # won't necesarily have all 5 year types in the truncated period
  #expect_true(all(1:5 %in% unique(tmp)))
})

