library(xts)

# reindex.xts -----------------------------
x1_ann <- xts(
  matrix(22, nrow = 4, ncol = 1), 
  order.by = as.yearmon("Dec 2020") + 0:3
)

x2_ann <- xts(
  matrix(75, nrow = 5, ncol = 4),
  order.by = as.yearmon("Sep 2020") + 0:4
)

x3_ann <- xts(22, order.by = as.yearmon("Dec 2020"))

x1_mon <- xts(
  matrix(22, nrow = 36, ncol = 1), 
  order.by = as.yearmon("Jan 2020") + 0:35/12
)

x2_mon <- xts(
  matrix(75, nrow = 17, ncol = 4),
  order.by = as.yearmon("Sep 2020") + 0:16/12
)

x3_mon <- xts(22, order.by = as.yearmon("Feb 2020"))

test_that("reindex.xts() works", {
  # annual
  expect_equivalent(coredata(x <- reindex.xts(x1_ann, 2000)), coredata(x1_ann))
  expect_identical(start(x), as.yearmon("Dec 2000"))
  expect_identical(end(x), as.yearmon("Dec 2003"))
  
  expect_equivalent(coredata(x <- reindex.xts(x2_ann, "2053")), coredata(x2_ann))
  expect_identical(start(x), as.yearmon("Sep 2053"))
  expect_identical(end(x), as.yearmon("Sep 2057"))
  
  expect_equivalent(coredata(x <- reindex.xts(x3_ann, "2019")), coredata(x3_ann))
  expect_identical(start(x), as.yearmon("Dec 2019"))
  expect_identical(end(x), as.yearmon("Dec 2019"))
  
  # monthly
  expect_equivalent(coredata(x <- reindex.xts(x1_mon, 2000)), coredata(x1_mon))
  expect_identical(start(x), as.yearmon("Jan 2000"))
  expect_identical(end(x), as.yearmon("Dec 2002"))
  
  expect_equivalent(coredata(x <- reindex.xts(x2_mon, "2053")), coredata(x2_mon))
  expect_identical(start(x), as.yearmon("Sep 2053"))
  expect_identical(end(x), as.yearmon("Jan 2055"))
  
  expect_equivalent(coredata(x <- reindex(x3_mon, "2019")), coredata(x3_mon))
  expect_identical(start(x), as.yearmon("Feb 2019"))
  expect_identical(end(x), as.yearmon("Feb 2019"))
  
  expect_error(reindex(x3_mon, "Jan 2010"))
})

# reindex.nfd ------------------------------------------

nfd_ann <- nfd(4, n_months = 36, n_trace = 1, n_sites = 1, flow_space = "total",
               time_step = "annual", start_yearmon = "Sep 2020", year = "wy")
nfd_mon <- nfd(5, n_months = 37, n_trace = 5, n_sites = 5, 
               flow_space = "intervening", time_step = "monthly", 
               start_yearmon = "Jan 2020")
nfd_both <- nfd(6, n_months = 72, n_trace = 4, n_sites = 7, flow_space = "both",
                time_step = "both", start_yearmon = "Feb 2020")

test_that("reindex works on nfd objects", {
  expect_is(x <- reindex(nfd_ann, 2022), "nfd")
  expect_identical(start(x), as.yearmon("Sep 2022"))
  expect_identical(end(x), as.yearmon("Sep 2024"))
  
  expect_is(x <- reindex(nfd_mon, "2000"), "nfd")
  expect_identical(start(x), as.yearmon("Jan 2000"))
  expect_identical(end(x), as.yearmon("Jan 2003"))
  
  expect_is(x <- reindex(nfd_both, 2022), "nfd")
  expect_identical(start(x), as.yearmon("Feb 2022"))
  expect_identical(end(x), as.yearmon("Jan 2028"))
  
  expect_error(reindex(nfd_both, "jan 2022"))
})

# crssi.nfd -----------------------------------------

crssi_mon <- crssi(crss_nf(CoRiverNF::monthlyInt), CRSSIO:::sacYT, 1.1906)
crssi_both <- crssi(crss_nf(5, n_months = 36, n_trace = 5, flow_space = "both",
                              time_step = "both", start_yearmon = "Jan 2000"),
                    xts(
                      matrix(5, nrow = 3, ncol = 5), 
                      order.by = as.yearmon("Dec 2000") + 0:2
                    ),
                    1.2000, "weird scen", drop_flow = FALSE)

test_that("reindex works on crssi objects", {
  expect_is(x <- reindex(crssi_mon, 2022), "crssi")
  expect_identical(start(x), as.yearmon("Jan 2022"))
  expect_identical(end(x), end(crssi_mon) + (2022-1906))
  
  expect_is(x <- reindex(crssi_both, "1980"), "crssi")
  expect_identical(start(x), as.yearmon("Jan 1980"))
  expect_identical(end(x), as.yearmon("Dec 1982"))
  
  expect_error(reindex(crssi_mon, "2022-12"))
})