# nfd <- function(data = NA, start_yearmon = NA, n_months = NA,
#                 n_trace = 1, flow_space = c("intervening", "total", "both"), 
#                 time_step = c("annual", "monthly", "both"), year = c("cy", "wy")
# )
library(zoo)
library(xts)
library(CoRiverNF)
this_year <- format(Sys.Date(), "%Y")

# tesk key --------------
# test key -
#   
#   TYYYYMMSS.f
# 
# T = trace: 1 - 3
# SS = site number: 01 - 29
# MM = month: 01-12
# YY = year: yyyy
# f = flow space: 1 = total; 2 = intervening
create_test_mat <- function(nmonths, start_month, start_year)
{
  mseq <- as.yearmon(paste0(start_year, "-", sprintf("%02d", start_month))) + 
    seq(0, nmonths - 1) / 12
  
  mseq <- as.numeric(paste0(format(mseq, "%Y"), format(mseq, "%m"))) * 100
  mseq <- do.call(cbind, lapply(1:29, function(x) mseq))
  mask <- do.call(rbind, lapply(1:nmonths, function(x) 1:29))
  mseq <- mseq + mask
  mseq
}

int_flow <- create_test_mat(24, 1, 2000)
tot_flow <- int_flow + 0.1
int_flow <- int_flow + 0.2
ym <- as.yearmon("Jan 2000") + seq(0, 23) / 12
t1_int <- int_flow + 100000000
t2_int <- int_flow + 200000000
t3_int <- int_flow + 300000000
t1_tot <- tot_flow + 100000000
t2_tot <- tot_flow + 200000000
t3_tot <- tot_flow + 300000000

to_named_xts <- function(x, ym) {
  tmp <- xts(x, order.by = ym)
  colnames(tmp) <- nf_gage_abbrv()
  tmp
}

t1_int_xts <- to_named_xts(t1_int, ym)
t2_int_xts <- to_named_xts(t2_int, ym)
t3_int_xts <- to_named_xts(t3_int, ym)
t1_tot_xts <- to_named_xts(t1_tot, ym)
t2_tot_xts <- to_named_xts(t2_tot, ym)
t3_tot_xts <- to_named_xts(t3_tot, ym)

mon_array <- array(dim = c(24, 3, 29, 2))
mon_array[,1,,1] <- t1_tot
mon_array[,2,,1] <- t2_tot
mon_array[,3,,1] <- t3_tot
mon_array[,1,,2] <- t1_int
mon_array[,2,,2] <- t2_int
mon_array[,3,,2] <- t3_int

ann <- cyAnnTot
ann2 <- cyAnnTot * matrix(rnorm(length(ann)) + 1, nrow = nrow(ann))
ann3 <- cyAnnTot * matrix(rnorm(length(ann)) + 1, nrow = nrow(ann))
ann4 <- cyAnnTot * matrix(rnorm(length(ann)) + 1, nrow = nrow(ann))

ann_array <- array(dim = c(nrow(ann), 4, 29, 2))
ann_array[,1,,1] <- ann
ann_array[,1,,2] <- ann
ann_array[,2,,1] <- ann2
ann_array[,2,,2] <- ann2
ann_array[,3,,1] <- ann3
ann_array[,3,,2] <- ann3
ann_array[,4,,1] <- ann4
ann_array[,4,,2] <- ann4

# 1D -----------------
test_that("nfd constructor for 1d data works", {
  expect_is(x <- nfd(), "nfd")
  expect_true(CRSSIO:::has_annual(x) && CRSSIO:::has_total(x))
  expect_false(CRSSIO:::has_intervening(x))
  expect_false(CRSSIO:::has_monthly(x))
  expect_null(x$annual$intervening)
  expect_null(x$monthly$intervening)
  expect_null(x$monthly$total)
  expect_length(x$annual$total, 1)
  expect_equal(dim(x$annual$total[[1]]), c(1L, 1L))
  
  expect_is(
    x <- nfd(
      -999999, 
      start_yearmon = "Jan 2020", 
      n_months = 24, 
      n_trace = 10, 
      n_sites = 29,
      flow_space = "both", 
      time_step = "both",
      year = "cy"),
    "nfd"
  )
  expect_true(CRSSIO:::has_annual(x) && CRSSIO:::has_intervening(x) && 
                CRSSIO:::has_monthly(x) && CRSSIO:::has_total(x))
  expect_length(x$annual$total, 10)
  expect_length(x$annual$intervening, 10)
  expect_length(x$monthly$total, 10)
  expect_length(x$monthly$intervening, 10)
  expect_equal(dim(x$annual$total[[1]]), c(2L, 29L))
  expect_equal(dim(x$annual$intervening[[10]]), c(2L, 29L))
  expect_equal(dim(x$monthly$total[[4]]), c(24L, 29L))
  expect_equal(dim(x$monthly$intervening[[7]]), c(24L, 29L))
  expect_true(all(x$annual$total[[1]] == -999999))
  expect_true(all(x$annual$total[[9]] == -999999))
  expect_true(all(x$annual$intervening[[2]] == -999999))
  expect_true(all(x$annual$intervening[[10]] == -999999))
  expect_true(all(x$monthly$intervening[[3]] == -999999))
  expect_true(all(x$monthly$intervening[[4]] == -999999))
  expect_true(all(x$monthly$total[[5]] == -999999))
  expect_true(all(x$monthly$total[[8]] == -999999))
  expect_identical(start(x), zoo::as.yearmon("Jan 2020"))
  expect_identical(end(x), zoo::as.yearmon("Dec 2021"))
  
  expect_identical(
    nfd(55, start_yearmon = "Jan 2020"), 
    nfd(55, start_yearmon = "2020-01")
  )
  
  expect_is(x <- nfd(year = "wy"), "nfd")
  expect_identical(
    start(x), 
    zoo::as.yearmon(paste0("Sep", this_year))
  )
})

# array ----------------------
# create a 3 trace, 24 month array
nf_array <- array(-999999, dim = c(24, 3, 29))
a2 <- array(-999999, dim = c(4, 1, 29, 2))
a3 <- array(dim = c(24, 3, 29))
a3[,1,] <- t1_tot
a3[,2,] <- t2_tot
a3[,3,] <- t3_tot

a4 <- array(45524, dim = c(24, 3, 1))
  
test_that("nfd works with arrays", {
  expect_warning(expect_is(x <- nfd(nf_array, time_step = "monthly"), "nfd"))
  expect_identical(as_nfd(nf_array, time_step = "monthly"), x)
  expect_null(x$annual$intervening)
  expect_null(x$annual$total)
  expect_null(x$monthly$intervening)
  expect_length(x$monthly$total, 3)
  expect_identical(dim(x$monthly$total[[1]]), dim(x$monthly$total[[3]]))
  expect_identical(dim(x$monthly$total[[1]]), c(24L, 29L))
  expect_identical(start(x), zoo::as.yearmon(paste("Jan", this_year)))
  expect_identical(
    end(x), 
    zoo::as.yearmon(paste("Dec", as.numeric(this_year) + 1))
  )
  
  # annual total and intervening for 1 trace, 4 years
  expect_is(
    x <- nfd(a2, time_step = "annual", year = "wy", flow_space = "both", 
             n_sites = 29), 
    "nfd"
  )
  expect_identical(as_nfd(a2, time_step = "annual", year = "wy"), x)
  expect_null(x$monthly$total)
  expect_null(x$monthly$intervening)
  expect_identical(x$annual$total, x$annual$intervening)
  expect_length(x$annual$total, 1)
  expect_identical(dim(x$annual$total[[1]]), c(4L, 29L))
  expect_identical(start(x), as.yearmon(paste("Sep", this_year)))
  expect_identical(end(x), as.yearmon(paste("Sep", as.numeric(this_year) + 3)))
  
  # values are preserved correctly
  expect_is(
    x <- nfd(a3, time_step = "monthly", flow_space = "total", 
             start_yearmon = "Jan 2000", n_trace = 3, n_sites = 29,
             site_names = nf_gage_abbrv()), 
    "nfd"
  )
  expect_null(x$monthly$intervening)
  expect_null(x$annual$intervening)
  expect_null(x$annual$total)
  expect_equal(x$monthly$total[[1]], t1_tot_xts)
  expect_equal(x$monthly$total[[2]], t2_tot_xts)
  expect_equal(x$monthly$total[[3]], t3_tot_xts)
  
  # works with tot and int flow
  expect_is(
    x <- as_nfd(mon_array, time_step = "monthly", start_yearmon = "Jan 2000"),
    "nfd"
  )
  
  # monthly for 3 traces and 1 site
  expect_warning(expect_is(
    x <- nfd(
      a4, time_step = "monthly", flow_space = "total", site_names = "LeesFerry",
      start_yearmon = "Jan 2000"
    ), 
    "nfd"
  ))
  expect_null(x$annual$total)
  expect_null(x$annual$intervening)
  expect_null(x$monthly$intervening)
  expect_length(x$monthly$total, 3)
  expect_identical(dim(x$monthly$total[[1]]), c(24L, 1L))
  expect_identical(start(x), as.yearmon("Jan 2000"))
  expect_identical(end(x), as.yearmon("Dec 2001"))
})

# matrix ---------------------------------------------
test_that("nfd works with matrices", {
  expect_is(x <- nfd(t1_tot, time_step = "monthly"), "nfd")
  expect_null(x$annual$intervening)
  expect_null(x$annual$total)
  expect_null(x$monthly$intervening)
  expect_length(x$monthly$total, 1)
  expect_equivalent(coredata(x$monthly$total[[1]]), t1_tot)
  expect_identical(start(x), as.yearmon(paste0("Jan ", this_year)))
  expect_identical(
    end(x), 
    as.yearmon(paste0("Dec ", as.numeric(this_year) + 1))
  )
  
  expect_is(
    x <- as_nfd(
      matrix(1:58, ncol = 29), 
      flow_space = "intervening", 
      timestep = "annual", 
      start_yearmon = "Jan 2021"
    ), 
    "nfd"
  )
  
  expect_null(x$monthly$total)
  expect_null(x$monthly$intervening)
  expect_null(x$annual$total)
  expect_length(x$annual$intervening, 1)
  expect_identical(start(x), as.yearmon("Dec 2021"))
  expect_identical(end(x), as.yearmon("Dec 2022"))
  
  # one site
  expect_is(x <- nfd(matrix(1:36, ncol = 1), start_yearmon = "Jan 2020"), "nfd")
  expect_equal(CRSSIO:::n_trace(x), 1)
  expect_equal(CRSSIO:::n_years(x), 36)
  expect_null(colnames(x$annual$total[[1]]))
})

# xts --------------------------------------------------
test_that("nfd works with xts", {
  expect_is(x <- nfd(t1_tot_xts, time_step = "monthly"), "nfd")
  expect_identical(
    x, 
    nfd(
      t1_tot, 
      time_step = "monthly", 
      start_yearmon = "Jan 2000", 
      site_names = nf_gage_abbrv(), 
      n_sites = 29
    )
  )
  
  expect_identical(
    nfd(t3_tot_xts, time_step = "monthly"), 
    nfd(
      t3_tot, 
      time_step = "monthly",
      start_yearmon = "Jan 2000", 
      site_names = nf_gage_abbrv()
    )
  )
  
  # use the monthly data, but says its annual. will result in new years
  expect_is(
    x <- nfd(t1_tot_xts[1:3], time_step = "annual", n_sites = 29), 
    "nfd"
  )
  expect_identical(start(x), as.yearmon("Dec 2000"))
  expect_identical(end(x), as.yearmon("Dec 2002"))
  
  # two sites
  expect_is(x <- as_nfd(t1_tot_xts[,1:2], time_step = "monthly"), "nfd")
  expect_equal(CRSSIO:::n_trace(x), 1)
  expect_equal(CRSSIO:::n_sites(x), 2)
  expect_setequal(colnames(x$monthly$total[[1]]), c("GlenwoodSprings", "Cameo"))
})

# data.frame -----------------------------------------------
df_simple <- data.frame(
  year = 2021,
  month = 1:12,
  site = c(rep("a", 12), rep("b", 12)),
  trace = c(rep(1, 24), rep(2, 24)),
  value = 1:48
)

test_that("nfd() works with data.frames.", {
  expect_s3_class(tmp <- nfd(df_simple, time_step = "monthly"), "nfd")
  expect_equivalent(zoo::coredata(tmp$monthly$total[[1]]), cbind(1:12, 13:24))
  expect_equivalent(zoo::coredata(tmp$monthly$total[[2]]), cbind(25:36, 37:48))
  expect_equivalent(CRSSIO:::sites(tmp), c("a", "b"))
})
