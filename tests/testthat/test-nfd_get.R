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

nfd_ann <- nfd(ann_array, flow_space = "both", time_step = "annual", 
               n_sites = 29, n_trace = 4,
               start_yearmon = "Dec 2020", site_names = nf_gage_abbrv())
nfd_mon <- nfd(mon_array, flow_space = "both", time_step = "monthly", 
               n_sites = 29, n_trace = 3,
               start_yearmon = "Jan 2020", site_names = nf_gage_abbrv())

nfd_simple <- nfd(5, n_months = 24, n_sites = 3, flow_space = "total", 
                  time_step = "annual")

x_crss_nf <- crss_nf(nfd_mon)
sac_yt <- xts(
  matrix(sample.int(5, 6, replace = TRUE), ncol = 3), 
  order.by = as.yearmon("Dec 2020") + 0:1
)
x_crssi <- crssi(x_crss_nf, sac_yt, 1.20002001, "my scenario", drop_flow = FALSE)

# nfd_get_site() --------------------------------------
test_that("nfd_get_site() works", {
  expect_is(x <- nfd_get_site(nfd_ann, "LeesFerry", "total", "annual"), "xts")
  expect_equivalent(coredata(x), ann_array[,,20,1])
  expect_identical(start(x), as.yearmon("Dec 2020"))
  expect_identical(end(x), as.yearmon("Dec 2020") + nrow(ann) - 1)
  expect_identical(x, nfd_get_site(nfd_ann, 20, "total", "annual"))
  expect_error(nfd_get_site(nfd_ann, 20, "total", "monthly"))
  expect_error(nfd_get_site(nfd_ann, 40, "intervening", "annual"))
  expect_error(nfd_get_site(nfd_ann, "abc", "intervening", "annual"))
  
  expect_is(x <- nfd_get_site(nfd_mon, "Maybell", "intervening", "monthly"), "xts")
  expect_equivalent(coredata(x), mon_array[,,12,2])
  expect_identical(start(x), as.yearmon("Jan 2020"))
  expect_identical(end(x), as.yearmon("Dec 2021"))
  expect_identical(x, nfd_get_site(nfd_mon, 12, "intervening", "monthly"))
  expect_error(nfd_get_site(nfd_mon, 12, "intervening", "annual"))
  expect_error(nfd_get_site(nfd_mon, 40, "intervening", "monthly"))
  expect_error(nfd_get_site(nfd_mon, "Bluffs", "intervening", "monthly"))
  expect_is(x2 <- nfd_get_site(x_crss_nf, "Maybell", "intervening", "monthly"), "xts")
  expect_identical(x, x2)
  
  # unnamed ------------------
  expect_is(nfd_get_site(nfd_simple, 2, "total", "annual"), "xts")
  
  # crssi --------------------
  expect_is(x3 <- nfd_get_site(x_crssi, "Maybell", "intervening", "monthly"), "xts")
  expect_identical(x3, x)
})

# nfd_get_trace() -----------------------------------------
test_that("nfd_get_trace() works", {
  expect_is(x <- nfd_get_trace(nfd_ann, 2, "intervening", "annual"), "xts")
  expect_equal(coredata(x), coredata(ann2))
  expect_identical(start(x), as.yearmon("Dec 2020"))
  expect_identical(end(x), as.yearmon("Dec 2020") + nrow(ann) - 1)
  expect_error(nfd_get_trace(nfd_ann, 2, "total", "monthly"))
  expect_error(nfd_get_trace(nfd_ann, 40, "intervening", "annual"))
  expect_error(nfd_get_trace(nfd_ann, "abc", "intervening", "annual"))
  
  expect_is(x <- nfd_get_trace(nfd_mon, 3, "total", "monthly"), "xts")
  expect_equal(coredata(x), coredata(t3_tot_xts))
  expect_identical(start(x), as.yearmon("Jan 2020"))
  expect_identical(end(x), as.yearmon("Dec 2021"))
  expect_error(nfd_get_trace(nfd_mon, 2, "total", "annual"))
  expect_error(nfd_get_trace(nfd_mon, 4, "intervening", "monthly"))
  expect_error(nfd_get_trace(nfd_mon, "defs", "intervening", "monthly"))
  
  expect_is(x2 <- nfd_get_trace(x_crss_nf, 3, "total", "monthly"), "xts")
  expect_identical(x, x2)
  
  # crssi
  expect_is(x3 <- nfd_get_trace(x_crssi, 3, "total", "monthly"), "xts")
  expect_identical(x, x3)
})

# nfd_get_time() ----------------------------------------------
test_that("nfd_get_time() works", {
  expect_is(x <- nfd_get_time(nfd_ann, "Dec 2021", "total", "annual"), "matrix")
  expect_equivalent(x, ann_array[2,,,1])
  expect_identical(
    x, 
    nfd_get_time(nfd_ann, as.yearmon("Dec 2021"), "total", "annual")
  )
  
  expect_equivalent(
    nfd_get_time(nfd_ann, "Dec 2132", "total", "annual"),
    ann_array[113,,,2]
  )
  expect_error(nfd_get_time(nfd_ann, "Sep 2030", "total", "annual"))
  expect_error(nfd_get_time(nfd_ann, "Dec 2021", "total", "monthly"))
  
  expect_is(x <- nfd_get_time(nfd_mon, "Feb 2020", "total", "monthly"), "matrix")
  expect_equivalent(x, mon_array[2,,,1])
  expect_identical(
    x, 
    nfd_get_time(nfd_mon, as.yearmon("Feb 2020"), "total", "monthly")
  )
  expect_equivalent(
    nfd_get_time(nfd_mon, "Dec 2021", "intervening", "monthly"),
    mon_array[24,,,2]
  )
  expect_error(nfd_get_time(nfd_mon, "Jan 2022", "total", "monthly"))
  expect_error(nfd_get_time(nfd_mon, "Sep 2021", "intervening", "annual"))
  
  expect_is(x2 <- nfd_get_time(x_crss_nf, "Feb 2020", "total", "monthly"), "matrix")
  expect_identical(x, x2)
  
  # crssi
  expect_is(x3 <- nfd_get_time(x_crssi, "Feb 2020", "total", "monthly"), "matrix")
  expect_identical(x, x3)
})
