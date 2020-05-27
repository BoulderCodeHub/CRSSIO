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
  
})
