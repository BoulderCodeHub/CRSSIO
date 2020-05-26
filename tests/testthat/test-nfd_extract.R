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

# nfd_extract -------------------------------------------------
test_that("nfd_extract() works", {
  # only monthly nfd
  x <- as_nfd(mon_array, time_step = "monthly", start_yearmon = "Jan 2000")
  
  # by time
  expect_is(x2 <- nfd_extract(x, "2001/"), "nfd")
  expect_identical(x$monthly$total[[2]]['2001/'], x2$monthly$total[[2]])
  expect_identical(
    x$monthly$intervening[[3]]['2001/'], 
    x2$monthly$intervening[[3]]
  )
  expect_identical(CRSSIO:::n_trace(x), CRSSIO:::n_trace(x2))
  expect_identical(CRSSIO:::n_months(x2), 12)
  expect_true(
    CRSSIO:::has_intervening(x2, "monthly") && CRSSIO:::has_total(x2, "monthly")
  )
  
  # by trace
  x2 <- nfd_extract(x, , 1)
  expect_identical(CRSSIO:::n_trace(x2), 1L)
  expect_identical(x$monthly$total[[1]], x2$monthly$total[[1]])
  expect_identical(x$monthly$intervening[[1]], x2$monthly$intervening[[1]])
  
  x2 <- nfd_extract(x, , 2:3)
  expect_identical(CRSSIO:::n_trace(x2), 2L)
  expect_identical(x$monthly$total[[2]], x2$monthly$total[[1]])
  expect_identical(x$monthly$intervening[[3]], x2$monthly$intervening[[2]])
  
  # by flow_space
  x2 <- nfd_extract(x, , , , "intervening")
  x3 <- nfd_extract(x, , , , "total")
  expect_identical(x$monthly$total[[1]], x3$monthly$total[[1]])
  expect_identical(x$monthly$total[[3]], x3$monthly$total[[3]])
  expect_identical(x$monthly$intervening[[2]], x2$monthly$intervening[[2]])
  expect_identical(x$monthly$intervening[[3]], x2$monthly$intervening[[3]])
  expect_false(CRSSIO:::has_total(x2, "monthly"))
  expect_false(CRSSIO:::has_intervening(x3, "monthly"))
  
  # by all 3
  x2 <- nfd_extract(x, "/2000", 1:2, , "total")
  expect_identical(x$monthly$total[[1]][1:12], x2$monthly$total[[1]])
  expect_identical(x$monthly$total[[2]][1:12], x2$monthly$total[[2]])
  
  # only annual nfd
  x <- as_nfd(ann_array, time_step = "annual", start_yearmon = "Dec 1906")
  # by time
  expect_is(x2 <- nfd_extract(x, "2000/"), "nfd")
  expect_identical(coredata(x2$annual$total[[1]]), coredata(ann["2000/"]))
  expect_identical(coredata(x2$annual$intervening[[4]]), coredata(ann4["2000/"]))
  expect_identical(CRSSIO:::n_trace(x), CRSSIO:::n_trace(x2))
  expect_identical(as.integer(nrow(ann["2000/"])), as.integer(CRSSIO:::n_years(x2)))
  expect_true(CRSSIO:::has_annual(x2))
  expect_true(CRSSIO:::has_intervening(x2))
  expect_true(CRSSIO:::has_total(x2))
  # by trace
  x2 <- nfd_extract(x, , 2)
  expect_identical(CRSSIO:::n_trace(x2), 1L)
  expect_identical(coredata(ann2), coredata(x2$annual$total[[1]]))
  expect_identical(coredata(ann2), coredata(x2$annual$intervening[[1]]))
  x2 <- nfd_extract(x, , c(2,4))
  expect_identical(CRSSIO:::n_trace(x2), 2L)
  expect_identical(coredata(ann2), coredata(x2$annual$total[[1]]))
  expect_identical(coredata(ann4), coredata(x2$annual$intervening[[2]]))
  
  # by flow_space
  x2 <- nfd_extract(x, , , , "intervening")
  x3 <- nfd_extract(x, , , , "total")
  expect_identical(coredata(x$annual$total[[1]]), coredata(x3$annual$total[[1]]))
  expect_identical(coredata(x$annual$total[[3]]), coredata(x3$annual$total[[3]]))
  expect_identical(coredata(x$annual$intervening[[2]]), coredata(x2$annual$intervening[[2]]))
  expect_identical(coredata(x$annual$intervening[[4]]), coredata(x2$annual$intervening[[4]]))
  expect_false(CRSSIO:::has_total(x2, "annual"))
  expect_false(CRSSIO:::has_intervening(x3, "annual"))
  
  # by all 3
  x2 <- nfd_extract(x, "/2000", 1:2, , "total")
  expect_identical(coredata(ann["/2000"]), coredata(x2$annual$total[[1]]))
  expect_identical(coredata(ann2["/2000"]), coredata(x2$annual$total[[2]]))
  expect_false(CRSSIO:::has_intervening(x2))
  
  # monthly and annual nfd
  # TODO: enhance this once we can create a non arbitray nfd with annual and
  # monthly data
  x <- nfd(
    4, 
    start_yearmon = "Jan 2000", 
    n_months = 24,
    n_trace = 20,
    flow_space = "both", 
    time_step = "both"
  )
  
  expect_is(
    x2 <- nfd_extract(x, "2001/", 1:5, , "intervening", "annual"), 
    "nfd"
  )
  expect_identical(CRSSIO:::n_trace(x2), 5L)
  expect_true(CRSSIO:::has_annual(x2))
  expect_true(CRSSIO:::has_intervening(x2))
  expect_false(CRSSIO:::has_monthly(x2))
  expect_false(CRSSIO:::has_total(x2))
  expect_identical(CRSSIO:::n_years(x2), 1)
  
  expect_is(
    x2 <- nfd_extract(x, "2000/", , , c("intervening", "total"), "monthly"), 
    "nfd"
  )
  expect_identical(CRSSIO:::n_trace(x2), 20L)
  expect_false(CRSSIO:::has_annual(x2))
  expect_true(CRSSIO:::has_intervening(x2, "monthly"))
  expect_true(CRSSIO:::has_monthly(x2))
  expect_true(CRSSIO:::has_total(x2, "monthly"))
  expect_identical(CRSSIO:::n_months(x2), 24)
})
