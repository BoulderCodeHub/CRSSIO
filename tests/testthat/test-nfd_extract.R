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
  mseq <- do.call(cbind, lapply(1:29, function(x_nfd) mseq))
  mask <- do.call(rbind, lapply(1:nmonths, function(x_nfd) 1:29))
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

to_named_xts <- function(x_nfd, ym) {
  tmp <- xts(x_nfd, order.by = ym)
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

x_nfd <- as_nfd(mon_array, time_step = "monthly", start_yearmon = "Jan 2000", 
                site_names = nf_gage_abbrv())
x_crss_nf <- crss_nf(x_nfd)
sac_yt <- xts(
  matrix(sample.int(5, 6, replace = TRUE), ncol = 3), 
  order.by = as.yearmon("Dec 2000") + 0:1
)
x_crssi <- crssi(x_crss_nf, sac_yt, 1.20002001, "my scenario", drop_flow = FALSE)


# time - monthly -------------------------------------------------
# test nfd_extract.nfd and nfd_extract.crss_nf simultaneously as they will
# need to match a lot. 
test_that("nfd_extract works by month", {
  # nfd
  expect_is(x2 <- nfd_extract(x_nfd, "2001/"), "nfd")
  expect_identical(x_nfd$monthly$total[[2]]['2001/'], x2$monthly$total[[2]])
  expect_identical(
    x_nfd$monthly$intervening[[3]]['2001/'], 
    x2$monthly$intervening[[3]]
  )
  expect_identical(CRSSIO:::n_trace(x_nfd), CRSSIO:::n_trace(x2))
  expect_identical(CRSSIO:::n_months(x2), 12)
  expect_true(
    CRSSIO:::has_intervening(x2, "monthly") && CRSSIO:::has_total(x2, "monthly")
  )
  
  # crss_nf
  expect_is(x3 <- nfd_extract(x_crss_nf, "2001/"), "crss_nf")
  expect_identical(x3, crss_nf(x2))
  
  # crssi
  expect_is(x4 <- nfd_extract(x_crssi, "2001/"), "crssi")
  expect_identical(expect_message(as_crss_nf(x4)), x3)
  expect_identical(start(x4), as.yearmon("Jan 2001"))
  expect_identical(end(x4), as.yearmon("Dec 2001"))
  expect_identical(start(x4[["sac_year_type"]]), as.yearmon("Dec 2001"))
  expect_identical(end(x4[["sac_year_type"]]), as.yearmon("Dec 2001"))
})

# trace - monthly ----------------------------
test_that("nfd_extract works by trace", {

  x2 <- nfd_extract(x_nfd, , 1)
  expect_identical(CRSSIO:::n_trace(x2), 1L)
  expect_identical(x_nfd$monthly$total[[1]], x2$monthly$total[[1]])
  expect_identical(x_nfd$monthly$intervening[[1]], x2$monthly$intervening[[1]])
  
  # crss_nf
  expect_is(x5 <- nfd_extract(x_crss_nf, , 1), "crss_nf")
  expect_identical(x5, crss_nf(x2))
  
  x2 <- nfd_extract(x_nfd, , 2:3)
  expect_identical(CRSSIO:::n_trace(x2), 2L)
  expect_identical(x_nfd$monthly$total[[2]], x2$monthly$total[[1]])
  expect_identical(x_nfd$monthly$intervening[[3]], x2$monthly$intervening[[2]])
  
  expect_is(x3 <- nfd_extract(x_crss_nf, , 2:3), "crss_nf")
  expect_identical(x3, crss_nf(x2))
  
  # crssi
  expect_is(x4 <- nfd_extract(x_crssi, , 2:3), "crssi")
  expect_identical(x3, as_crss_nf(x4))
  expect_identical(x5, as_crss_nf(nfd_extract(x_crssi, , 1)))
})

# site - monthly -------------------------------
test_that("nfd_extract works by site", {  
  # nfd
  x2 <- nfd_extract(x_nfd, , , 1:2)
  x3 <- nfd_extract(x_nfd, , , "LeesFerry")
  x4 <- nfd_extract(x_nfd, , , c("Imperial", "LeesFerry"))
  expect_error(nfd_extract(x_nfd, , "xasdf"))
  expect_error(nfd_extract(x_nfd, , 20:35))
  expect_equal(x2$monthly$total[[1]], x_nfd$monthly$total[[1]][, 1:2])
  expect_equal(x2$monthly$intervening[[2]], x_nfd$monthly$intervening[[2]][, 1:2])
  expect_equal(x3$monthly$total[[3]], x_nfd$monthly$total[[3]][, 20])
  expect_equal(x3$monthly$intervening[[2]], x_nfd$monthly$intervening[[2]][, 20])
  expect_equal(x4$monthly$total[[1]], x_nfd$monthly$total[[1]][, c(29, 20)])
  expect_equal(
    x4$monthly$intervening[[3]], 
    x_nfd$monthly$intervening[[3]][, c(29, 20)]
  )
  
  # crss_nf - should return nfd since we are pulling sites out
  expect_is(x5 <- nfd_extract(x_crss_nf, , , 1:2), "nfd")
  expect_identical(x2, x5)
  expect_is( x6 <- nfd_extract(x_nfd, , , "LeesFerry"), "nfd")
  expect_identical(x3, x6)
  expect_is(x7 <- nfd_extract(x_nfd, , , c("Imperial", "LeesFerry")), "nfd")
  expect_identical(x4, x7)
  
  # crssi
  expect_error(nfd_extract(x_crssi, , , 1:2))
  expect_error(nfd_extract(x_Crssi, , , "LeesFerry"))
})

# flow_space - monthly ---------------------------
test_that("nfd_extract works by flow_space", {  
  # nfd
  x2 <- nfd_extract(x_nfd, , , , "intervening")
  x3 <- nfd_extract(x_nfd, , , , "total")
  expect_identical(x_nfd$monthly$total[[1]], x3$monthly$total[[1]])
  expect_identical(x_nfd$monthly$total[[3]], x3$monthly$total[[3]])
  expect_identical(x_nfd$monthly$intervening[[2]], x2$monthly$intervening[[2]])
  expect_identical(x_nfd$monthly$intervening[[3]], x2$monthly$intervening[[3]])
  expect_false(CRSSIO:::has_total(x2, "monthly"))
  expect_false(CRSSIO:::has_intervening(x3, "monthly"))
  
  # crss_nf - if pulling out only total, should be nfd, otherwise stay crss_nf
  expect_is(x4 <- nfd_extract(x_crss_nf, , , ,"intervening"), "crss_nf")
  expect_identical(crss_nf(x2), x4)
  expect_is(x5 <- nfd_extract(x_crss_nf, , , , "total"), "nfd")
  expect_identical(x3, x5)
  
  # crssi
  expect_is(x6 <- nfd_extract(x_crssi, , , , "intervening"), "crssi")
  expect_identical(x4, as_crss_nf(x6))
  expect_error(nfd_extract(x_crssi, , , , "total"))
})

# combined - monthly ---------------------  
test_that("nfd_extract works for combined dimensions", {
  # by all 3
  x2 <- nfd_extract(x_nfd, "/2000", 1:2, "LeesFerry", "total")
  expect_identical(x_nfd$monthly$total[[1]][1:12, "LeesFerry"], x2$monthly$total[[1]])
  expect_identical(x_nfd$monthly$total[[2]][1:12, "LeesFerry"], x2$monthly$total[[2]])
  
  x3 <- nfd_extract(x_nfd, "/2000", 1:2, "LeesFerry", "intervening")
  x4 <- nfd_extract(x_nfd, "/2000", 1:2, ,"intervening")
  expect_identical(x_nfd$monthly$intervening[[1]][1:12, "LeesFerry"], x3$monthly$intervening[[1]])
  expect_identical(x_nfd$monthly$intervening[[2]][1:12, "LeesFerry"], x3$monthly$intervening[[2]])
  expect_length(x3$monthly$intervening, 2)
  expect_identical(x_nfd$monthly$intervening[[1]][1:12, ], x4$monthly$intervening[[1]])
  expect_identical(x_nfd$monthly$intervening[[2]][1:12, ], x4$monthly$intervening[[2]])
  expect_length(x4$monthly$intervening, 2)
  
  # crss_nf
  expect_is(x5 <- nfd_extract(x_crss_nf, "/2000", 1:2, "LeesFerry", "total"), "nfd")
  expect_identical(x2, x5)
  
  expect_is(x6 <- nfd_extract(x_crss_nf, "/2000", 1:2, "LeesFerry", "intervening"), "nfd")
  expect_identical(x3, x6)
  expect_is(x7 <- nfd_extract(x_crss_nf, "/2000", 1:2, ,"intervening"), "crss_nf")
  expect_identical(crss_nf(x4), x7)
  
  # crssi
  expect_error(nfd_extract(x_crssi, "/2000", 1:2, "LeesFerry", "total"))
  expect_identical(
    as_crss_nf(nfd_extract(x_crssi, "/2000", 1:2, ,"intervening")), 
    x7
  )
})

# nfd - annual --------------------------
# don't try on crss_nf, as only annual data cannot exist for crss_nf objects
test_that("nfd_extract works with annual nfd", {
  # only annual nfd
  x_nfd <- as_nfd(ann_array, time_step = "annual", start_yearmon = "Dec 1906", 
              site_names = nf_gage_abbrv())
  # by time
  expect_is(x2 <- nfd_extract(x_nfd, "2000/"), "nfd")
  expect_identical(coredata(x2$annual$total[[1]]), coredata(ann["2000/"]))
  expect_identical(coredata(x2$annual$intervening[[4]]), coredata(ann4["2000/"]))
  expect_identical(CRSSIO:::n_trace(x_nfd), CRSSIO:::n_trace(x2))
  expect_identical(as.integer(nrow(ann["2000/"])), as.integer(CRSSIO:::n_years(x2)))
  expect_true(CRSSIO:::has_annual(x2))
  expect_true(CRSSIO:::has_intervening(x2))
  expect_true(CRSSIO:::has_total(x2))
  # by trace
  x2 <- nfd_extract(x_nfd, , 2)
  expect_identical(CRSSIO:::n_trace(x2), 1L)
  expect_identical(coredata(ann2), coredata(x2$annual$total[[1]]))
  expect_identical(coredata(ann2), coredata(x2$annual$intervening[[1]]))
  x2 <- nfd_extract(x_nfd, , c(2,4))
  expect_identical(CRSSIO:::n_trace(x2), 2L)
  expect_identical(coredata(ann2), coredata(x2$annual$total[[1]]))
  expect_identical(coredata(ann4), coredata(x2$annual$intervening[[2]]))
  
  # by site
  x2 <- nfd_extract(x_nfd, , ,"LeesFerry")
  x3 <- nfd_extract(x_nfd, , , c(4:6))
  expect_equal(x2$annual$total[[1]], x_nfd$annual$total[[1]][,"LeesFerry"])
  expect_equal(x2$annual$intervening[[2]], x_nfd$annual$intervening[[2]][,"LeesFerry"])
  expect_equal(x3$annual$total[[3]], x_nfd$annual$total[[3]][,4:6])
  expect_equal(x3$annual$total[[4]], x_nfd$annual$total[[4]][,4:6])
  
  # by flow_space
  x2 <- nfd_extract(x_nfd, , , , "intervening")
  x3 <- nfd_extract(x_nfd, , , , "total")
  expect_identical(coredata(x_nfd$annual$total[[1]]), coredata(x3$annual$total[[1]]))
  expect_identical(coredata(x_nfd$annual$total[[3]]), coredata(x3$annual$total[[3]]))
  expect_identical(coredata(x_nfd$annual$intervening[[2]]), coredata(x2$annual$intervening[[2]]))
  expect_identical(coredata(x_nfd$annual$intervening[[4]]), coredata(x2$annual$intervening[[4]]))
  expect_false(CRSSIO:::has_total(x2, "annual"))
  expect_false(CRSSIO:::has_intervening(x3, "annual"))
  
  # by all 3
  x2 <- nfd_extract(x_nfd, "/2000", 1:2, c("Cameo", "LeesFerry") , "total")
  expect_identical(coredata(ann["/2000", c("Cameo", "LeesFerry")]), coredata(x2$annual$total[[1]]))
  expect_identical(coredata(ann2["/2000", c("Cameo", "LeesFerry")]), coredata(x2$annual$total[[2]]))
  expect_false(CRSSIO:::has_intervening(x2))
})

# combined annual and monthly -----------------------
test_that("can extract monthly and annual data from nfd", {
  # monthly and annual nfd
  # TODO: enhance this once we can create a non arbitray nfd with annual and
  # monthly data
  x_nfd <- nfd(
    4, 
    start_yearmon = "Jan 2000", 
    n_months = 24,
    n_trace = 20,
    flow_space = "both", 
    time_step = "both",
    n_sites = 29, 
    site_names = nf_gage_abbrv()
  )
  x_crss_nf <- crss_nf(x_nfd)
  sac_yt <- xts(
    matrix(1, ncol = 20, nrow = 2), order.by = as.yearmon("Dec 2000") + 0:1
  )
  x_crssi <- crssi(x_crss_nf, sac_yt, -99, drop_flow = FALSE)
  
  # nfd
  expect_is(
    x2 <- nfd_extract(x_nfd, "2001/", 1:5, , "intervening", "annual"), 
    "nfd"
  )
  expect_identical(CRSSIO:::n_trace(x2), 5L)
  expect_true(CRSSIO:::has_annual(x2))
  expect_true(CRSSIO:::has_intervening(x2))
  expect_false(CRSSIO:::has_monthly(x2))
  expect_false(CRSSIO:::has_total(x2))
  expect_identical(CRSSIO:::n_years(x2), 1)
  
  # crss_nf, but should become nfd
  expect_is(
    x3 <-  nfd_extract(x_crss_nf, "2001/", 1:5, , "intervening", "annual"), 
    "nfd"
  )
  expect_identical(x2, x3)
  
  # nfd
  expect_is(
    x2 <- nfd_extract(x_nfd, "2000/", , , c("intervening", "total"), "monthly"), 
    "nfd"
  )
  expect_identical(CRSSIO:::n_trace(x2), 20L)
  expect_false(CRSSIO:::has_annual(x2))
  expect_true(CRSSIO:::has_intervening(x2, "monthly"))
  expect_true(CRSSIO:::has_monthly(x2))
  expect_true(CRSSIO:::has_total(x2, "monthly"))
  expect_identical(CRSSIO:::n_months(x2), 24)
  
  # crss_nf should stay crss_nf
  expect_is(
    x3 <- nfd_extract(x_crss_nf, "2000/", , , c("intervening", "total"), "monthly"), 
    "crss_nf"
  )
  expect_identical(crss_nf(x2), x3)
  
  # crssi
  expect_error(nfd_extract(x_crssi, "2001/", 1:5, , "intervening", "annual"))
  expect_identical(
    as_crss_nf(
      nfd_extract(x_crssi, "2000/", , , c("intervening", "total"), "monthly" )
    ),
    x3
  )
})

