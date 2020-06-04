library(xts)


# ism.xts --------------------------
# setup 
myYM <- as.yearmon('1905-01') + seq(0,47)/12
tstData <- xts(1:48, myYM)
t2 <- xts(1:6, zoo::as.yearmon("Dec 2000") + 0:5)

myIsm <- xts(
  cbind(1:48,c(13:48,1:12),c(25:48,1:24),c(37:48,1:36)),
  as.yearmon('1905-01') + seq(0,47)/12
)
myIsm2 <- xts(
  cbind(1:6, c(2:6, 1), c(3:6, 1:2), c(4:6, 1:3), c(5:6, 1:4), c(6, 1:5)),
  as.yearmon("Dec 2000") + 0:5
)

test_that('ism.xts works as expected',{
  # monthly 
  expect_equal(dim(x <- ism(tstData)),c(48, 4))
  expect_identical(x, myIsm)
  expect_identical(start(x), start(tstData))
  expect_identical(end(x), end(tstData))
  
  expect_equal(dim(x <- ism(tstData, n_years_keep = 3)), c(36, 4))
  expect_identical(x, myIsm[1:36,])
  expect_identical(start(x), start(tstData))
  expect_identical(end(x), index(tstData)[36])
  
  expect_equal(dim(ism(tstData[1:36], n_years_keep = 2)), c(24, 3))
  
  # annual
  expect_equal(dim(x <- ism(t2[1:5], is_monthly = FALSE)), c(5, 5))
  expect_equal(
    coredata(x), 
    cbind(1:5, c(2:5, 1), c(3:5, 1:2), c(4:5, 1:3), c(5, 1:4))
  )
  expect_identical(start(x), start(t2))
  expect_identical(end(x), index(t2)[5])
  
  expect_equal(dim(x <- ism(t2, n_years_keep = 5)), c(5, 6))
  expect_equal(x, myIsm2[1:5,])
  expect_identical(start(x), start(t2))
  expect_identical(end(x), index(t2)[5])
  
  expect_equal(dim(x <- ism(t2[1:4], n_years_keep = 3, monthly = FALSE)), c(3,4))
  expect_identical(start(x), start(t2))
  expect_identical(end(x), index(t2)[3])
  
  # errors
  expect_error(ism(tstData, n_years_keep = 5))
  expect_error(ism(myIsm))
  expect_error(ism(t2, n_years_keep = 7))
  expect_error(ism(tstData[1:27]))
})

# nfd -------------------------------------------------
nfd_ann <- nfd(CoRiverNF::cyAnnTot["2000/2009", c("LeesFerry", "Imperial")], 
               flow_space = "total", time_step = "annual", n_sites = 2)
ann_mat <- coredata(CoRiverNF::cyAnnTot["2000/2009", c("LeesFerry", "Imperial")])
nfd_mon <- nfd(CoRiverNF::monthlyInt["2000/2009","LeesFerry"], 
               flow_space = "intervening", time_step = "monthly")
mon_mat <- coredata(CoRiverNF::monthlyInt["2000/2009","LeesFerry"])
nfd_both <- nfd(5, n_months = 36, n_sites = 5, flow_space = "both", 
                time_step = "both", start_yearmon = "Oct 2000", year = "wy")

test_that("ism.nfd works", {
  expect_is(x <- ism(nfd_ann), "nfd")
  expect_true(CRSSIO:::has_annual(x))
  expect_false(CRSSIO:::has_monthly(x))
  expect_true(CRSSIO:::has_total(x, "annual"))
  expect_false(CRSSIO:::has_intervening(x, "annual"))
  expect_equal(CRSSIO:::n_trace(x), 10)
  expect_identical(start(x), as.yearmon("Dec 2000"))
  expect_identical(end(x), as.yearmon("Dec 2009"))
  expect_setequal(CRSSIO:::sites(x), c("LeesFerry", "Imperial"))
  expect_equivalent(
    coredata(nfd_get_trace(x, 1, "total", "annual")),
    ann_mat
  )
  expect_equivalent(
    coredata(nfd_get_trace(x, 2, "total", "annual")),
    ann_mat[c(2:10, 1),]
  )
  expect_equivalent(
    coredata(nfd_get_trace(x, 9, "total", "annual")),
    ann_mat[c(9:10, 1:8),]
  )
  expect_equivalent(
    coredata(nfd_get_trace(x, 10, "total", "annual")),
    ann_mat[c(10, 1:9),]
  )
  
  expect_is(x <- ism(nfd_mon, n_years_keep = 5), "nfd")
  expect_false(CRSSIO:::has_annual(x))
  expect_true(CRSSIO:::has_monthly(x))
  expect_false(CRSSIO:::has_total(x, "monthly"))
  expect_true(CRSSIO:::has_intervening(x, "monthly"))
  expect_equal(CRSSIO:::n_trace(x), 10)
  expect_identical(start(x), as.yearmon("Jan 2000"))
  expect_identical(end(x), as.yearmon("Dec 2004"))
  expect_setequal(CRSSIO:::sites(x), c("LeesFerry"))
  expect_equivalent(
    coredata(nfd_get_trace(x, 1, "intervening", "monthly")),
    mon_mat[1:60,]
  )
  expect_equivalent(
    coredata(nfd_get_trace(x, 2, "intervening", "monthly")),
    mon_mat[c(13:72),]
  )
  expect_equivalent(
    coredata(nfd_get_trace(x, 9, "intervening", "monthly")),
    mon_mat[c(97:120, 1:36),]
  )
  expect_equivalent(
    coredata(nfd_get_trace(x, 10, "intervening", "monthly")),
    mon_mat[c(109:120, 1:48),]
  )
  
  expect_is(x <- ism(nfd_both), "nfd")
  expect_true(CRSSIO:::has_annual(x))
  expect_true(CRSSIO:::has_monthly(x))
  expect_true(CRSSIO:::has_total(x, "monthly"))
  expect_true(CRSSIO:::has_intervening(x, "monthly"))
  expect_true(CRSSIO:::has_total(x, "annual"))
  expect_true(CRSSIO:::has_intervening(x, "annual"))
  expect_equal(CRSSIO:::n_trace(x), 3)
  expect_identical(start(x), as.yearmon("Oct 2000"))
  expect_identical(end(x), as.yearmon("Sep 2003"))
  expect_equal(CRSSIO:::sites(x), NULL)
})

# crss_nf ---------------------------------------------
nfd_ann <- crss_nf(-99, n_trace = 1, flow_space = "intervening", 
                   time_step = "both")
nfd_ann$monthly$intervening[[1]] <- CoRiverNF::monthlyInt["1980/1989"]
nfd_ann$annual$intervening[[1]] <- CoRiverNF::cyAnnInt["1980/1989"]
ann_mat <- coredata(CoRiverNF::cyAnnInt["1980/1989"])

nfd_mon <- crss_nf(-99, n_trace = 1, flow_space = "both", time_step = "monthly", 
                   year = "wy")
nfd_mon$monthly$intervening[[1]] <- CoRiverNF::monthlyInt["1970-10/1980-09"]
nfd_mon$monthly$total[[1]] <- CoRiverNF::monthlyTot["1970-10/1980-09"]
mon_mat <- coredata(CoRiverNF::monthlyTot["1970-10/1980-09"])

nfd_both <- crss_nf(5, n_months = 36, flow_space = "both", 
                time_step = "both", start_yearmon = "Jan 2000")

test_that("ism.crss_nf works", {
  expect_is(x <- ism(nfd_ann), "crss_nf")
  expect_true(CRSSIO:::has_annual(x))
  expect_true(CRSSIO:::has_monthly(x))
  expect_false(CRSSIO:::has_total(x, "annual"))
  expect_true(CRSSIO:::has_intervening(x, "annual"))
  expect_equal(CRSSIO:::n_trace(x), 10)
  expect_identical(start(x), as.yearmon("Jan 1980"))
  expect_identical(end(x), as.yearmon("Dec 1989"))
  expect_true(all(CRSSIO:::sites(x) == nf_gage_abbrv()))
  expect_equivalent(
    coredata(nfd_get_trace(x, 1, "intervening", "annual")),
    ann_mat
  )
  expect_equivalent(
    coredata(nfd_get_trace(x, 2, "intervening", "annual")),
    ann_mat[c(2:10, 1),]
  )
  expect_equivalent(
    coredata(nfd_get_trace(x, 9, "intervening", "annual")),
    ann_mat[c(9:10, 1:8),]
  )
  expect_equivalent(
    coredata(nfd_get_trace(x, 10, "intervening", "annual")),
    ann_mat[c(10, 1:9),]
  )
  
  expect_is(x <- ism(nfd_mon, n_years_keep = 5, is_monthly = TRUE), "crss_nf")
  expect_false(CRSSIO:::has_annual(x))
  expect_true(CRSSIO:::has_monthly(x))
  expect_true(CRSSIO:::has_total(x, "monthly"))
  expect_true(CRSSIO:::has_intervening(x, "monthly"))
  expect_equal(CRSSIO:::n_trace(x), 10)
  expect_identical(start(x), as.yearmon("Oct 1970"))
  expect_identical(end(x), as.yearmon("Sep 1975"))
  expect_true(all(CRSSIO:::sites(x) == nf_gage_abbrv()))
  expect_equivalent(
    coredata(nfd_get_trace(x, 1, "total", "monthly")),
    mon_mat[1:60,]
  )
  expect_equivalent(
    coredata(nfd_get_trace(x, 2, "total", "monthly")),
    mon_mat[c(13:72),]
  )
  expect_equivalent(
    coredata(nfd_get_trace(x, 9, "total", "monthly")),
    mon_mat[c(97:120, 1:36),]
  )
  expect_equivalent(
    coredata(nfd_get_trace(x, 10, "total", "monthly")),
    mon_mat[c(109:120, 1:48),]
  )
  
  expect_is(x <- ism(nfd_both), "crss_nf")
  expect_true(CRSSIO:::has_annual(x))
  expect_true(CRSSIO:::has_monthly(x))
  expect_true(CRSSIO:::has_total(x, "monthly"))
  expect_true(CRSSIO:::has_intervening(x, "monthly"))
  expect_true(CRSSIO:::has_total(x, "annual"))
  expect_true(CRSSIO:::has_intervening(x, "annual"))
  expect_equal(CRSSIO:::n_trace(x), 3)
  expect_identical(start(x), as.yearmon("Jan 2000"))
  expect_identical(end(x), as.yearmon("Dec 2002"))
  expect_true(all(CRSSIO:::sites(x) == nf_gage_abbrv()))
})

# crssi -----------------------------------------------
sac1 <- CRSSIO:::sacYT["1980/1989"]
sac2 <- CRSSIO:::sacYT["1971/1979"]
sac3 <- CRSSIO:::sacYT["2000/2002"]

crssi1 <- crssi(nfd_ann, sac1, 1.1)
crssi2 <- crssi(nfd_mon, sac2, 1.2, "ok")
crssi3 <- crssi(nfd_both, sac3, 1.2, "ok", drop_flow = FALSE)

mm1 <- coredata(CoRiverNF::monthlyInt["1980/1989"])
mm2 <- coredata(CoRiverNF::monthlyInt["1971/1979"])

test_that("ism.crssi works", {
  expect_is(x <- ism(crssi1), "crss_nf")
  expect_false(CRSSIO:::has_annual(x))
  expect_true(CRSSIO:::has_monthly(x))
  expect_false(CRSSIO:::has_total(x, "annual"))
  expect_true(CRSSIO:::has_intervening(x, "monthly"))
  expect_equal(CRSSIO:::n_trace(x), 10)
  expect_identical(start(x), as.yearmon("Jan 1980"))
  expect_identical(end(x), as.yearmon("Dec 1989"))
  expect_true(all(CRSSIO:::sites(x) == nf_gage_abbrv()))
  expect_equivalent(
    coredata(nfd_get_trace(x, 1, "intervening", "monthly")),
    mm1
  )
  expect_equivalent(
    coredata(nfd_get_trace(x, 2, "intervening", "monthly")),
    mm1[c(13:120, 1:12),]
  )
  expect_equivalent(
    coredata(nfd_get_trace(x, 9, "intervening", "monthly")),
    mm1[c(97:120, 1:96),]
  )
  expect_equivalent(
    coredata(nfd_get_trace(x, 10, "intervening", "monthly")),
    mm1[c(109:120, 1:108),]
  )
  
  expect_is(x <- ism(crssi2, n_years_keep = 5), "crss_nf")
  expect_false(CRSSIO:::has_annual(x))
  expect_true(CRSSIO:::has_monthly(x))
  expect_false(CRSSIO:::has_total(x, "monthly"))
  expect_true(CRSSIO:::has_intervening(x, "monthly"))
  expect_equal(CRSSIO:::n_trace(x), 9)
  expect_identical(start(x), as.yearmon("Jan 1971"))
  expect_identical(end(x), as.yearmon("Dec 1975"))
  expect_true(all(CRSSIO:::sites(x) == nf_gage_abbrv()))
  expect_equivalent(
    coredata(nfd_get_trace(x, 1, "intervening", "monthly")),
    mm2[1:60,]
  )
  expect_equivalent(
    coredata(nfd_get_trace(x, 2, "intervening", "monthly")),
    mm2[c(13:72),]
  )
  expect_equivalent(
    coredata(nfd_get_trace(x, 8, "intervening", "monthly")),
    mm2[c(85:108, 1:36),]
  )
  expect_equivalent(
    coredata(nfd_get_trace(x, 9, "intervening", "monthly")),
    mm2[c(97:108, 1:48),]
  )
  
  expect_is(x <- ism(nfd_both), "crss_nf")
  expect_true(CRSSIO:::has_annual(x))
  expect_true(CRSSIO:::has_monthly(x))
  expect_true(CRSSIO:::has_total(x, "monthly"))
  expect_true(CRSSIO:::has_intervening(x, "monthly"))
  expect_true(CRSSIO:::has_total(x, "annual"))
  expect_true(CRSSIO:::has_intervening(x, "annual"))
  expect_equal(CRSSIO:::n_trace(x), 3)
  expect_identical(start(x), as.yearmon("Jan 2000"))
  expect_identical(end(x), as.yearmon("Dec 2002"))
  expect_true(all(CRSSIO:::sites(x) == nf_gage_abbrv()))
})

# process check ---------------------------
test_that("2 processes yield the same result", {
  expect_identical(
    ism(crssi1),
    crssi(ism(nfd_ann), ism(sac1), 1.1)
  )
})
