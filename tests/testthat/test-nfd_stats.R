sink('nul')
xx <- nfd(CoRiverNF::cyAnnTot, flow_space = "total", n_sites = 29)
xx2 <- ism(xx, n_years_keep = 50)

mm <- crss_nf(CoRiverNF::monthlyInt) 
mm <- nfd_extract(mm, "1906/") 
mm2 <- ism(mm, n_years_keep = 50)

ann1 <- nfd_stats(xx, "LeesFerry", "total", "annual")
ann2 <- nfd_stats(xx2, "LeesFerry", "total", "annual")
mon1 <- nfd_stats(mm, "Cameo", "intervening", "monthly")
mon2 <- nfd_stats(mm2, "Cameo", "intervening", "monthly")
sink()

test_that("nfd_stats() returns correctly", {
  expect_is(ann1, c("nfd_stats", "data.frame"))
  expect_is(ann2, c("nfd_stats", "data.frame"))
  expect_is(mon1, c("nfd_stats", "data.frame"))
  expect_is(mon2, c("nfd_stats", "data.frame"))
  
  expect_identical(CRSSIO:::nfd_stats_verify(ann1), ann1)
  expect_identical(CRSSIO:::nfd_stats_verify(ann2), ann2)
  expect_identical(CRSSIO:::nfd_stats_verify(mon1), mon1)
  expect_identical(CRSSIO:::nfd_stats_verify(mon2), mon2)
  
  expect_equal(nrow(ann1), 6)
  expect_equal(nrow(ann2), 6 * CRSSIO:::n_trace(xx2))
  expect_equal(nrow(mon1), 6 * 12)
  expect_equal(nrow(mon2), 6 * 12 * CRSSIO:::n_trace(mm2))
})

test_that("plot.nfd_stats() generally works", {
  expect_is(p1 <- plot(ann1, show = FALSE), "nfdplot")
  expect_is(p2 <- plot(ann2, show = FALSE), "nfdplot")
  expect_is(p3 <- plot(ann2, ann1, base_units = "acre-ft", show = FALSE), 
            "nfdplot")
  
  expect_is(p4 <- plot(mon1, show = FALSE), "nfdplot")
  expect_is(p5 <- plot(mon2, show = FALSE), "nfdplot")
  expect_is(p6 <- plot(mon2, mon1, base_units = "acre-ft", show = FALSE), 
            "nfdplot")
  
  expect_is(ggplot2::ggplot_build(p1[[1]]), "ggplot_built")
  expect_is(ggplot2::ggplot_build(p2[[1]]), "ggplot_built")
  expect_is(ggplot2::ggplot_build(p3[[1]]), "ggplot_built")
  expect_is(ggplot2::ggplot_build(p4[[1]]), "ggplot_built")
  expect_is(ggplot2::ggplot_build(p5[[1]]), "ggplot_built")
  expect_is(ggplot2::ggplot_build(p6[[1]]), "ggplot_built")
})
