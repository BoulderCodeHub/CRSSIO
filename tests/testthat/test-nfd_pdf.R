xx <- nfd(CoRiverNF::cyAnnTot, flow_space = "total", n_sites = 29)
xx2 <- ism(xx, n_years_keep = 50)

mm <- crss_nf(CoRiverNF::monthlyInt) 
mm <- nfd_extract(mm, "1906/") 
mm2 <- ism(mm, n_years_keep = 50)

# nfd_pdf --------------------------------------
test_that("nfd_pdf returns correctly", {
  # annual ------------------------------------
  expect_is(
    ann1 <- nfd_pdf(xx, "Imperial", "total", "annual", breaks = 50),
    c("nfd_pdf", "data.frame")
  )
  expect_identical(CRSSIO:::nfd_pdf_verify(ann1), ann1)
  expect_equal(nrow(ann1), 50)
  expect_is(cur_breaks <- nfd_pdf_get_breaks(ann1), "numeric")
  expect_length(cur_breaks, 50)
  expect_is(
    ann2 <- nfd_pdf(xx2, "Imperial", "total", "annual", breaks = cur_breaks),
    c("nfd_pdf", "data.frame")
  )
  expect_identical(CRSSIO:::nfd_pdf_verify(ann1), ann1)
  expect_equal(nrow(ann2), 50 * CRSSIO:::n_trace(xx2))
  
  # monthly ------------------------------------
  expect_is(
    mon1 <- nfd_pdf(mm, "Watson", "intervening", "monthly", 
                    which = c(4,7), breaks = 40),
    c("nfd_pdf", "data.frame")
  )
  expect_identical(CRSSIO:::nfd_pdf_verify(mon1), mon1)
  expect_equal(nrow(mon1), 40 * 2)
  expect_is(mon_breaks <- nfd_pdf_get_breaks(mon1), "matrix")
  expect_equal(dim(mon_breaks), c(40, 2))
  expect_is(
    mon2 <- nfd_pdf(mm2, "Watson", "intervening", "monthly",
                    which = c(4,7), breaks = mon_breaks),
    c("nfd_pdf", "data.frame")
  )
  expect_identical(CRSSIO:::nfd_pdf_verify(mon2), mon2)
  expect_equal(nrow(mon2), 40 * 2 * CRSSIO:::n_trace(mm2))
  
  expect_is(
    mon1 <- nfd_pdf(mm, "Watson", "intervening", "monthly", which = c(2)),
    c("nfd_pdf", "data.frame")
  )
  expect_is(
    mon1 <- nfd_pdf(mm, "Watson", "intervening", "monthly", which = 1:12),
    c("nfd_pdf", "data.frame")
  )
})

# plot.nfd_pdf ---------------------------------------
test_that("plot.nfd_pdf() works", {
  ann1 <- nfd_pdf(xx, "Imperial", "total", "annual")
  cur_breaks <- nfd_pdf_get_breaks(ann1)
  ann2 <- nfd_pdf(xx2, "Imperial", "total", "annual", breaks = cur_breaks)
  
  mon1 <- nfd_pdf(mm, "Watson", "intervening", "monthly", which = c(4,7))
  mon_breaks <- nfd_pdf_get_breaks(mon1)
  mon2 <- nfd_pdf(mm2, "Watson", "intervening", "monthly", 
                  which = c(4,7), breaks = mon_breaks)
  
  # annual -----------------------------
  expect_is(gg <- plot(ann1, show = FALSE), "nfdplot")
  expect_is(ggplot2::ggplot_build(gg[[1]]), "ggplot_built")
  expect_length(gg, 1)
  
  expect_is(gg <- plot(ann2, base_units = "af", show = FALSE), "nfdplot")
  expect_is(ggplot2::ggplot_build(gg[[1]]), "ggplot_built")
  expect_length(gg, 1)
  
  expect_is(gg <- plot(ann2, ref = ann1, show = FALSE), "nfdplot")
  expect_is(ggplot2::ggplot_build(gg[[1]]), "ggplot_built")
  expect_length(gg, 1)
  
  # monthly -----------------------------
  expect_is(gg <- plot(mon1, show = FALSE), "nfdplot")
  expect_is(ggplot2::ggplot_build(gg[[1]]), "ggplot_built")
  expect_length(gg, 2)
  
  expect_is(gg <- plot(mon2, base_units = "af", show = FALSE), "nfdplot")
  expect_is(ggplot2::ggplot_build(gg[[2]]), "ggplot_built")
  expect_length(gg, 2)
  
  expect_is(gg <- plot(mon2, ref = mon1, show = FALSE), "nfdplot")
  expect_is(ggplot2::ggplot_build(gg[[1]]), "ggplot_built")
  expect_length(gg, 2)
})
