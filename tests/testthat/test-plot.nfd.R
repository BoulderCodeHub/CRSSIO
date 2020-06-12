library(CoRiverNF)
x <- nfd(50, n_months = 144, n_trace = 20, n_sites = 3, flow_space = "both", 
    time_step = "both", start_yearmon = "Oct 2000", year = "wy")
x2 <- nfd_extract(
  nfd(cyAnnTot, flow_space = "total", time_step = "annual", n_sites = 29), 
  "1980/"
)
x3 <- ism(
  nfd_extract(
    nfd(cyAnnInt, flow_space = "intervening", time_step = "annual", 
        n_sites = 29), 
    "1980/"
  )
)
x4 <- nfd_extract(
  nfd(monthlyTot, flow_space = "total", time_step = "monthly", n_sites = 29), 
  "2000/"
)
x5 <- crss_nf(monthlyInt, flow_space = "intervening", time_step = "monthly")
x6 <- ism(crssi(
  nfd_extract(x5, "2000/"), 
  sac_year_type_get(TRUE)["2000/"], 
  scen_number = -99
))

test_that("plot.nfd() works", {
  expect_is(
    gg <- plot(x, site = 2, 
               which = c("box", "spaghetti", "cloud"), show = FALSE), 
    "nfdplot"
  )
  expect_length(gg, 8)
  expect_is(ggplot2::ggplot_build(gg[[1]]), "ggplot_built")
  expect_is(ggplot2::ggplot_build(gg[[2]]), "ggplot_built")
  expect_is(ggplot2::ggplot_build(gg[[3]]), "ggplot_built")
  expect_is(ggplot2::ggplot_build(gg[[4]]), "ggplot_built")
  expect_is(ggplot2::ggplot_build(gg[[5]]), "ggplot_built")
  expect_is(ggplot2::ggplot_build(gg[[6]]), "ggplot_built")
  expect_is(ggplot2::ggplot_build(gg[[7]]), "ggplot_built")
  expect_is(ggplot2::ggplot_build(gg[[8]]), "ggplot_built")
  
  expect_is(
    gg <- plot(
      x2, site = "Bluff",time_step = "annual", flow_space = "total", 
      which = "box", show = FALSE
    ), 
    "nfdplot"
  )
  expect_length(gg, 1)
  expect_is(ggplot2::ggplot_build(gg[[1]]), "ggplot_built")
  
  expect_is(
    gg <- plot(
      x3, site = "Imperial", time_step = "annual", flow_space = "intervening", 
      which = c("cloud", "spaghetti"), show = FALSE
    ), 
    "nfdplot"
  )
  expect_length(gg, 2)
  expect_is(ggplot2::ggplot_build(gg[[1]]), "ggplot_built")
  expect_is(ggplot2::ggplot_build(gg[[2]]), "ggplot_built")
  
  expect_is(
    gg <- plot(x4, site = "Littlefield", time_step = "monthly", flow_space = "total", show = FALSE), 
    "nfdplot"
  )
  expect_length(gg, 1)
  expect_is(ggplot2::ggplot_build(gg[[1]]), "ggplot_built")
  
  expect_is(
    gg <- plot(x5, site = "Cameo", time_step = "monthly", flow_space = "intervening", 
               show = FALSE), 
    "nfdplot"
  )
  expect_length(gg, 1)
  expect_is(ggplot2::ggplot_build(gg[[1]]), "ggplot_built")
  
  expect_is(
    gg <- plot(x6, site = "LeesFerry", time_step = "monthly", 
               flow_space = "intervening", show = FALSE), 
    "nfdplot"
  )
  expect_length(gg, 1)
  expect_is(ggplot2::ggplot_build(gg[[1]]), "ggplot_built")
})
