# as.data.frame.nfd -------------------------------
sink('nul')
nf <- nfd(
  CoRiverNF::monthlyInt["2000/2002"],
  flow_space = "intervening",
  time_step = "monthly",
  n_sites = 29
)

nf2 <- ism(nf)

nf3 <- nfd(
  CoRiverNF::cyAnnTot["1990/1999"], flow_space = "total", time_step = "annual", 
  n_sites = 29
)

nf3 <- nf_to_intervening(nf3)
sink()

test_that("as.data.frame.nfd works", {
  expect_s3_class(df <- as.data.frame(nf), "data.frame")
  expect_equal(ncol(df), 29 + 4)
  expect_equal(nrow(df), 3 * 12)
  expect_equal(unique(df$trace), 1)
  expect_true(!anyNA(df$date))
  
  expect_s3_class(df <- as.data.frame(nf, wide = FALSE), "data.frame")
  expect_equal(ncol(df), 6)
  expect_equal(nrow(df), 3 * 12 * 29)
  expect_equal(unique(df$trace), 1)
  expect_true(!anyNA(df$date))
  
  expect_s3_class(df <- as.data.frame(nf2), "data.frame")
  expect_equal(ncol(df), 29 + 4)
  expect_equal(nrow(df), 3 * 12 * 3)
  expect_equal(unique(df$trace), 1:3)
  
  expect_s3_class(df <- as.data.frame(nf2, wide = FALSE), "data.frame")
  expect_equal(ncol(df), 6)
  expect_equal(nrow(df), 3 * 12 * 29 * 3)
  expect_equal(unique(df$trace), 1:3)
  
  expect_s3_class(df <- as.data.frame(nf3), "data.frame")
  expect_equal(ncol(df), 29 + 4)
  expect_equal(nrow(df), 10 * 2)
  expect_equal(unique(df$trace), 1)
  
  expect_s3_class(df <- as.data.frame(nf3, wide = FALSE), "data.frame")
  expect_equal(ncol(df), 6)
  expect_equal(nrow(df), 10 * 29 * 2)
  expect_equal(unique(df$trace), 1)
  expect_length(unique(df$site), 29)
})

# as.data.frame.crssi -----------------------------
# this also tests as.data.frame.crss_nf by inheritance and they way 
# as.data.frame.crssi is coded

sink('nul')
sac <- sac_year_type_get(internal = TRUE)["2000/2002"]
nf <- crssi(as_crss_nf(nf), sac, scen_number = 1.20002002)
sink()

test_that("as.data.frame.crssi() works", {
  expect_s3_class(df <- as.data.frame(nf), "data.frame")
  expect_equal(ncol(df), 29 + 5)
  expect_equal(nrow(df), 12 * 3)
  expect_equal(unique(df$trace), 1)
  expect_true(!anyNA(df$date))
  
  expect_s3_class(df <- as.data.frame(nf, wide = FALSE), "data.frame")
  expect_equal(ncol(df), 6)
  expect_equal(nrow(df), 12 * 3 * 29 + 3)
  expect_equal(unique(df$trace), 1)
  expect_length(unique(df$site), 30)
  expect_true(!anyNA(df$date))
})
