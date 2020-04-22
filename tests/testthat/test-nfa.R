# nfa() with only 1 value for data ---------------------
test_that("nfa() with only 1 value for data", {
  expect_is(x <- nfa(), "nfa")
  expect_equal(dim(x), c(1, 1, 29))
  expect_equivalent(x[1,1,], rep(NA, 29))
  expect_identical(attr(x, "flow_space"), "intervening")
  expect_setequal(attr(x, "dimnames")[[3]], nf_gage_abbrv())
  
  expect_is(
    x <- nfa(-99, c(24, 10), start_yearmon = "2000-01", flow_space = "total"), 
    "nfa"
  )
  expect_equal(dim(x), c(24, 10, 29))
  expect_equivalent(x[1,1,], rep(-99, 29))
  expect_true(all(x == -99))
  expect_equal(rownames(x)[1], "2000-01")
  expect_equal(tail(rownames(x), 1), "2001-12")
  expect_identical(attr(x, "flow_space"), "total")
  expect_setequal(attr(x, "dimnames")[[3]], nf_gage_abbrv())
  
  expect_is(x <- nfa(dim = c(36, 2), start_yearmon = "Oct 2005"), "nfa")
  expect_equal(rownames(x)[1], "2005-10")
  expect_equal(tail(rownames(x), 1), "2008-09")
})

# arrays ---------------------------
a1 <- array(rep(1:24, 29), dim = c(24, 1, 29))
yy <- as.numeric(format(Sys.Date(), "%Y"))
mm <- expand.grid(sprintf("%02d", 1:12), yy:(yy+1))
mm <- paste0(mm[,2], "-", mm[,1])
test_that("nfa() works with arrays", {
  expect_is(x <- nfa(a1), "nfa")
  expect_identical(dim(x), dim(a1))
  expect_identical(
    dimnames(x), 
    list(mm, "Trace1", nf_gage_abbrv())
  )
  expect_equal(attr(x, "flow_space"), "intervening")
})

