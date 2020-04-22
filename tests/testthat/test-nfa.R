# nfa() with only 1 value for data ---------------------
test_that("nfa() with only 1 value for data", {
  expect_is(x <- nfa(), "nfa")
  expect_equal(dim(x), c(1, 1, 29))
  expect_equivalent(as.array(x[1,1,]), array(NA, dim = c(1, 1, 29)))
  expect_identical(attr(x, "flow_space"), "intervening")
  expect_setequal(attr(x, "dimnames")[[3]], nf_gage_abbrv())
  
  expect_is(
    x <- nfa(-99, c(24, 10), start_yearmon = "2000-01", flow_space = "total"), 
    "nfa"
  )
  expect_equal(dim(x), c(24, 10, 29))
  expect_equivalent(as.array(x[1,1,]), array(-99, dim = c(1, 1, 29)))
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
a2 <- array(NA, dim = c(24, 10, 29))
for (i in 1:9) {
  a2[, i, ] <- i
}
a2[, 10, ] <- a1

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
  
  expect_is(x2 <- nfa(a2, flow_space = "total", start_yearmon = "2000-10"), "nfa")
  expect_equal(colnames(x2), paste0("Trace", 1:10))
  expect_equivalent(x2[, 10, ], x)
})

# matrix ----------------------------
m1 <- matrix(rep(1:24, 29), ncol = 29, byrow = FALSE)
test_that("nfa() works with matrices", {
  expect_is(x <- nfa(m1), "nfa")
  expect_identical(x, as_nfa(m1))
  expect_identical(x, nfa(a1))
  
  expect_is(x2 <- nfa(m1, start_yearmon = "2000-10", flow_space = "total"), "nfa")
  expect_equivalent(x2, x)
  expect_equal(tail(rownames(x2), 1), "2002-09")
})

x <- nfa(a1)
# print ----------------------------
test_that("print.nfa() works", {
  expect_identical(expect_output(print(x)), x)
})

# head and tail-----------------------------
test_that("head.nfa() and tail.nfa() work", {
  expect_is(h1 <- head(x), "matrix")
  expect_is(t1 <- tail(x), "matrix")
  expect_equal(dim(h1), c(6, 29))
  expect_equal(dim(t1), c(6, 29))
  expect_setequal(colnames(h1), nf_gage_abbrv())
  expect_setequal(colnames(t1), nf_gage_abbrv())
  expect_equal(rownames(h1)[3], paste0(yy, "-03"))
  expect_equal(rownames(t1)[5], paste0(yy + 1, "-11"))
  expect_equal(dim(head(x, 4)), c(4, 29))
  expect_equal(dim(tail(x, 12)), c(12, 29))
  expect_equal(dim(head(x, -1)), c(23, 29))
  expect_equal(dim(tail(x, -6)), c(18, 29))
})

#  assignment -----------------------------
x <- nfa(a2)
test_that("assignment works", {
  # before assignment
  expect_equivalent(as.matrix(x[,1,1]), matrix(rep(1, 24), ncol = 1))
  # assignment
  x[,1,1] <- -42.2
  expect_is(x, "nfa")
  # assignment does not change attributes
  expect_equal(attr(x, "flow_space"), "intervening")
  # assignment worked
  expect_equivalent(as.matrix(x[,1,1]), matrix(rep(-42.2, 24), ncol = 1))
})
