# nfa() with only 1 value for data ---------------------
test_that("nfa() with only 1 value for data", {
  expect_is(x <- nfa(), "nfa")
  expect_equal(dim(x), c(1, 1, 29))
  expect_equivalent(x[1,1,], rep(NA, 29))
  
  expect_is(x <- nfa(-99, c(24, 10), start_year = 2000), "nfa")
  expect_equal(dim(x), c(24, 10, 29))
  expect_equivalent(x[1,1,], rep(-99, 29))
  expect_true(all(x == -99))
  expect_equal(rownames(x)[1], "2000-01")
})
