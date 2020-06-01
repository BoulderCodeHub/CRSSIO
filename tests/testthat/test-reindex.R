library(xts)

# reindex.xts -----------------------------
x1_ann <- xts(
  matrix(22, nrow = 4, ncol = 1), 
  order.by = as.yearmon("Dec 2020") + 0:3
)

x2_ann <- xts(
  matrix(75, nrow = 5, ncol = 4),
  order.by = as.yearmon("Sep 2020") + 0:4
)

x3_ann <- xts(22, order.by = as.yearmon("Dec 2020"))

x1_mon <- xts(
  matrix(22, nrow = 36, ncol = 1), 
  order.by = as.yearmon("Jan 2020") + 0:35/12
)

x2_mon <- xts(
  matrix(75, nrow = 17, ncol = 4),
  order.by = as.yearmon("Sep 2020") + 0:16/12
)

x3_mon <- xts(22, order.by = as.yearmon("Feb 2020"))

test_that("CRSSIO:::reindex.xts() works", {
  # annual
  expect_equivalent(coredata(x <- CRSSIO:::reindex.xts(x1_ann, 2000)), coredata(x1_ann))
  expect_identical(start(x), as.yearmon("Dec 2000"))
  expect_identical(end(x), as.yearmon("Dec 2003"))
  
  expect_equivalent(coredata(x <- CRSSIO:::reindex.xts(x2_ann, "2053")), coredata(x2_ann))
  expect_identical(start(x), as.yearmon("Sep 2053"))
  expect_identical(end(x), as.yearmon("Sep 2057"))
  
  expect_equivalent(coredata(x <- CRSSIO:::reindex.xts(x3_ann, "2019")), coredata(x3_ann))
  expect_identical(start(x), as.yearmon("Dec 2019"))
  expect_identical(end(x), as.yearmon("Dec 2019"))
  
  # monthly
  expect_equivalent(coredata(x <- CRSSIO:::reindex.xts(x1_mon, 2000)), coredata(x1_mon))
  expect_identical(start(x), as.yearmon("Jan 2000"))
  expect_identical(end(x), as.yearmon("Dec 2002"))
  
  expect_equivalent(coredata(x <- CRSSIO:::reindex.xts(x2_mon, "2053")), coredata(x2_mon))
  expect_identical(start(x), as.yearmon("Sep 2053"))
  expect_identical(end(x), as.yearmon("Jan 2055"))
  
  expect_equivalent(coredata(x <- CRSSIO:::reindex.xts(x3_mon, "2019")), coredata(x3_mon))
  expect_identical(start(x), as.yearmon("Feb 2019"))
  expect_identical(end(x), as.yearmon("Feb 2019"))
})
