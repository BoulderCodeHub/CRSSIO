sink('nul')

d1 <- ism(nfd(CoRiverNF::cyAnnTot["2000/2005"]))
d2 <- ism(nfd(CoRiverNF::monthlyInt["2000/2002"], time_step = "monthly",
              flow_space = "intervening"))
d2 <- nf_to_annual(d2)
d2 <- nf_to_total(d2)

sink()

p1 <- file.path(tempdir(), "d1")
p2 <- file.path(tempdir(), "d2")
dir.create(p1)
dir.create(p2)
teardown({
  unlink(p1, recursive = TRUE)
  unlink(p2, recursive = TRUE)
})

# write csv files ----------------------------
test_that("write_nfd() works for csv files", {
  write_nfd(d1, p1)
  expect_true(dir.exists(file.path(p1, "annual_total")))
  expect_true(
    all(file.exists(
      file.path(p1, "annual_total", paste0(nf_gage_abbrv(), ".csv"))
    ))
  )
  expect_error(write_nfd(d1, p1))
  expect_length(list.dirs(p1), 2)
  expect_type(write_nfd(d2, p2, overwrite = TRUE), "list")
  
  expect_type(
    write_nfd(d2, p2, trace_names = month.name[1:3], insert_name = "zz"),
    "list"
  )
  expect_length(list.dirs(p2), 5)
  file_names <- paste0("zz_", nf_gage_abbrv(), ".csv")
  expect_true(
    all(file.exists(
      file.path(p2, "annual_total", file_names)
    ))
  )
  expect_true(
    all(file.exists(
      file.path(p2, "annual_intervening", file_names)
    ))
  )
  expect_true(
    all(file.exists(
      file.path(p2, "monthly_total", file_names)
    ))
  )
  expect_true(
    all(file.exists(
      file.path(p2, "monthly_intervening", file_names)
    ))
  )
  expect_error(write_nfd(d2, p2, insert_name = "zz"))
  
  # check trace names
  f1 <- read.csv(file.path(p1, "annual_total", "Cameo.csv"))
  f2 <- read.csv(file.path(p2, "annual_total", "zz_Cameo.csv"))
  
  expect_true(all(colnames(f1) == c("X", paste0("Trace", 1:6))))
  expect_true(all(colnames(f2) == c("X", month.name[1:3])))
})

# write excel files --------------------------
test_that("write_nfd() works for Excel files", {
  expect_type(write_nfd(d1, p1, format = "excel"), "list")
  expect_true(file.exists(file.path(p1, "annual_total.xlsx")))
  df <- readxl::read_excel(file.path(p1, "annual_total.xlsx"), "Cameo")
  expect_true(all(colnames(df) == c("timestep", paste0("Trace", 1:6))))
  expect_error(write_nfd(d1, p1, format = "excel"))
  expect_type(
    write_nfd(
      d2, p2, format = "excel", insert_name = "xx", 
      trace_names = month.name[1:3]
    ), 
    "list"
  )
  file_names <- c("xx_annual_total.xlsx", "xx_annual_intervening.xlsx",
                  "xx_monthly_total.xlsx", "xx_monthly_intervening.xlsx")
  expect_true(all(file.exists(file.path(p2, file_names))))
  df <- readxl::read_excel(file.path(p2, "xx_monthly_total.xlsx"), "Bluff")
  expect_true(all(colnames(df) == c("timestep", month.name[1:3])))
})
