# create crssi
# ISM
# write out data then test it

sink('nul')

nf <- crssi(
  crss_nf(CoRiverNF::monthlyInt["2000/2001"]),
  sac_year_type_get(TRUE)["2000/2001"],
  scen_number = 1.20002001,
  scen_name = "ISM 2000-2001"
)

nf <- ism(nf)

sink()

path <- file.path(tempdir(), "write_crssi")
dir.create(path)
teardown(unlink(path, recursive = TRUE))

all_files <- c(
  nf_file_names(), getOption("crssio.supplyScenarioSlot"),
  getOption("crssio.traceNumberSlot"), 
  getOption("crssio.hydroIncrement"), getOption("crssio.sacYTSlot")
)

test_that("write_crssi works with generic crssi object", {
  expect_is(write_crssi(nf, path), "crssi")
  expect_setequal(list.files(path), c("trace1", "trace2", "README.txt"))
  expect_setequal(list.files(file.path(path, "trace1")), all_files)
  expect_setequal(list.files(file.path(path, "trace2")), all_files)
})

