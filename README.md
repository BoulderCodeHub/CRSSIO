
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CRSSIO

*Stable version (v0.6.3):* [![Travis-CI Build
Status](https://travis-ci.org/BoulderCodeHub/CRSSIO.svg?branch=master)](https://travis-ci.org/BouldercodeHub/CRSSIO)
[![AppVeyor Build
status](https://ci.appveyor.com/api/projects/status/tkbmrk4hosv96rin?svg=true)](https://ci.appveyor.com/project/BoulderCodeHub/crssio)
[![codecov](https://codecov.io/gh/BoulderCodeHub/CRSSIO/branch/master/graphs/badge.svg)](https://codecov.io/gh/BoulderCodeHub/CRSSIO)

*Development version:* [![Travis-CI Build
Status](https://travis-ci.org/rabutler/CRSSIO.svg?branch=master)](https://travis-ci.org/rabutler/CRSSIO)
[![AppVeyor Build
status](https://ci.appveyor.com/api/projects/status/88fep06n341s4kdb?svg=true)](https://ci.appveyor.com/project/BoulderCodeHub/crssio-kvvpl)
[![codecov](https://codecov.io/gh/rabutler/CRSSIO/branch/master/graphs/badge.svg)](https://codecov.io/gh/rabutler/CRSSIO)

R Package to manage code for manipulating the input and output data for
CRSS.

## Usage

The CRSSIO package includes functions for manipulating and creating
input data necessary to run CRSS and for processing CRSS output data. It
uses a “prefix\_verb\_noun” naming system for most functions. Functions
that create input for CRSS use the `crssi_` prefix, while those that
process CRSS output use the `crsso_` prefix. The `nf_`, `natsalt_`, and
`ism_` prefixes are also used for multiple functions that deal with
natural flow names, natural salt names, and with ISM related functions.

Functions that use the `create` verb will create files on the user’s
computer. Functions that use the `change` verb will edit files that
already exist on the user’s computer. Functions that use the `get` verb
will return data.

The final noun of the function describes what is created or retrieved.
Ex: `crssi_create_dnf_files()` creates the DNF (direct natural flow)
files.

### `crssi_`

  - Create CRSS input files with `crssi_create_dnf_files()`,
    `crssi_create_cmip_nf_files()`, and `crssi_create_hist_nf_xlsx()`
      - These files can also be created with a GUI through an R Studio
        Addin (see `?crss_input_addin`)
  - Modify existing CRSS natural flow input files with
    `crssi_change_nf_start_date()`, `crssi_change_nf_file_names()`, and
    `crssi_change_evap_files()`
  - `trimCCNFFiles` trims the climate change hydrology input files to a
    specified time period

### `crsso_`

  - `sys_cond_matrix()` and `crsso_get_sys_cond_table()` help create the
    standard System Conditions Table from CRSS output. Commonly referred
    to as the “5-year table” but it can go through as many years as
    simulation data exists. Ex:

<!-- end list -->

``` r
library(CRSSIO)
library(RWDataPlyr) # install_github("BoulderCodeHub/RWDataPlyr")
# create the RiverWare data aggregator
rwa <- sys_cond_rwa()

# use example data in RWDataPlyr to create system condition table
# first get all of the data
scenFolder <- "ISM1988_2014,2007Dems,IG,Most"
scenName <- "scenA"
scenPath <- system.file('extdata','Scenario/',package = 'RWDataPlyr')
sysData <- RWDataPlyr::rdf_aggregate(
  rwa,
  rdf_dir = file.path(scenPath, scenFolder),
  scenario = scenName
)

# then create the system condition table
sysCondTable <- crsso_get_sys_cond_table(sysData, 2018:2022)
# sysCondTable[['limitedTable']] to access results
```

### Natural Flow and Salt Names

  - Vectors of the natural flow gage names (`nf_gage_names()`), along
    with corresponding CRSS natural inflow input slot names
    (`nf_file_names()`), corresponding CRSS natural salt input slot
    names (`natsalt_file_names()`), and corresponding abbreviated, i.e.,
    variable, names (`nf_gage_abbrv()`).

### Other CRSS Functions

  - `elevation_to_storage()` converts elevations to storage values for
    reservoirs modeled in CRSS.
  - `ism_get_site_matrix()` applies the index sequential method (ISM) to
    a single time series of data.

## Installation

Only available from GitHub. Use the following to install:

``` r
install.packages('devtools')
library(devtools)
devtools::install_github('BoulderCodeHub/CRSSIO')
```

## Log:

For details, see the [News](NEWS.md)

  - 2018-11-29: version 0.6.3 available
  - 2018-03-23: version 0.6.2 available
  - 2018-01-23: version 0.6.1 available
  - 2018-01-17: version 0.6.0 available
  - 2017-08-30: version 0.5.0 available
  - 2017-05-10: version 0.4.1 available
  - 2017-03-31: version 0.4.0 available
  - 2016-10-04: version 0.3 available
  - 2016-05-30: version 0.2.1 available
  - 2016-05-05: version 0.2 available
  - 2015-02-10: version 0.1 available
