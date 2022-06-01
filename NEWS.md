CRSSIO 0.8.2.9000
=================

*In development*

- added `write_nfd()` to create csv and Excel files from `nfd` and `crss_nf` objects.
- added `sac_year_type_calc()`. It calculates the Sacramento Water Year index based on either the Sacramento 4-river index water year volume or the Colorado River WY intervening natural flows. 
- `sac_year_type_get()` gains a `paleo` argument. When this is true, the index values are obtained from paleo data. 
  - added internal data from Meko et al. 2018's paleo reconstruction of the 4-river volume for use in this function. 
  - also updated internal historical index data to go through 2019. 
- added `as_nfd.data.frame()`, `as_nfd.list()`, `as_crss_nf.data.frame()`, and `as_crss_nf.list()` to convert data frames to `nfd` and `crss_nf` objects. (#82)
- added Meko et al. (2017) and Woodhouse et al. (2006) Lees Ferry paleo reconstrutions for use in package. (#114)

CRSSIO 0.8.2
=================

*Released March 24, 2021*

## Bug Fixes

- `reindex()` now fails if start year cannot be successfully converted to numeric. (#103)
- Now importing xts to ensure `reindex()`, `print.nfd()`, etc. work properly. (#107)
- Changed default for `n_sites` argument in `nfd()`. It now defaults to `NA` and is computed based on the shape of the data.

## New Features

- Added `as.data.frame.nfd()` to create data.frames from `nfd`, `crss_nf`, and `crssi` objects. (#83)
- Added xts method for `nfd_trim_ts()`.
- Added `nf_to_annual()` to sum monthly data to annual data. (#83)

CRSSIO 0.8.1
=================

*Released March 19, 2021*

## Bug Fixes

- Fixed bug in `crssi_create_dnf_files()` that was preventing file names from correctly being passed when specified
- Fixed bug in `crssi_change_nf_file_names()` that was preventing folder names from being constructed correctly if input parameters did not have trailing slash. 

## New Features
- Added version 5 names to `natsalt_file_names()` and `nf_file_names()` for files that will work with CRSS v5.x.x. 
- Added `as_nfd.crss_nf()` to convert `crss_nf` objects to `nfd` objects, which are less restrictive.
- Added `nf_to_total()` (#101) and `nf_to_intervening()` (#105) to convert between intervening and total natural flows. 


CRSSIO 0.8.0
=================

*Released June 12, 2020*

v0.8.0 marks a major enhancement to CRSSIO. CRSSIO now defines three classes for storing multi-trace natural flow data: `nfd`, `crss_nf`, and `crssi`. `crss_nf` inherits from `nfd` and `crssi` inherits from both `nfd` and `crss_nf`. These classes intend to make working with CRSS input natural flow data easier and more uniform.

## Major Enhancements

* New classes: `nfd`, `crss_nf` (#86), `crssi` (#85)
  * `nfd` stores multi-trace, multi-site data for annual and/or monthly time steps and for intervening and/or total natural flow (flow space). `nfd()` creates the object from arrays, matrices, or xts objects. The object can contain any/all of the four combinations of time step and flow space.
  * `crss_nf` objects require that there are exactly 29 sites: the 29 sites required for input into CRSS, and that there are monthly intervening natural flows, which are also required input to CRSS. Other time step and flow space combinations are allowed, but there must be monthly intervening data. 
  * `crssi` objects add additional required input into CRSS: the Sacramento year type index and a scenario number. 
* New methods for `nfd` type objects:
  * `nfd_extract()` - extract data from `nfd` objects by time, trace, site, time step, and/or flow_space. This is a stand in for `[]`. 
  * `nfd_trim_ts()` (#92) - trim `nfd` objects to "exact" calendar/water years, i.e., start and end in January and December or October and September, respectively. 
  * `has_overlapping_ts()` (#92) - determine if `nfd` object has monthly and annual data that overlap exactly (default) or at all (`exact = FALSE`)
  * `reindex()` (#91) - change the time component of the `nfd` object.
  * `ism()` (#88) - apply Index Sequential Method to a single trace in the `nfd` object.
    * With this generic, `ism_get_site_matrix()` is deprecated and will be removed in a future release.
  * `plot()` (#95) - `plot.nfd()` will plot the data in the `nfd` object. Because there can be many traces of data, the data are typically summarized using boxplots; however, annual data can be shown as individual lines using `which = "spaghetti"`. `which = cloud` shows the same info as boxplots but as filled ribbons. 
* `nfd_get_time()`, `nfd_get_site()`, and `nfd_get_trace()`  are used to get a single time step, site, or trace of data from `nfd` objects. (#84)
* `nfd_stats()` and `nfd_pdf()` are used to compute basic statistics (mean, max, min, variance, lag-1 correlation, and skew) for each trace in an `nfd` object and to compute PDFs for each trace. 
  * `plot.nfd_stats()` and `plot.nfd_pdf()` plot these statistics.
  
## Other Enhancements 

* The Sacramento year type data can now be downloaded and parsed directly from the website using `sac_year_type_get()`. Previously, these data were stored internal to the package, thus requiring this package to be updated every time a new year of data is added to the record. This internal data can now be used by using `internal = TRUE`, but the internal data will not be routinely updated. (#78)
* Removed deprecated functions. (#89)
* Switched `crssi_change_nf_file_names()`, `crssi_change_nf_start_date()` and `crssi_change_evap_files()` to use progress bar instead of printing new line for each trace.

## Bug Fixes

* The Excel file created by `crssi_create_hist_nf_xlsx()` had a `month` column that contained strings. This column is now formated as a `Date`. (#81)
* Fixed `add_secondary_y_conversoin()` and `stat_boxplot_custom()` to work with ggplot2 v3.3.0.

## Internals

* Updated `crssi_create_dnf_files()` to use the new classes and methods for storing natural flow data and applying ISM. (#90)

CRSSIO 0.7.4
=================

*Released January 24, 2020*

* Updated Sacramento year type data through 2018

CRSSIO 0.7.3
=================

*Released July 25, 2019*

## Bug fixes

* Updated the labels in `crsso_get_sys_cond_table()` to include elevations for all Powell tiers, and for normal conditions in the Lower Basin. 
* Reorderd the table returned by `crsso_get_sys_cond_table()` to show Lower Basin from highest (surplus) to lowest (shortage) conditions.
* Fixed concluding messesage in Shiny addin when creating CMIP files.
* Fixed on more variable in `crsso_create_cmip_nf_files()` that didn't get inot the last bug fix. 

CRSSIO 0.7.2
=================

*Released February 7, 2019*

## Bug fixes

* Updated `crsso_create_cmip_nf_files()` to work with new format of ncdf files. The ncdf files now use snake_case for all variable names, instead of camelCase.

CRSSIO 0.7.1
=================

*Released January 18, 2019*

## Bug fixes

* Fixed the mid-elevation release tier labels in `crsso_get_sys_cond_table()`. (#67)
* Changed the `period` function in `sys_cond_rwa()` to be `"eocy"`, instead of `"asis"`. This ensures that `crsso_get_sys_cond_table()` will work, even if the SystemConditions.rdf file has monthly data, instead of the expected annual data. (#67)

CRSSIO 0.7.0
=================

*Released January 17, 2019*

## New functions

* Added `stat_boxplot_custom()`. This allows the user to specify the extents of the whiskers using percentiles (`qs`) instead of the IQR. (#51)
* Added `storage_to_elevation()`. This is the inverse of `elevation_to_storage()`. (#40)
* Added `add_secondary_y_conversion()`. This function takes a ggplot and adds in a secondary axis with different units. The units are converted from the current label positions of the primary axis. (#49)

## Removed functions

* Removed `elevation2Volume()`. Since `storage_to_elevation()` was added in this release, this was removed for consistency of names between these two functions. The other functions deprecated in v0.6.0 will be removed in v0.8.0. 

## Bug fixes

* Updated the create files add-in so that the start year parameter stays visible for creation of historical and CMIP natural flows and the HistoricalNaturalFlows.xlsx file. (#47)
* Fixed the closing parenthesis in the level 2 and 3 shortage rows in `crsso_get_sys_cond_table()`. (#62)
* Updated `crssi_change_nf_start_date()` so that it only edits the year of the start date. This was required so that the MWD ICS files are correctly modified as they use a 12/31 year/month, while the other files use a 1/31 year/month. As part of this, the `startDate` argument is deprecated and replaced by `start_year`. The function now updates the README.txt file if it exists, or creates one if it doesn't exist. (#52)

CRSSIO 0.6.3
===================

*Released November 29, 2018*

Released to work with CoRiverNF v0.5.0. It will work with older versions, but to get 2016 natural flow files this patch release is necesary. 

## New Functions

* added `sys_cond_rwa()` object for use with RWDataPlyr > v0.5.0

## Under the hood updates

* updated test files to work with 2016 natural flow files since the Little Snake natural flow changed in the pre-1971 data
* modified how the CMIP3 tests work
* updated the Sacramento year type data to go through 2017
    * ensure that specified years exist in the Sacramento year type data (#55)

CRSSIO 0.6.2
==============

*Released March 22, 2018*

## Bug Fixes

* `createISMMatrix()` is now exported (it should be until next major release)

CRSSIO 0.6.1
==============

*Released January 23, 2018*

## Minor Updates and Bug Fixes

* Fixed Deprecated message in `createSysCondTable()` to state that `crsso_get_sys_cond_table` should be used.
* Updated the "Create CRSS Input Files"" Addin (#35)
    - Fixed issues with radio buttons not working
    - Changed overall layout
    - Added ability to create CMIP natural flow files
* Added tests for `storage_to_elevation()`

CRSSIO 0.6.0
==============

*Released January 17, 2018*

## New Features and Functions

* This release implements a new naming system for most functions that includes a "prefix_verb_noun" for most functions. (#38) Functions that create input for CRSS are prefixed with `crssi_` while those that process CRSS output use the `crsso_` prefix. The `nf_` and `ism_` prefixes are also used for multiple functions that deal with natural flow (nf) names and with ISM related functions. This results in deprecating many functions in favor of their newer version. The new and their respective old functions are:
    - `crssi_create_dnf_files()` = `createCRSSDNFInputFiles()`
    - `crsso_get_sys_cond_table()` = `createSysCondTable()`
    - `sys_cond_matrix()` = `sysCondSALMatrix()`
    - `crssi_change_nf_start_date()` = `changeStartDate()`
    - `crssi_change_evap_files()` = `changeStartDateForEvapAndAddZeros()`
    - `nf_file_names()` = `CRSSNFInputNames()`
    - `natsalt_file_names()` = `CRSSNatSaltInputNames()`
    - `nf_gage_names()` = `nfGageNames()`
    - `nf_gage_abbrv()` = `nfShortNames()`
    - `crssi_change_nf_file_names()` = `copyAndChangeNFFileNames()`
    - `ism_get_site_matrix()` = `createISMMatrix()`
    - `elevation_to_storage()` = `elevation2Volume()`
* Additionally, `crssi_create_dnf_files()`, which replaces `createCRSSDNFInputFiles()`, gains an `overwriteFiles` argument that will ensure users do not accidently overwrite existing natural flow files. (#34) 
    - `startDate` and `simYrs` are replaced by `startYear` and `endYear`
* `crssi_create_hist_nf_xlsx()` is a new function that will standardize the creation of the HistoricalNaturalFlows.xlsx file that CRSS relies on. (#29)
* `crssi_create_cmip_nf_files()` is a new function that will create natural flow files from a netcdf file that contains cmip3/5 data. (#30)
    - The netcdf file is provided by the user (not included in the package).
* The RStudio AddIn that creates natural flow files from the observed natural flow record using the ISM was updated to use `crssi_create_dnf_files()`. The ability to create the HistoricalNaturalFlows.xlsx file was also added. (#35)

## Minor Updates and Bug Fixes

* `trimCCNFFiles()` now only trims natural flow and salt files, not all files in the directory. (#31)
    * it is also deprecated in favor of `crssi_create_cmip_nf_files()`. (#32)
    * it gains a `force` argument that must be `TRUE` to proceed. If it's `FALSE` (default), it won't work, which will hopefully encourage the use of `crssi_create_cmip_nf_files()`.
* Changed the default slot name for the Sacramento Year Type Index, which is set by the `crssio.sacYTSlot` option. The default is now "MWD_ICS.SacWYType". (#33)
* Formatting and other updates to conform to `goodpractice::gp()`.

CRSSIO 0.5.0
===============

*Released August 30, 2017* 

## New Features and Functions

* `createISMMatrix()` now works with annual data as well as monthly data. (#24)
* `createCRSSDNFInputFiles()` now creates Sacramento year type index using ISM and saves it (#25)
* new function: `elevation2Volume()` that will convert reservoir elevation values to resevoir storage values

## Minor Updates and Bug Fixes

* `createSysCondTable()` now checks to see if `yrs` exist in the data and reacts properly. (#26)
* package now depends on xts >= 0.10.0 (#17)
* Improved unit tests

# CRSSIO 0.4.1

*Released May 10, 2017*

## Bug Fixes

* Modified `changeStartDate()` so that it works for natural flow files created by `CRSSIO` >= v0.4.0 (#20)

# CRSSIO 0.4.0

*Released March 31, 2017*

## Major new features

* When the observed natural flow input files are created (`createCRSSDNFInputFiles`), a README file is generated to provide information about who/when/how the natural input files were created. (#13)
* Three additional slots are now created when running `createCRSSDNFInputFiles`: MeadFloodControlData.hydrologyIncrement, HydrologyParameters.SupplyScenario, and HydrologyParameters.TraceNumber. These slots will be imported into CRSS in the next CRSS version, so they need to be created when creating the other natural inflow files. The slot names that are created are set by the `crssio.hydroIncrement`, `crssio.supplyScenarioSlot`, and `crssio.traceNumberSlot` options. (#16 and #18)
* New function: `sysCondSALMatrix`. This function easily allows the user to create the matrix that is necessary for `RWDataPlyr::createSlotAggList`, as part of the progression of creating the system condition table. The example in `createSysCondTable` help file now provides an example of this progression. (#15)

## Minor improvements

* `trimCCNFFiles` will now provide an error message if the user specified dates do not exist in the provided flow files. (#12)
