CRSSIO 0.6.2
==============

*Released March 22, 2018*

## Bug Fixes

* `createISMMatrix()` is now exported (it should be until next major release)

CRSSIO 0.6.1
==============

## Minor Updates and Bug Fixes

* Fixed Deprecated message in `createSysCondTable()` to state that `crsso_get_sys_cond_table` should be used.
* Updated the "Create CRSS Input Files) Addin (#35)
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
