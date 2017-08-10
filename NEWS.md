# CRSSIO 0.4.1.9000

*In development* 

- package now depends on xts >= 0.10.0 (#17)

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