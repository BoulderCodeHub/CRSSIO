## CRSSIO Package
R Package to manage code for manipulating the input/output for CRSS.

Only available from GitHub. Use the following to install:
```
install.packages('devtools')
library(devtools)
devtools::install_github('rabutler/CRSSIO')
```

Includes:
* code to create CRSS natural flow input files from posted natural flow data (http://www.usbr.gov/lc/region/g4000/NaturalFlow/current.html)
* function to change the start date of the natural flow input files
* function(s) to change the file names of the natural flow input files in order to use natural flow input files in either the older or new structure of CRSS
* function to create the standard System Conditions Table from CRSS output. Commonly refered to as the "5-year table" but it can go through as many years as simulation data exists.
* function to trim the climate change hydrology files: `trimCCNFFiles`.       
