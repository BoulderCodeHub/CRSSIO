source("data-raw/parseSacYTData.R")

# get the elevation volume tables, and save them as system data

res <- c("blueMesa", "flamingGorge", "mead", "navajo", "powell") # reservoirs

getData <- function(res) 
{
  tmp <- read.csv(file.path("data-raw", paste0(res, "ElevationVolume.csv")))
  tmp
}

evTables <- lapply(res, getData)
names(evTables) <- tolower(res)

# and get the sacramento year type data too
sacYT <- parseSACData("data-raw/sacrementoData.txt", 1906:2015)

devtools::use_data(evTables, sacYT, internal = TRUE, overwrite = TRUE)
