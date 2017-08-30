# get the elevation volume tables, and save them as system data

res <- c("blueMesa", "flamingGorge", "mead", "navajo", "powell") # reservoirs

getData <- function(res) 
{
  tmp <- read.csv(file.path("data-raw", paste0(res, "ElevationVolume.csv")))
  tmp
}

evTables <- lapply(res, getData)
names(evTables) <- tolower(res)

devtools::use_data(evTables, internal = TRUE)
