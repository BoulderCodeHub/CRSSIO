source("data-raw/parseSacYTData.R")

# get the elevation volume tables, and save them as system data

message("Do you need to update the end year of sacramento yt data?")

res <- c("blueMesa", "flamingGorge", "mead", "navajo", "powell") # reservoirs

getData <- function(res) 
{
  tmp <- read.csv(file.path("data-raw", paste0(res, "ElevationVolume.csv")))
  tmp
}

evTables <- lapply(res, getData)
names(evTables) <- tolower(res)


# and get the sacramento year type data too
sacYT <- parseSACData("data-raw/sacrementoData.txt", 1906:2019)
  


# sacramento 4 gage paleo data ----------------
sac_paleo <- read.table("data-raw/sacramentofourupdate.txt", skip = 3, nrows = 1006)
sac_paleo2 <- read.table("data-raw/sacramentofourupdate.txt", skip = 1009)

sac_paleo <- rbind(sac_paleo, sac_paleo2[, 1:2])
sac_paleo$V1 <- zoo::as.yearmon(paste("Dec", sac_paleo$V1))

sac_paleo_wy_vol <- xts::xts(sac_paleo$V2, sac_paleo$V1)
colnames(sac_paleo_wy_vol) <- "WY_vol"

usethis::use_data(evTables, sacYT, sac_paleo_wy_vol, 
                  internal = TRUE, overwrite = TRUE)
