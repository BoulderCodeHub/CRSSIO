library(dplyr)
library(tidyr)
library(lubridate)

rm(list=ls()); gc()

cfs=read.csv(file.path('data', 'Selected_Station_Analysis_Xtab_202608051549.csv'))

cfs=cfs %>% select(-X)

#### data QC ######
####################

### any duplicate dates?

nrow(cfs)==n_distinct(cfs$meas_date) # returns T if each row is a distinct date

### are there any missing dates?
cfs$Date=as.Date(cfs$meas_date, format = '%m/%d/%Y')

all_dates= data.frame(
  
  Date=
  seq.Date(
    
    from = min(cfs$Date),
    to=max(cfs$Date),
    by = '1 day'
    
  )
  
)

before=nrow(cfs)

cfs= all_dates %>% left_join(cfs, by='Date') # will create NAs if any missing days

before==nrow(cfs) # should be true if no missing dates

### Any NAs?

sum(is.na(cfs$Streamflow.Value))

testNA=cfs %>% filter(is.na(Streamflow.Value))

years_wNA=unique(year(testNA$Date))
print(years_wNA)# missing data occurs in 1903, 1904, and 2013
# 1903 and 1904 will be removed later since first year of CRB natural flow is 1906.
# 2013 NA in September. Likely due to historic 2013 floods. This is probably a good year to remove from
# model training/testing, anyway. 
# So, no need to fill NAs

############## daily cfs to annual acre-feet ########

### CY

CY=cfs %>% 
  mutate(
    Year=year(Date),
    Month=month(Date),
    af=Streamflow.Value * (24*60*60) / 43560 # convert sec to day then cf to af
    
    ) %>%
  group_by(Year) %>%
  summarize(
    af=sum(af)
  )

CY$Year[which(is.na(CY$af))] # expect 1903, 1904, 2013

if(!dir.exists('outdata')){
  dir.create('outdata')
}

write.csv(CY, file.path('outdata', 'Historical CY Flow - St Vrain Lyons.csv'))

