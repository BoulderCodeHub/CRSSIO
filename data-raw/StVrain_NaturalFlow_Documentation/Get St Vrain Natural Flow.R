# Get St Vrain Natural Flow
# Combo of StateMod and regression equation from state of CO
# Nathan Bonham
# 8/6/26

library(dplyr)
library(tidyr)
library(lubridate)
library(readr) # read_table

rm(list=ls()); gc()

### Get modeled natural flow from Southe Platte StateMod ###

# Read entire file as text

xbm_path=file.path('data', 'SP2016_BFx.xbm')

# Find first data row (starts with a 4‑digit year followed by station ID)
txt <- readLines(xbm_path)
data_start <- which(grepl("^\\s*\\d{4}\\s+\\S+\\s+\\d", txt))[1]

# Column names (15 columns)
cols <- c(
  "Yr", "Station_ID",
  "JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC",
  "TOTAL"
)

# Read the data from that line onward
df <- read_table(
  xbm_path,
  skip = data_start - 1,
  col_names = cols
)

df$DEC=as.numeric(
  sub("\\.$", "", df$DEC)
)

### Get St Vrain Lyons CO ###

df=df %>% filter(Station_ID== '06724000')

range(df$Yr)

df=df %>% rename(Year=Yr)

###### compute the rest of years via a regression ###

# load CY gaged flow

StVrain=read.csv(file.path('outdata', 'Historical CY Flow - St Vrain Lyons.csv'))[,-1]

names(StVrain)=c( 'Year' ,'Gaged')

StVrain=StVrain %>% left_join(df %>% select(Year, TOTAL), by='Year') %>% rename(Natural=TOTAL)

# NAs created in each year we do not have modeled natural flow.
# Need to use a regression to estimate

noNA=StVrain %>% filter(!(is.na(Gaged)|is.na(Natural)))

### seeing if a LM fit in R matches the equation CO provided.
summary(
lm(Natural ~ Gaged, data=noNA)
)
# Yes, same slope and intercept. R2 of 0.98

# predict NF in the years it is not modeled in StateMod

StVrain = StVrain %>% mutate(
  
  Natural =
  
  case_when(
    is.na(Natural) ~ 1.0859*Gaged + 21873,
    !is.na(Natural) ~ Natural
  )
  
)

write.csv(StVrain, file.path('outdata', 'Gaged and Natural Flow - St Vrain Lyons.csv'))





