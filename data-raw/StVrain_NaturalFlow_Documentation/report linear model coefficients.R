# After much testing, a simple linear model was deemed best
# This script fits the model and reports the slope and intercept
# Nathan Bonham

rm(list=ls()); gc()

df_sv=read.csv(file.path('outdata', 'Gaged and Natural Flow - St Vrain Lyons.csv'))[,-1]

df_crb=CoRiverNF::cyAnnTot

# convert to dataframe with Year column
df_crb=data.frame(
  
  Year=floor(as.numeric(zoo::index(df_crb))),
  
  zoo::coredata(df_crb)
  
)

df_complete=df_crb %>% left_join(df_sv %>% select(Year, Natural)%>%rename(StVrain=Natural), by='Year')

df_complete=df_complete %>% filter(!is.na(StVrain))

md=lm(StVrain ~ GlenwoodSprings, data = df_complete)

print(summary(md))


print(md$coefficients)
