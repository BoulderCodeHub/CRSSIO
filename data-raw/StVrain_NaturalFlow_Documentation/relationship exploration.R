library(CoRiverNF) # for historical CRB natural flow
library(zoo)
library(dplyr)
library(ggplot2)

rm(list=ls()); gc()

SV_histNF=read.csv(file.path('outdata', 'Gaged and Natural Flow - St Vrain Lyons.csv'))[,-1]

### test correlation with total natural flow ######
###################################################

CRB_NF=CoRiverNF::cyAnnTot

# convert to dataframe with Year column
CRB_NF=data.frame(
  
  Year=floor(as.numeric(zoo::index(CRB_NF))),
  
  zoo::coredata(CRB_NF)
  
)

combined=CRB_NF %>% left_join(SV_histNF %>% select(Year, Natural)%>%rename(StVrain=Natural), by='Year')

combined=combined %>% filter(!is.na(StVrain)) # removes 2013 (missing data)

CRB_gages=setdiff(names(CRB_NF), 'Year')

g=CRB_gages[1]

save_cor=c()
for(g in CRB_gages){
  
  corr=cor(combined[[g]], combined$StVrain)
  
  ggplot(combined, aes(x=.data[[g]], y=StVrain))+
    geom_point()+
    geom_smooth(method = 'lm', se=F)+
    annotate('text', x=1.2*min(combined[[g]]), y=.95*max(combined$StVrain),
             label=paste('corr:', round(corr,2)))+
    theme_bw()
    
  save_cor=c(save_cor, corr)
  
  ggsave(file.path('figures', 'correlation', 'total NF', paste0(g, '.jpg')), create.dir = T, width = 5, height = 5)
  
}

corr_df=data.frame(
  
  gage=CRB_gages,
  correlation=save_cor
  
)

print(corr_df %>% slice_max(correlation, n=10))


### test correlation with intervening natural flow ######
#######################################################

CRB_NFint=CoRiverNF::cyAnnInt

# convert to dataframe with Year column
CRB_NFint=data.frame(
  
  Year=floor(as.numeric(zoo::index(CRB_NFint))),
  
  zoo::coredata(CRB_NFint)
  
)

combined=CRB_NFint %>% left_join(SV_histNF %>% select(Year, Natural)%>%rename(StVrain=Natural), by='Year')

combined=combined %>% filter(!is.na(StVrain)) # removes 2013 (missing data)

CRB_gages=setdiff(names(CRB_NFint), 'Year')

g=CRB_gages[1]

save_cor=c()
for(g in CRB_gages){
  
  corr=cor(combined[[g]], combined$StVrain)
  
  ggplot(combined, aes(x=.data[[g]], y=StVrain))+
    geom_point()+
    geom_smooth(method = 'lm', se=F)+
    annotate('text', x=1.2*min(combined[[g]]), y=.95*max(combined$StVrain),
             label=paste('corr:', round(corr,2)))+
    theme_bw()
  
  save_cor=c(save_cor, corr)
  
  ggsave(file.path('figures', 'correlation', 'int NF', paste0(g, '.jpg')), create.dir = T, width = 5, height = 5)
  
}

corr_int_df=data.frame(
  
  gage=CRB_gages,
  correlation=save_cor
  
)

print(corr_int_df %>% slice_max(correlation, n=5))

# St Vrain highly correlated with GlenwoodSprings (both total and intervening)
# the relationship is close to linear, but at low flows Glenwood higher than StVrain, and at high flows St Vrain higher than GlenwoodSprings

### test correlation with LOG total natural flow ######
###################################################

CRB_NF=CoRiverNF::cyAnnTot

# convert to dataframe with Year column
CRB_NF=data.frame(
  
  Year=floor(as.numeric(zoo::index(CRB_NF))),
  
  zoo::coredata(CRB_NF)
  
)

combined=CRB_NF %>% left_join(SV_histNF %>% select(Year, Natural)%>%rename(StVrain=Natural), by='Year')

combined=combined %>% filter(!is.na(StVrain)) # removes 2013 (missing data)

CRB_gages=setdiff(names(CRB_NF), 'Year')

g=CRB_gages[1]

save_cor=c()
for(g in CRB_gages){
  
  corr=cor(log(combined[[g]]),log( combined$StVrain) )
  
  ggplot(combined, aes(x=log(.data[[g]]), y=log(StVrain)))+
    geom_point()+
    geom_smooth(method = 'lm', se=F)+
    annotate('text', x=1.01*min(log(combined[[g]])), y=max(log(combined$StVrain)),
             label=paste('corr:', round(corr,2)))+
    theme_bw()
  
  save_cor=c(save_cor, corr)
  
  ggsave(file.path('figures', 'correlation', 'log total NF', paste0(g, '.jpg')), create.dir = T, width = 5, height = 5)
  
}

corr_log_df=data.frame(
  
  gage=CRB_gages,
  correlation=save_cor
  
)

print(corr_log_df %>% slice_max(correlation, n=5))


#### Conclusions so far:

# StVrain highly correlated with Glenwood Springs, both total natural flow and intervening
# But, it seems StVrain more variable (highs are higher, lows lower) than Glenwood.
# I want to explore if any gages are highly correlated with the residuals
# StVrain ~ Glenwood


md= lm(StVrain ~ GlenwoodSprings, data = combined)

combined$residuals=md$residuals

save_cor=c()
for(g in CRB_gages){
  
  corr=cor(combined[[g]],combined$residuals)
  
  ggplot(combined, aes(x=.data[[g]], y=residuals))+
    geom_point()+
    geom_smooth(method = 'lm', se=F)+
    annotate('text', x=1.2*min(combined[[g]]), y=.95*max(combined$StVrain),
             label=paste('corr:', round(corr,2)))+
    theme_bw()
  
  save_cor=c(save_cor, corr)
  
  ggsave(file.path('figures', 'correlation', 'StVrain Glenwood Residuals', paste0(g, '.jpg')), create.dir = T, width = 5, height = 5)
  
}

corr_resid_df=data.frame(
  
  gage=CRB_gages,
  correlation=save_cor
  
)

print(corr_resid_df %>% slice_max(abs(correlation), n=5))

corr_resid_df=corr_resid_df %>% arrange(desc(abs(correlation)))
