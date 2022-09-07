library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(hms)
library(sf)

#Day
Agg=function(dta,Range,dtaOrg,CTListTrt,Unit){
  dtaDay = dta %>%
    filter(Hour%in%Range) %>% 
    group_by(week,Date,OD_Pair,GeoTrt,Post,
             PU_BdN,PU_BdNW,PU_BdSW,PU_BdS,
             DO_BdN,DO_BdNW,DO_BdSW,DO_BdS) %>% 
    summarise(across(matches("Count|Surge"),sum,na.rm=T),
              across(!matches("Count|Surge"),mean,na.rm=T)) %>% 
    select_if(~sum(!is.na(.)) > 0)
  dtaDay = left_join(dtaOrg,dtaDay,by=c("Date","OD_Pair")) %>%
    mutate(week=week(Date),
           PairDist=as.numeric(PairDist)) %>% 
    mutate(Count=ifelse(is.na(Count)==T,0,Count),
           GeoTrt=ifelse(PU_Tract%in%CTListTrt$Tract|DO_Tract%in%CTListTrt$Tract,1,0),
           Post=ifelse(Date>ymd_hms("2020/01/06 00:00:00"),1,0))
  return(dtaDay)
}

#Week
AggWeek=function(dtaDay){
  dtaWeek = dtaDay %>% 
    mutate(Year=year(Date)) %>% 
    group_by(Year,week,OD_Pair,GeoTrt,Post,
             PU_BdN,PU_BdNW,PU_BdSW,PU_BdS,
             DO_BdN,DO_BdNW,DO_BdSW,DO_BdS) %>% 
    summarise(across(matches("Count|Surge"),sum,na.rm=T),
              across(!matches("Count|Surge"),mean,na.rm=T)) %>% 
    select_if(~sum(!is.na(.)) > 0)
  return(dtaWeek)
}