#Created April 21th
#Update: Added flexibility to use Community tract as OD pair unit.

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(hms)
library(sf)

#Day
Agg=function(dta,Range,Month,CTListTrt,GeoUnit,TimeUnit){
  if(GeoUnit=="Census_Tract"&TimeUnit=="Day"){
    Org = dtaOrg(Month,GeoUnit,TimeUnit)
    dtaDay = dta %>%
      filter(Hour%in%Range) %>% 
      group_by(Date,OD_PairCT,GeoTrt,Post,
               PU_BdN,PU_BdNW,PU_BdSW,PU_BdS,
               DO_BdN,DO_BdNW,DO_BdSW,DO_BdS) %>% 
      summarise(across(matches("Count|Surge"),sum,na.rm=T),
                across(!matches("Count|Surge"),mean,na.rm=T)) %>% 
      select_if(~sum(!is.na(.)) > 0)
    
    dtaWeek = left_join(Org,dtaDay,by=c("Week","OD_PairCT")) %>%
      mutate(Count=ifelse(is.na(Count)==T,0,Count),
             GeoTrt=ifelse(PU_Tract%in%CTListTrt$Tract|DO_Tract%in%CTListTrt$Tract,1,0),
             Post=ifelse(Date>ymd_hms("2020/01/06 00:00:00"),1,0))
    return(dtaDay)
  }
  
  if(GeoUnit=="Community_Area"&TimeUnit=="Day"){
    Org = dtaOrg(Month,GeoUnit,TimeUnit)
    dtaDay = dta %>%
      filter(Hour%in%Range) %>% 
      group_by(Date,OD_PairCO,GeoTrt,Post,
               PU_BdN,PU_BdNW,PU_BdSW,PU_BdS,
               DO_BdN,DO_BdNW,DO_BdSW,DO_BdS) %>% 
      summarise(across(matches("Count|Surge"),sum,na.rm=T),
                across(!matches("Count|Surge"),mean,na.rm=T)) %>% 
      select_if(~sum(!is.na(.)) > 0) 
    
    dtaDay = left_join(Org,dtaDay,by=c("Date","OD_PairCO")) %>%
      mutate(week=week(Date)) %>% 
      mutate(Count=ifelse(is.na(Count)==T,0,Count),
             GeoTrt=ifelse(PU%in%CTListTrt$Tract|DO%in%CTListTrt$Tract,1,0),
             Post=ifelse(Date>ymd_hms("2020/01/06 00:00:00"),1,0)) %>% 
      rename(OD_Pair=OD_PairCO)
    return(dtaDay)
  }
  
  if(GeoUnit=="Census_Tract"&TimeUnit=="Week"){
    Org = dtaOrg(Month,GeoUnit,TimeUnit)
    dtaWeek = dta %>%
      mutate(Week=floor_date(Date,unit="weeks")) %>% 
      filter(Hour%in%Range) %>% 
      group_by(Week,OD_PairCT,GeoTrt,Post,
               PU_BdN,PU_BdNW,PU_BdSW,PU_BdS,
               DO_BdN,DO_BdNW,DO_BdSW,DO_BdS) %>% 
      summarise(across(matches("Count|Surge"),sum,na.rm=T),
                across(!matches("Count|Surge"),mean,na.rm=T)) %>% 
      select_if(~sum(!is.na(.)) > 0)
    
    dtaWeek = left_join(Org,dtaDay,by=c("Week","OD_PairCT")) %>% 
      mutate(Count=ifelse(is.na(Count)==T,0,Count),
             GeoTrt=ifelse(PU_Tract%in%CTListTrt$Tract|DO_Tract%in%CTListTrt$Tract,1,0),
             Post=ifelse(Week>ymd_hms("2020/01/06 00:00:00"),1,0))
    return(dtaWeek)
  }
  
  if(GeoUnit=="Community_Area"&TimeUnit=="Week"){
    Org = dtaOrg(Month,GeoUnit,TimeUnit)
    dtaWeek = dta %>%
      mutate(Week=floor_date(Date,unit="weeks")) %>% 
      filter(Hour%in%Range) %>% 
      group_by(Week,OD_PairCO,GeoTrt,Post,
               PU_BdN,PU_BdNW,PU_BdSW,PU_BdS,
               DO_BdN,DO_BdNW,DO_BdSW,DO_BdS) %>% 
      summarise(across(matches("Count|Surge"),sum,na.rm=T),
                across(!matches("Count|Surge"),mean,na.rm=T)) %>% 
      select_if(~sum(!is.na(.)) > 0) 
    
    dtaWeek = left_join(Org,dtaWeek,by=c("Week","OD_PairCO")) %>%
      mutate(Count=ifelse(is.na(Count)==T,0,Count),
             GeoTrt=ifelse(PU%in%CTListTrt$Tract|DO%in%CTListTrt$Tract,1,0),
             Post=ifelse(Week>ymd_hms("2020/01/06 00:00:00"),1,0)) %>% 
      rename(OD_Pair=OD_PairCO)
    return(dtaWeek)
  }
}

#dtaDay_temp = left_join(dtaOrg,dtaDay,by=c("Date","OD_PairCO")) %>%
#  mutate(week=week(Date)) %>% 
#  mutate(Count=ifelse(is.na(Count)==T,0,Count),
#         GeoTrt=ifelse(PU%in%CTListTrt$Tract|DO%in%CTListTrt$Tract,1,0),
#         Post=ifelse(Date>ymd_hms("2020/01/06 00:00:00"),1,0)) %>% 
#  rename(OD_Pair=OD_PairCO)