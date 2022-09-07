#Editted at April 4th 2022
#Data is seperated for counting daily total for trips within peak-time and outside of peak time.
#This is unavoidable since DID for Geo cutoff is done at the daily level. 
#Difference from Previous Version: Using refined border.

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(hms)
library(sf)

#### Download Data ####
dta = read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/dtaDist.csv")
CTList = read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/Boundaries - Census Tracts - 2010.geojson")
zone = read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/downtown_zone.shp")
zone = zone %>% mutate(zone=ifelse(FID==0,1,0))

PairList=read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/PairList.csv")
#CTList = read_sf("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Uber_Chicago/Raw_Data/Data Geo/Boundaries - Census Tracts - 2010.geojson")#
#zone=read_sf("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Uber_Chicago/Raw_Data/Data Geo/downtown_zone.shp")
#zone = zone %>% mutate(zone=ifelse(FID==0,1,0))

#Eliminate overlapping regions
CTList = CTList %>% filter(!geoid10%in%c(17031839100,17031842200))
CTListTrt = CTList %>% filter(zone==1)

PairList = PairList %>% filter(!PU_Tract%in%c("842200","841900"),
                               !DO_Tract%in%c("842200","841900"))

#### Clean Trip level data ####
List=c("8382", #Hospital
       "814.02",#Navy_Pier 
       "3301", #McCormich_Place
       "7706.02", #O'hare
       "9800")#O'hare
dta = dta %>% 
  filter(is.na(PU_Dist)==F & is.na(DO_Dist)==F) %>% 
  filter(Additional.Charges!=0,Fare!=0) %>% 
  filter(Shared.Trip.Authorized=="false") %>%
  filter(!PU_name10 %in% List&
           !DO_name10 %in% List) %>% 
  filter(!PU_Tract%in%c("842200","841900"),
         !DO_Tract%in%c("842200","841900"))

dta = dta %>% 
  rename(Start_Time=Trip.Start.Timestamp,
         End_Time=Trip.End.Timestamp) %>% 
  mutate(Start_Time=ymd_hms(Start_Time),
         End_Time=ymd_hms(End_Time),
         Hour = hour(Start_Time),
         Date = as.Date(Start_Time),
         week = week(Start_Time),
         GeoTrt = case_when(PU_zone=="Commute_zone"|DO_zone=="Commute_zone"~1,
                            TRUE~0),
         Post   = ifelse(Start_Time>ymd_hms("2020/01/06 00:00:00"),1,0)) %>% 
  mutate(AdjAdd=case_when(Post==1&GeoTrt==1~Additional.Charges-3-0.02-0.1,
                          Post==1&GeoTrt==0~Additional.Charges-1.25-0.02-0.1,
                          TRUE~Additional.Charges-0.72),
         Surge=case_when(AdjAdd>1~1,
                         TRUE~0))

##Trip Level Data
dta = dta %>% mutate(OD_Pair=paste(PU_tractce10,DO_tractce10,sep="-"),
                     exist=1)

#Geo ####
Dates18 = seq(ISOdatetime(2018,12,01,0,0,0,tz="UTC"),
              ISOdatetime(2019,01,31,0,0,0,tz="UTC"),
              by=(60*60*24)) %>% as.data.frame() 
Dates19 = seq(ISOdatetime(2019,12,01,0,0,0,tz="UTC"),
              ISOdatetime(2020,01,31,0,0,0,tz="UTC"),
              by=(60*60*24)) %>% as.data.frame() 
Dates = rbind(Dates18,Dates19)
colnames(Dates)[1] = "Date"

dtaOrg = expand.grid(Dates$Date,PairList$OD_Pair) %>% 
  rename(Date=Var1,OD_Pair=Var2)
dtaOrg =left_join(dtaOrg,PairList,by="OD_Pair") %>% 
  mutate(Date=as.Date(Date))

#Within Peak Time ####
dtaDay1 = dta %>%
  filter(!Hour%in%c(0,1,2,3,4,5,22,23)) %>% 
  group_by(week,Date,OD_Pair,GeoTrt,Post,
           PU_BdN,PU_BdNW,PU_BdSW,PU_BdS,
           DO_BdN,DO_BdNW,DO_BdSW,DO_BdS) %>% 
  summarise(Count = sum(exist,na.rm = T),
            Trip.Seconds = mean(Trip.Seconds,na.rm = T),
            Trip.Miles = mean(Trip.Miles,na.rm = T),
            Fare = mean(Fare,na.rm = T),
            Tip = mean(Tip,na.rm = T),
            Additional.Charges = mean(Additional.Charges,na.rm = T),
            Trip.Total = mean(Trip.Total,na.rm = T),
            Surge = sum(Surge))
dtaDay1 = left_join(dtaOrg,dtaDay1,by=c("Date","OD_Pair")) %>%
  mutate(week=week(Date),
         PairDist=as.numeric(PairDist)) %>% 
  mutate(Count=ifelse(is.na(Count)==T,0,Count),
         GeoTrt=ifelse(PU_Tract%in%CTListTrt$Tract|DO_Tract%in%CTListTrt$Tract,1,0),
         Post=ifelse(Date>ymd_hms("2020/01/06 00:00:00"),1,0),
  )

write.csv(dtaDay1,"/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/dtaDay1.csv")

# Outside of peak-time
dtaDay2 = dta %>%
  filter(Hour%in%c(0,1,2,3,4,5,22,23)) %>% 
  group_by(week,Date,Hour,OD_Pair,GeoTrt,Post,
           PU_BdN,PU_BdNW,PU_BdSW,PU_BdS,
           DO_BdN,DO_BdNW,DO_BdSW,DO_BdS) %>% 
  summarise(Count = sum(exist,na.rm = T),
            Trip.Seconds = mean(Trip.Seconds,na.rm = T),
            Trip.Miles = mean(Trip.Miles,na.rm = T),
            Fare = mean(Fare,na.rm = T),
            Tip = mean(Tip,na.rm = T),
            Additional.Charges = mean(Additional.Charges,na.rm = T),
            Trip.Total = mean(Trip.Total,na.rm = T)
  )
dtaDay2 = left_join(dtaOrg,dtaDay2,by=c("Date","OD_Pair")) %>%
  mutate(week=week(Date),
         PairDist=as.numeric(PairDist)) %>% 
  mutate(Count=ifelse(is.na(Count)==T,0,Count),
         GeoTrt=ifelse(PU_Tract%in%CTListTrt$Tract|DO_Tract%in%CTListTrt$Tract,1,0),
         Post=ifelse(Date>ymd_hms("2020/01/06 00:00:00"),1,0))

write.csv(dtaDay2,"/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/dtaDay2.csv")

