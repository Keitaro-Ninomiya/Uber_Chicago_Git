#Editted at April 4th 2022
#Data is seperated for counting daily total for trips within peak-time and outside of peak time.
#This is unavoidable since DID for Geo cutoff is done at the daily level. 
#Difference from Previous Version: Using functions for merging.
source("/home/keitaro2/Uber_Chicago/Data_Cleaning/Aggregation/Geo/Aggregate/Code_v3.R")
source("/home/keitaro2/Uber_Chicago/Data_Cleaning/Aggregation/Geo/Slots/Code_v3.R")
source("/home/keitaro2/Uber_Chicago/Data_Cleaning/Aggregation/Geo/RefineOD/Code.R")

#### Download Data ####
dta = read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/dtaDist.csv")
CTList = read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/Boundaries - Census Tracts - 2010.geojson")
zone = read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/downtown_zone.shp")
zone = zone %>% mutate(zone=ifelse(FID==0,1,0))
CommuList = read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/Modified/CommuList_Outside.geojson") %>% 
  rename(comm=area_num_1)

PairListCT=read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/PairList.csv")
PairListCO=read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/PairList_Expand.csv")

#Eliminate overlapping regions
CTList = CTList %>% filter(!tractce10%in%c(839100,842200)) %>%
  st_join(zone)
CTListTrt = CTList %>% filter(zone==1) %>% rename(Tract=tractce10)
PairListCO = PairListCO %>% filter(!PU%in%c("842200","841900"),
                               !DO%in%c("842200","841900"))
PairListCT = PairListCT %>% filter(!PU_Tract %in% c("842200","841900"),
                                   !DO_Tract %in% c("842200","841900"))

List=c("8382", #Hospital
       "814.02",#Navy_Pier 
       "3301", #McCormich_Place
       "7706.02", #O'hare
       "9800")#O'hare

#### Clean Trip level data ####
dta = dta %>% 
  filter(is.na(PU_Dist)==F & is.na(DO_Dist)==F,Additional.Charges!=0,Fare!=0,
         Shared.Trip.Authorized=="false") %>%
  filter(!PU_name10 %in% List,!DO_name10 %in% List,
         !PU_name10%in%c("8422","8419"),!DO_name10%in%c("8422","8419"))
dta = dta %>% 
  rename(Start_Time=Trip.Start.Timestamp,End_Time=Trip.End.Timestamp) %>% 
  mutate(Start_Time=ymd_hms(Start_Time),End_Time=ymd_hms(End_Time),
         Hour = hour(Start_Time),Date = as.Date(Start_Time),week = week(Start_Time),
         GeoTrt = case_when(PU_zone=="Commute_zone"|DO_zone=="Commute_zone"~1,TRUE~0),
         Post   = ifelse(Start_Time>ymd_hms("2020/01/06 00:00:00"),1,0)) %>% 
  mutate(AdjAdd=case_when(Post==1&GeoTrt==1~Additional.Charges-3-0.02-0.1,
                          Post==1&GeoTrt==0~Additional.Charges-1.25-0.02-0.1,
                          TRUE~Additional.Charges-0.72),
         Surge=case_when(AdjAdd>1~1,TRUE~0),
         Count=1)

#Update OD Pair
dta = dta %>% 
  rename(PU_comm=Pickup.Comunity.Area,DO_comm=Dropoff.Comunity.Area,
         PU_Tract=Pickup.Census.Tract,DO_Tract=Dropoff.Census.Tract) %>% 
  mutate(PU_Tract=as.character(PU_Tract),DO_Tract=as.character(DO_Tract),
         PU_Tract=str_remove(PU_Tract,"17031"),DO_Tract=str_remove(DO_Tract,"17031"),
         PU_comm=as.character(PU_comm),DO_comm=as.character(DO_comm),
         PU=case_when(PU_comm%in%CommuList$comm~PU_comm,
                      TRUE~PU_Tract),
         DO=case_when(DO_comm%in%CommuList$comm~DO_comm,
                      TRUE~DO_Tract),
         OD_PairCO=paste(PU,DO,sep="-"),
         OD_PairCT=paste(PU_tractce10,DO_tractce10,sep="-"))

#Merge ####
#Community Area level

#Day+Week
# Within of peak-time
path = "/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/Day/"
for (i in 1:3) {
  dtaDayCT =Agg(dta,c(6:21),i,CTListTrt,"Census_Tract","Day")
  dtaWeekCT=Agg(dta,c(6:21),i,CTListTrt,"Census_Tract","Week")
  dtaDayCO =Agg(dta,c(6:21),i,CTListTrt,"Community_Area","Day")
  dtaWeekCO=Agg(dta,c(6:21),i,CTListTrt,"Community_Area","Week")
  write.csv(dtaDayCT,paste(path,"dtaWeekCT",as.character(i),".csv",sep=""))
  write.csv(dtaWeekCT,paste(path,"dtaWeekCT",as.character(i),".csv",sep=""))
  write.csv(dtaDayCO,paste(path,"dtaWeekCO",as.character(i),".csv",sep=""))
  write.csv(dtaWeekCO,paste(path,"dtaWeekCO",as.character(i),".csv",sep=""))
  }
#
## Outside of peak-time
#dtaDayOut=Agg(dta,c(0:5,22,23),dtaOrg(2),CTListTrt)
#write.csv(dtaDayOut,paste(path,"dtaDay_Out.csv",sep=""))
#
#
###--------------------##
dtaDay1=Agg(dta,c(6:21),1,CTListTrt,"Census_Tract","Day")
write.csv(dtaDay1,paste(path,"dtaDayCT",as.character(1),".csv",sep=""))


dtaWeek1=Agg(dta,c(6:21),1,CTListTrt,"Community_Area","Week")
write.csv(dtaWeek1,paste(path,"dtaWeekCO",as.character(1),".csv",sep=""))
dtaWeek3=Agg(dta,c(6:21),3,CTListTrt,"Community_Area","Week")
write.csv(dtaWeek3,paste(path,"dtaWeekCO",as.character(3),".csv",sep=""))
dtaWeek5=Agg(dta,c(6:21),5,CTListTrt,"Community_Area","Week")
write.csv(dtaWeek5,paste(path,"dtaWeekCO",as.character(5),".csv",sep=""))

