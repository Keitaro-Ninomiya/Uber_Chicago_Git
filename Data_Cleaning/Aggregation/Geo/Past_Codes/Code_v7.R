#Editted at April 4th 2022
#Data is seperated for counting daily total for trips within peak-time and outside of peak time.
#This is unavoidable since DID for Geo cutoff is done at the daily level. 
#Difference from Previous Version: Using functions for merging.
source("/home/keitaro2/Uber_Chicago/Data_Cleaning/Aggregation/Geo/Aggregate/Code.R")
source("/home/keitaro2/Uber_Chicago/Data_Cleaning/Aggregation/Geo/Slots/Code.R")

#### Download Data ####
dta = read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/dtaDist.csv")
CTList = read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/Boundaries - Census Tracts - 2010.geojson")
zone = read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/downtown_zone.shp")
zone = zone %>% mutate(zone=ifelse(FID==0,1,0))
PairList=read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/PairList.csv")

#Eliminate overlapping regions
CTList = CTList %>% filter(!geoid10%in%c(17031839100,17031842200)) %>%
		st_join(zone)
CTListTrt = CTList %>% filter(zone==1)
PairList = PairList %>% filter(!PU_Tract%in%c("842200","841900"),
                               !DO_Tract%in%c("842200","841900"))

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
         Surge=case_when(AdjAdd>1~1,TRUE~0)) %>% 
  mutate(OD_Pair=paste(PU_tractce10,DO_tractce10,sep="-"),
         Count=1)

#Merge ####
#Day+Week
# Within of peak-time
path = "/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/Day/"
#for (i in 1:6) {
#  dtaDay=Agg(dta,c(6,21),dtaOrg(i),CTListTrt)
#  dtaWeek=AggWeek(dtaDay)
#  write.csv(dtaDay,paste(path,"dtaDay/",as.character(i),".csv",sep=""))
#  write.csv(dtaWeek,paste(path,"dtaWeek/",as.character(i),".csv",sep=""))
#}
#
## Outside of peak-time
#dtaDayOut=Agg(dta,c(0:5,22,23),dtaOrg(2),CTListTrt)
#write.csv(dtaDayOut,paste(path,"dtaDay_Out.csv",sep=""))
#
#
###--------------------##
dtaDay1=Agg(dta,c(6,21),dtaOrg(1),CTListTrt)
write.csv(dtaDay1,paste(path,"dtaDay",as.character(1),".csv",sep=""))
dtaDay3=Agg(dta,c(6,21),dtaOrg(3),CTListTrt)
write.csv(dtaDay3,paste(path,"dtaDay",as.character(3),".csv",sep=""))
dtaDay5=Agg(dta,c(6,21),dtaOrg(5),CTListTrt)
write.csv(dtaDay5,paste(path,"dtaDay",as.character(5),".csv",sep=""))

dtaWeek1=AggWeek(dtaDay1)
write.csv(dtaWeek1,paste(path,"dtaWeek1",as.character(1),".csv",sep=""))
dtaWeek3=AggWeek(dtaDay3)
write.csv(dtaWeek3,paste(path,"dtaWeek3",as.character(3),".csv",sep=""))
dtaWeek5=AggWeek(dtaDay5)
write.csv(dtaWeek5,paste(path,"dtaWeek5",as.character(5),".csv",sep=""))