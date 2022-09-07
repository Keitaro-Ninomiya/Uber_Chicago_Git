library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(hms)
library(sf)

#### Download Data ####
dta = read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/dtaDist.csv")
CTList = read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/Boundaries - Census Tracts - 2010.geojson")
border = read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/border.shp")
zone = read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/downtown_zone.shp")
zone = zone %>% mutate(zone=ifelse(FID==0,1,0))
#CTList = read_sf("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Uber_Chicago/Raw_Data/Data Geo/Boundaries - Census Tracts - 2010.geojson")#
#zone=read_sf("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Uber_Chicago/Raw_Data/Data Geo/downtown_zone.shp")
#zone = zone %>% mutate(zone=ifelse(FID==0,1,0))

#### Clean Trip level data ####
dta = dta %>% 
  filter(is.na(PU_Dist)==F & is.na(DO_Dist)==F) %>% 
  filter(Shared.Trip.Authorized=="false") %>%
  filter(!PU_name10%in%c("8382","814.02","3301","7706.02","9800")&
           !DO_name10%in%c("814.02","3301","7706.02","9800")) %>% 
  filter(Additional.Charges!=0) %>% 
  filter(Fare!=0)

dta = dta %>% 
  mutate(Trip.Start.Timestamp=ymd_hms(Trip.Start.Timestamp),
         Trip.End.Timestamp=ymd_hms(Trip.End.Timestamp)) %>% 
  rename(Start_Time=Trip.Start.Timestamp,
         End_Time=Trip.End.Timestamp) %>% 
  mutate(Hour=hour(Start_Time),
         Date=as.Date(Start_Time),
         week=week(Start_Time),
         GeoTrt = ifelse(PU_zone=="Commute_zone"|DO_zone=="Commute_zone",1,0)) %>% 
  mutate(GeoTrt = ifelse(is.na(GeoTrt)==F,1,0),
         Post =ifelse(Start_Time>ymd_hms("2020/01/06 00:00:00"),1,0))

##### Create OD pairs ####
##Trip Level Data
dta = dta %>% mutate(OD_Pair=paste(PU_tractce10,DO_tractce10,sep="-"),
                     exist=1)

## Distance between CT and border
CTList = CTList %>% st_centroid() %>% 
  mutate(Dist=as.numeric(st_distance(CTList,border)))
CTList = st_join(CTList,zone) %>% 
  rename(Tract=tractce10) %>% 
  mutate(zone=ifelse(is.na(FID)==F,1,0)) %>% 
  filter(!Tract %in% c("838200","081402","330100","7706.02","980000", #NavyPier+Ohare+etc
                       "081403","081300","081202","842300","081402","081401","081201","081100","320100")) #Central areas
CTListTrt = CTList %>% filter(zone==1)

## OD Pair List 
PairList = CTList %>% 
  select(Tract) %>% 
  as_data_frame() %>% 
  mutate(geometry=NULL,Tract2=Tract) %>% 
  expand(Tract,Tract2) %>% 
  mutate(Tract = as.character(Tract),
         Tract2 = as.character(Tract2)) %>% 
  mutate(OD_Pair=paste(Tract,Tract2,sep="-")) %>% 
  rename(PU_Tract=Tract,DO_Tract=Tract2)

PairList = left_join(PairList,
                     CTList %>% 
                       rename(PU_Dist=Dist,PU_Tract=Tract) %>% 
                       select(PU_Tract,PU_Dist) %>% 
                       as_data_frame() %>% 
                       mutate(geometry=NULL),
                     by="PU_Tract")
PairList = left_join(PairList,
                     CTList %>% 
                       rename(DO_Dist=Dist,DO_Tract=Tract) %>% 
                       select(DO_Tract,DO_Dist) %>% 
                       as_data_frame() %>% 
                       mutate(geometry=NULL),
                     by="DO_Tract")

PU_PairList = 
  left_join(PairList %>% select(PU_Tract) %>% rename(Tract=PU_Tract),
            CTList %>% rename(PU_Dist=Dist) %>% select(Tract,geometry,PU_Dist),
            by="Tract") %>%
  rename(PU_Tract=Tract) %>% 
  st_as_sf() %>% 
  st_cast("POINT")

DO_PairList = 
  left_join(PairList %>% select(DO_Tract) %>% rename(Tract=DO_Tract),
            CTList %>% select(Tract,geometry),
            by="Tract") %>%
  rename(DO_Tract=Tract) %>% 
  st_as_sf()%>% 
  st_cast("POINT")

PairList = PairList %>% 
  mutate(PairDist = st_distance(PU_PairList,DO_PairList,by_element = TRUE))

#Int ####
##Aggregate by 15 min####
Dates = seq(ISOdatetime(2019,12,01,0,0,0,tz="UTC"),
             ISOdatetime(2020,01,31,0,0,0,tz="UTC"),
             by=(60*15)) %>% as.data.frame() 
colnames(Dates)[1] = "Dates"
Dates = Dates %>% filter(hour(Dates)%in%c(5,6,21,22))

dtaOrg = expand.grid(Dates$Dates,PairList$OD_Pair)
dtaOrg = dtaOrg %>% rename(Time=Var1,OD_Pair=Var2)
dtaOrg = left_join(dtaOrg,PairList,by="OD_Pair") %>% mutate(week=week(Time),
                                                            Hour=hour(Time),
                                                            Minute=minute(Time),
                                                            Date=as.Date(Time))
dtaOrg = dtaOrg %>% mutate(IntTrt=ifelse(Hour%in%c(6,21),1,0))

dta = dta %>% mutate(Hour_St=hour(Start_Time),
                     Hour_End=hour(End_Time),
                     Minute_St=minute(Start_Time),
                     Minute_End=minute(End_Time),
                     Date=as.Date(Start_Time),
                     week(Start_Time)
                     )

#First_Cutoff
dtaOrgSt = dtaOrg %>%
  group_by(OD_Pair,Hour,Minute,week,PairDist) %>% 
  summarise()
dtaOrgSt = dtaOrgSt %>% filter(!week%in%c(48,49))

dta = dta %>% filter(Date>ymd("2019/11/01"))
dta15_St = dta %>% 
  filter(Hour_St%in%c(21,22),!week%in%c(48,49)) %>% 
  group_by(week,Hour_St,Minute_St,OD_Pair,GeoTrt) %>% 
  summarise(Count = sum(exist,na.rm = T),
            Trip.Seconds = mean(Trip.Seconds,na.rm = T),
            Trip.Miles = mean(Trip.Miles,na.rm = T),
            Fare = mean(Fare,na.rm = T),
            Tip = mean(Tip,na.rm = T),
            Additional.Charges = mean(Additional.Charges,na.rm = T),
            Trip.Total = mean(Trip.Total,na.rm = T)
            )

dta15_End = dta %>% 
  filter(Hour_End%in%c(5,6),!week%in%c(48,49)) %>% 
  group_by(week,Hour_End,Minute_End,OD_Pair,GeoTrt) %>% 
  summarise(Count = sum(exist,na.rm = T),
            Trip.Seconds = mean(Trip.Seconds,na.rm = T),
            Trip.Miles = mean(Trip.Miles,na.rm = T),
            Fare = mean(Fare,na.rm = T),
            Tip = mean(Tip,na.rm = T),
            Additional.Charges = mean(Additional.Charges,na.rm = T),
            Trip.Total = mean(Trip.Total,na.rm = T)
            )

dta15=rbind(dta15_St %>% rename(Hour=Hour_St,Minute=Minute_St),dta15_End %>% rename(Hour=Hour_End,Minute=Minute_End))

dta15 = left_join(dtaOrgSt,dta15,by=c('week',"Hour","Minute","OD_Pair"))
dta15 = dta15 %>% 
  mutate(PairDist=as.numeric(PairDist), 
         Count=ifelse(is.na(Count)==T,0,Count),
         Date=ifelse(week>40,as.Date(week*7,origin="2019/01/01"),as.Date(week*7,origin="2020/01/01")),
         Post=ifelse(Date>ymd("2020/01/06"),1,0),
         GeoTrt=ifelse(PU_Tract%in%CTListTrt$Tract|DO_Tract%in%CTListTrt$Tract,1,0)) %>% 
  mutate(IntTrt=ifelse(Hour%in%c(6,21),1,0))

write.csv(dta15,"/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/dtaInt15Week.csv")
