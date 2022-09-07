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
dta = dta %>% filter(is.na(PU_Dist)==F & is.na(DO_Dist)==F)
dta = dta %>% filter(Shared.Trip.Authorized=="false")
dta = dta %>% filter(!PU_name10%in%c("8382","814.02","3301","7706.02","9800")&
                       !DO_name10%in%c("814.02","3301","7706.02","9800"))
dta = dta %>% filter(Additional.Charges!=0)
dta = dta %>% filter(Fare!=0)

dta = dta %>% mutate(Trip.Start.Timestamp=ymd_hms(Trip.Start.Timestamp),
                     Trip.End.Timestamp=ymd_hms(Trip.End.Timestamp)) %>% rename(Start_Time=Trip.Start.Timestamp,
                                                                                End_Time=Trip.End.Timestamp)
dta = dta %>% mutate(week=week(Start_Time),
                     GeoTrt = ifelse(PU_zone=="Commute_zone"|DO_zone=="Commute_zone",1,0)) %>% 
  mutate(GeoTrt=ifelse(is.na(GeoTrt)==F,1,0))%>% 
  filter(week%in%c(53,2))

##### Create OD pairs ####
##Trip Level Data
dta = dta %>% mutate(OD_Pair=paste(PU_tractce10,DO_tractce10,sep="-"))
dta = dta %>% mutate(exist=1)

## Distance between CT and border
CTList = CTList %>% st_centroid()
CTList = CTList %>% mutate(Dist=as.numeric(st_distance(CTList,border)))
CTList = st_join(CTList,zone) %>% rename(Tract=tractce10)
CTList = CTList %>% mutate(zone=ifelse(is.na(FID)==F,1,0))
CTList = CTList %>% filter(!Tract %in% c("838200","081402","330100","7706.02","980000", #NavyPier+Ohare+etc
                                         "081403","081300","081202","842300","081402","081401","081201","081100","320100")) #Central areas
CTListTrt = CTList %>% filter(zone==1)

## OD Pair List 
PairList = CTList %>% 
  select(Tract) %>% 
  as_data_frame() %>% 
  mutate(geometry=NULL)
PairList = PairList %>% mutate(Tract2=Tract)
PairList = PairList %>% expand(Tract,Tract2)
PairList = PairList %>% mutate(Tract = as.character(Tract),
                               Tract2 = as.character(Tract2))
PairList = PairList %>% mutate(OD_Pair=paste(Tract,Tract2,sep="-"))
PairList = PairList %>% rename(PU_Tract=Tract,DO_Tract=Tract2)

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


#PairList = PairList %>% filter(!duplicated(paste0(pmax(Tract,Tract2),pmin(Tract,Tract2))))

##Aggregate by 60 min####
Dates = seq(ISOdatetime(2019,12,24,0,0,0,tz="UTC"),ISOdatetime(2020,01,14,0,0,0,tz="UTC"),by=(60*60)) %>% as.data.frame() 
colnames(Dates)[1] = "Dates"

dtaOrg = expand.grid(Dates$Dates,PairList$OD_Pair)
dtaOrg = dtaOrg %>% rename(Start_Time=Var1,OD_Pair=Var2)
dtaOrg = left_join(dtaOrg,PairList,by="OD_Pair") %>% mutate(Hour=hour(Start_Time),
                                                            Minute=minute(Start_Time),
                                                            Date=as.Date(Start_Time))
dtaOrg = dtaOrg %>% mutate(TrtInt=ifelse(Hour==22,0,1))

dta = dta %>% mutate(Hour=hour(Start_Time),
                     Minute=minute(Start_Time),
                     Date=as.Date(Start_Time))

#Geo

dta60 = dta %>% 
  group_by(week,Date,Hour,OD_Pair,GeoTrt) %>% 
  summarise(Count = sum(exist,na.rm = T),
            Trip.Seconds = mean(Trip.Seconds,na.rm = T),
            Trip.Miles = mean(Trip.Miles,na.rm = T),
            Fare = mean(Fare,na.rm = T),
            Tip = mean(Tip,na.rm = T),
            Additional.Charges = mean(Additional.Charges,na.rm = T),
            Trip.Total = mean(Trip.Total,na.rm = T)
  )
dta60 = dta60 %>% mutate(Start_Time=ymd_h(paste(Date,Hour,sep=" ")))
dta60 = left_join(dtaOrg,dta60,by=c("Start_Time","OD_Pair","Date","Hour"))
dta60 = dta60 %>% 
  mutate(week=week(Start_Time)) %>% 
  filter(week %in% c(53,2))
dta60 = dta60 %>% mutate(Count=ifelse(is.na(Count)==T,0,Count),
                         Post=ifelse(week==2,1,0),
                         GeoTrt=ifelse(PU_Tract%in%CTListTrt$Tract|DO_Tract%in%CTListTrt$Tract,1,0))
dta60 = dta60 %>% filter(!Hour%in%c(0,1,2,3,4))
dta60 = dta60 %>% mutate(PairDist=as.numeric(PairDist))

write.csv(dta60,"/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/dtaAgg60.csv")

#