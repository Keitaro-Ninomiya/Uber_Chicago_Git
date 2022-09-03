library(sf)
library(dplyr)
library(stringr)
library(lubridate)
library(hms)

dta = read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/dta.csv")
add_distance(dta){
  tracts=read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/Boundaries - Census Tracts - 2010.geojson")
  zone=read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/downtown_zone.shp")
  
  border=read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/border.shp")
  borderN=read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/borderN.geojson")
  borderNW=read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/borderNW.geojson")
  borderSW=read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/borderSW.geojson")
  borderS=read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/borderS.geojson")
  
  zone = zone %>% mutate(zone="Commute_zone")
  tracts=st_join(tracts,zone)
  
  
  dtaPU = dta %>% st_as_sf(coords=c("Pickup.Centroid.Longitude","Pickup.Centroid.Latitude"),crs=4326,na.fail=F)
  tractsPU=tracts %>% rename_at(vars(statefp10:countyfp10),~paste0("PU_",.x))
  tractsPU=tractsPU %>% rename(PU_FID=FID,PU_zone=zone) 
  dtaPU = st_join(dtaPU,tractsPU)
  dtaPU = dtaPU %>% mutate(PU_Trt=ifelse(PU_zone=="Commute_zone" &Trip.Start.Timestamp>ymd("2020-01-05")&
                                           hour(Trip.Start.Timestamp)<22:00 & hour(Trip.Start.Timestamp)>06:00,1,0),
                           PU_Dist= as.numeric(st_distance(dtaPU,border)),
                           PU_DistN =as.numeric(st_distance(dtaPU,borderN)),
                           PU_DistNW=as.numeric(st_distance(dtaPU,borderNW)),
                           PU_DistSW=as.numeric(st_distance(dtaPU,borderSW)),
                           PU_DistS =as.numeric(st_distance(dtaPU,borderS)))
  dtaPU = dtaPU %>% mutate(PU_BdN =ifelse(PU_DistN==pmin(PU_DistN,PU_DistNW,PU_DistSW,PU_DistS),1,0),
                           PU_BdNW=ifelse(PU_DistNW==pmin(PU_DistN,PU_DistNW,PU_DistSW,PU_DistS),1,0),
                           PU_BdSW=ifelse(PU_DistSW==pmin(PU_DistN,PU_DistNW,PU_DistSW,PU_DistS),1,0),
                           PU_BdS =ifelse(PU_DistS==pmin(PU_DistN,PU_DistNW,PU_DistSW,PU_DistS),1,0))
  dtaPU = as.data.frame(dtaPU %>% mutate(geometry=NULL))
  
  
  dtaDO = dta %>% st_as_sf(coords=c("Dropoff.Centroid.Longitude","Dropoff.Centroid.Latitude"),crs=4326,na.fail=F)
  tractsDO=tracts %>% rename_at(vars(statefp10:countyfp10),~paste0("DO_",.x))
  tractsDO=tractsDO %>% rename(DO_FID=FID,DO_zone=zone) 
  dtaDO = st_join(dtaDO,tractsDO)
  dtaDO = dtaDO %>% mutate(DO_Trt=ifelse(DO_zone=="Commute_zone" &Trip.Start.Timestamp>ymd("2020-01-05")&
                                           hour(Trip.Start.Timestamp)<22:00 & hour(Trip.Start.Timestamp)>06:00,1,0),
                           DO_Dist=st_distance(dtaDO,border))
  dtaDO = dtaDO %>% mutate(DO_Trt=ifelse(DO_zone=="Commute_zone" &Trip.Start.Timestamp>ymd("2020-01-05")&
                                           hour(Trip.Start.Timestamp)<22:00 & hour(Trip.Start.Timestamp)>06:00,1,0),
                           DO_Dist=st_distance(dtaDO,border),
                           DO_DistN =st_distance(dtaDO,borderN),
                           DO_DistNW=st_distance(dtaDO,borderNW),
                           DO_DistSW=st_distance(dtaDO,borderSW),
                           DO_DistS =st_distance(dtaDO,borderS))
  dtaDO = dtaDO %>% mutate(DO_BdN =ifelse(DO_DistN== pmin(DO_DistN,DO_DistNW,DO_DistSW,DO_DistS),1,0),
                           DO_BdNW=ifelse(DO_DistNW==pmin(DO_DistN,DO_DistNW,DO_DistSW,DO_DistS),1,0),
                           DO_BdSW=ifelse(DO_DistSW==pmin(DO_DistN,DO_DistNW,DO_DistSW,DO_DistS),1,0),
                           DO_BdS =ifelse(DO_DistS== pmin(DO_DistN,DO_DistNW,DO_DistSW,DO_DistS),1,0))
  dtaDO = as.data.frame(dtaDO %>% mutate(geometry=NULL))
  
  dta=full_join(dtaPU,dtaDO,by=c("Trip.ID","Trip.Start.Timestamp", "Trip.End.Timestamp", "Trip.Seconds", "Trip.Miles",
                                 "Pickup.Comunity.Area","Dropoff.Comunity.Area", "Fare","Tip","Additional.Charges","Trip.Total","Shared.Trip.Authorized","Trips.Pooled",
                                 "Pickup.Census.Tract","Dropoff.Census.Tract","Pickup.Centroid.Location","Dropoff.Centroid.Location"))
  
  dta = dta %>% mutate(Pickup.Census.Tract=as.character(Pickup.Census.Tract),
                       Dropoff.Census.Tract=as.character(Dropoff.Census.Tract))
  dta = dta %>% mutate(Trip.Start.Timestamp=ymd_hms(as.character(Trip.Start.Timestamp)),
                       Trip.End.Timestamp=ymd_hms(as.character(Trip.End.Timestamp)))
  
  dta =dta %>% select(Trip.ID,Trip.Start.Timestamp,Trip.End.Timestamp,Trip.Seconds,Trip.Miles,Pickup.Census.Tract,Dropoff.Census.Tract,
                      Pickup.Comunity.Area,Dropoff.Comunity.Area,Fare,Tip,Additional.Charges,Trip.Total,Shared.Trip.Authorized,Trips.Pooled,
                      Pickup.Centroid.Location,Dropoff.Centroid.Location,
                      PU_Trt,PU_Dist,DO_Trt,DO_Dist,PU_name10,DO_name10,PU_zone,DO_zone,
                      PU_tractce10,DO_tractce10,
                      PU_BdN,PU_BdNW,PU_BdSW,PU_BdS,
                      DO_BdN,DO_BdNW,DO_BdSW,DO_BdS)
  return(dta)
}