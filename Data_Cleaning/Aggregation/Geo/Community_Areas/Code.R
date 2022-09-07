#Create April 19th
#Creates list of community zones with difference in distance from border. 

library(sf)
library(ggplot2)
library(dplyr)

#Download Data
zone=read_sf("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Uber_Chicago/Raw_Data/Data Geo/downtown_zone.shp")
Commu=read_sf("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Uber_Chicago/Raw_Data/Data Geo/Boundaries - Community Areas (current).geojson")
CenTrac=read_sf("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Uber_Chicago/Raw_Data/Data Geo/Boundaries - Census Tracts - 2010.geojson")
CenTrac_Cnt=read_sf("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Uber_Chicago/Raw_Data/Data Geo/Modified/CTList_Trun.geojson") %>% 
  filter(Dist_Cent<3000)
border=read_sf("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Uber_Chicago/Raw_Data/Key_Data/border.geojson")

Commu_Trun = Commu %>% 
  filter(!area_num_1%in%CenTrac_Cnt$commarea_n)
Dist_cent = st_distance(st_centroid(Commu_Trun),border)
Dist_cent=apply(Dist_cent,1,FUN=min)
Commu_Trun = Commu_Trun %>% 
  mutate(Dist=Dist_cent,
         Group=cut(Dist,breaks=c(0,5000,10000,15000,20000,25000),
                   labels=c("<5k","<10k","<15k","<20k","<25k")))
write_sf(Commu_Trun,"C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Uber_Chicago/Raw_Data/Data Geo/Modified/CommuList_Outside.geojson")
