library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(hms)
library(sf)

RefOD=function(dta){
  CommuList = read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/Modified/CommuList_Outside.geojson") %>% 
    rename(comm=area_num_1)
  dta = dta %>% 
    rename(PU_comm=Pickup.Comunity.Area,DO_comm=Dropoff.Comunity.Area,
           PU_Tract=Pickup.Census.Tract,DO_Tract=Dropoff.Census.Tract) %>% 
    mutate(PU_Tract=as.character(PU_Tract),DO_Tract=as.character(DO_Tract),
           PU_comm=as.character(PU_comm),DO_comm=as.character(DO_comm),
           PU=case_when(PU_comm%in%CommuList$comm~PU_comm,
                        TRUE~PU_Tract),
           DO=case_when(DO_comm%in%CommuList$comm~DO_comm,
                        TRUE~DO_Tract),
           OD_PairCO=paste(PU,DO,sep="-"),
           OD_PairCT=paste(PU_tractce10,DO_tractce10,sep="-")
           )
    return(dta)}

