Clean_Data=function(dta){
  #Erase trips with missing PU/DO location
  dta = dta %>% filter(is.na(PU_Dist)==F & is.na(DO_Dist)==F)
  #Erase shared trips
  dta = dta %>% filter(Shared.Trip.Authorized=="false")
  #Erase trips with high tax
  dta = dta %>% filter(!PU_name10%in%c("8382","814.02","3301","7706.02","9800")&
                         !DO_name10%in%c("814.02","3301","7706.02","9800"))
  #Erasing trips with missing fares
  dta = dta %>% filter(Additional.Charges!=0)
  dta = dta %>% filter(Fare!=0)
  
  dta = dta %>% mutate(PU_Trt=ifelse(PU_zone=="Commute_zone",1,0),
                       DO_Trt=ifelse(DO_zone=="Commute_zone",1,0))
  dta = dta %>% mutate(PU_Trt=ifelse(is.na(PU_Trt)==T,0,PU_Trt),
                       DO_Trt=ifelse(is.na(DO_Trt)==T,0,DO_Trt))
  dta = dta %>% mutate(Trt=ifelse(PU_Trt==1 |DO_Trt==1 ,1,0))
  
  #Convert travel duration to minutes
  dta = dta %>% mutate(Trip.Minutes=Trip.Seconds/60)
  return(dta)
}

Clean_for_Geo=function(dta){
  dtaGeo = dta %>% filter(hour(Trip.End.Timestamp)>=6 & hour(Trip.Start.Timestamp)<=21)
  
  dtaGeo = dtaGeo %>% mutate(Trip.Start.Timestamp=ymd_hms(Trip.Start.Timestamp))
  dtaGeo = dtaGeo %>% mutate(Pickup.Census.Tract=as.character(Pickup.Census.Tract))
  
  dtaGeo = dtaGeo %>% mutate(week=week(Trip.Start.Timestamp),
                             Post=case_when(as.Date(Trip.Start.Timestamp)>=ymd("2020-01-06")~1,
                                            TRUE~0)) %>% 
    rename(PU_Tract=Pickup.Census.Tract,
           DO_Tract=Dropoff.Census.Tract,
           GeoTrt=Trt)
  return(dtaGeo)
}