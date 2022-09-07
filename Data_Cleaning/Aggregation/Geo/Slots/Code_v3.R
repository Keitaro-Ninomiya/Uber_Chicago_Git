#Created April 24th
#Added flexibility to Units(CT or CO)
#Create weekly slots independantly; aggregating daily slots takes time to compute.

DateList=function(Year,Num){
  Dates = seq(ymd_hms(paste(Year,"/01/01 00:00:00",sep=""))%m-% months(Num),
              ymd_hms(paste(Year,"/01/01 00:00:00",sep=""))%m+% months(Num),
              by=(60*60*24)) %>% as.data.frame()
  return(Dates)
  }
Dates = function(Month){
  Dates=rbind(DateList("2019",Month),DateList("2020",Month)) %>% rename("Date"=1)
  return(Dates)
}

WeekList=function(Year,Num){
  Dates = seq(ymd_hms(paste(Year,"/01/01 00:00:00",sep=""))%m-% months(Num),
              ymd_hms(paste(Year,"/01/01 00:00:00",sep=""))%m+% months(Num),
              by=(60*60*24)) %>% 
    as_tibble() %>% 
    rename(date=1)
  Dates = Dates %>% 
    mutate(Date=floor_date(date,unit="weeks")) %>% 
    distinct(Date)
  return(Dates)
  }
Weeks = function(Month){
  Dates=rbind(WeekList("2019",Month),WeekList("2020",Month))
  Dates = Dates %>% rename(Week=Date)
  return(Dates)
  }

dtaOrg=function(Month, GeoUnit, TimeUnit){
  if(GeoUnit=="Census_Tract" & TimeUnit=="Day"){
    Org = expand.grid(Dates(Month)$Date,unique(PairListCT$OD_Pair)) %>% 
      rename(Date=Var1,OD_Pair=Var2)
    Org =left_join(Org,PairListCT,by="OD_Pair") %>% 
      mutate(Date=as.Date(Date)) %>% 
      rename(OD_PairCT=OD_Pair)
    return(Org)
  }
  
  if(GeoUnit=="Community_Area" & TimeUnit=="Day"){
    Org = expand.grid(Dates(Month)$Date,unique(PairListCO$OD_Pair)) %>% 
      rename(Date=Var1,OD_Pair=Var2)
    dtaOrg =left_join(Org,PairListCO,by="OD_Pair") %>% 
      mutate(Date=as.Date(Date)) %>% 
      rename(OD_PairCO=OD_Pair)
    return(Org)
  }
  
  if(GeoUnit=="Census_Tract" & TimeUnit=="Week"){
    Org = expand.grid(Weeks(Month)$Week,unique(PairListCT$OD_Pair)) %>% 
      rename(Week=Var1,OD_Pair=Var2)
    Org =left_join(Org,PairListCT,by="OD_Pair") %>% 
      mutate(Week=as.Date(Week)) %>% 
      rename(OD_PairCT=OD_Pair)
    return(Org)
  }
  
  if(GeoUnit=="Community_Area" & TimeUnit=="Week"){
    dtaOrg = expand.grid(Weeks(Month)$Week,unique(PairListCO$OD_Pair)) %>% 
      rename(Week=Var1,OD_Pair=Var2)
    dtaOrg =left_join(dtaOrg,PairListCO,by="OD_Pair") %>% 
      mutate(Week=as.Date(Week)) %>% 
      rename(OD_PairCO=OD_Pair)
    return(dtaOrg)
  }
}