library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(hms)
library(sf)

#Slots
DateList=function(Year,Num){
  Dates = seq(ymd_hms(paste(Year,"/01/01 00:00:00",sep=""))%m-% months(Num),
              ymd_hms(paste(Year,"/01/01 00:00:00",sep=""))%m+% months(Num),
              by=(60*60*24)) %>% as.data.frame()
}
Dates = function(Month){
  Dates=rbind(DateList("2019",Month),DateList("2020",Month)) %>% rename("Date"=1)
  return(Dates)
}

dtaOrg=function(Month){
  dtaOrg = expand.grid(Dates(Month)$Date,PairList$OD_Pair) %>% 
    rename(Date=Var1,OD_Pair=Var2)
  dtaOrg =left_join(dtaOrg,PairList,by="OD_Pair") %>% 
    mutate(Date=as.Date(Date))
  return(dtaOrg)
}





