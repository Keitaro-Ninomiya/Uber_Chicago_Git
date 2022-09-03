library(sf)
library(dplyr)

library(lubridate)
library(hms)

index = 0
chunkSize=1000000

readWeek=function()
  {
  filename="/home/keitaro2/Uber_Chicago/Raw_Data/Data_Trips/Transportation_Network_Providers_-_Trips.csv"
  
  int1=interval(ymd_hms("2018-10-31 00:00:00"),ymd_hms("2019-02-28 23:59:59"))
  int2=interval(ymd_hms("2019-10-31 00:00:00"),ymd_hms("2020-02-28 23:59:59"))
  
  colnames=c("Trip ID","Trip Start Timestamp", "Trip End Timestamp", "Trip Seconds", "Trip Miles", "Pickup Census Tract","Dropoff Census Tract",
             "Pickup Comunity Area","Dropoff Comunity Area", "Fare","Tip","Additional Charges","Trip Total","Shared Trip Authorized","Trips Pooled",
             "Pickup Centroid Latitude","Pickup Centroid Longitude","Pickup Centroid Location","Dropoff Centroid Latitude","Dropoff Centroid Longitude","Dropoff Centroid Location")
  
  con = file(description = filename,open="r")
  
  dta = read.table(con,nrows=chunkSize,header=T, fill=TRUE,sep=",",col.names=colnames)
  dta = dta %>% mutate(Trip.Start.Timestamp=mdy_hms(Trip.Start.Timestamp),
                       Trip.End.Timestamp=mdy_hms(Trip.End.Timestamp))
  dta = dta %>% filter(Trip.Start.Timestamp%within%int1|Trip.Start.Timestamp%within%int2)
  
  repeat {
    index = index + 1
    print(paste('Processing rows:', index * chunkSize))
    
    dta_temp <- read.table(con, nrows=chunkSize, skip=index*chunkSize, header=FALSE, fill = TRUE, sep=",", col.names=colnames)
    
    if (nrow(dta_temp)<chunkSize) break
    
    dta_temp = dta_temp %>% mutate(Trip.Start.Timestamp=mdy_hms(Trip.Start.Timestamp),
                                   Trip.End.Timestamp=mdy_hms(Trip.End.Timestamp))
    dta_temp = dta_temp %>% filter(Trip.Start.Timestamp%within%int)
    
    dta=rbind(dta,dta_temp)
  }
  close(con)
  return(dta)
}

readMonth=function()
{
  int1=interval(ymd_hms("2018-08-01 00:00:00"),ymd_hms("2019-05-31 23:59:59"))
  int2=interval(ymd_hms("2019-08-01 00:00:00"),ymd_hms("2020-05-31 23:59:59"))
  
  colnames=c("Trip ID","Trip Start Timestamp", "Trip End Timestamp", "Trip Seconds", "Trip Miles", "Pickup Census Tract","Dropoff Census Tract",
             "Pickup Comunity Area","Dropoff Comunity Area", "Fare","Tip","Additional Charges","Trip Total","Shared Trip Authorized","Trips Pooled",
             "Pickup Centroid Latitude","Pickup Centroid Longitude","Pickup Centroid Location","Dropoff Centroid Latitude","Dropoff Centroid Longitude","Dropoff Centroid Location")
  
  con = file(description = filename,open="r")
  
  dta = read.table(con,nrows=chunkSize,header=T, fill=TRUE,sep=",",col.names=colnames)
  dta = dta %>% mutate(Trip.Start.Timestamp=mdy_hms(Trip.Start.Timestamp),
                       Trip.End.Timestamp=mdy_hms(Trip.End.Timestamp))
  dta = dta %>% filter(Trip.Start.Timestamp%within%int1|Trip.Start.Timestamp%within%int2)
  
  repeat {
    index = index + 1
    print(paste('Processing rows:', index * chunkSize))
    
    dta_temp <- read.table(con, nrows=chunkSize, skip=index*chunkSize, header=FALSE, fill = TRUE, sep=",", col.names=colnames)
    
    if (nrow(dta_temp)<chunkSize) break
    
    dta_temp = dta_temp %>% mutate(Trip.Start.Timestamp=mdy_hms(Trip.Start.Timestamp),
                                   Trip.End.Timestamp=mdy_hms(Trip.End.Timestamp))
    dta_temp = dta_temp %>% filter(Trip.Start.Timestamp%within%int)
    
    dta=rbind(dta,dta_temp)
  }
  close(con)
  return(dta)
}