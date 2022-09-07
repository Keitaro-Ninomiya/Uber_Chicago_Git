library(dplyr)
#### Download Data ####
#Trip_Level_Data
dta = read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/dtaDist.csv")
CTList = read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/Boundaries - Census Tracts - 2010.geojson")
zone = read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/downtown_zone.shp")
CommuList = read_sf("/home/keitaro2/Uber_Chicago/Raw_Data/Data_Geo/Modified/CommuList_Outside.geojson") %>% 
  rename(comm=area_num_1)
#Pair:Census_Tract_Level
PairListCT=read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/PairList.csv")
#Pair:Community_Area_Level
PairListCO=read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/PairList_Expand.csv")

#Clean Data and Add variables
#Trip_Level
source("/home/keitaro2/Uber_Chicago_Git/Data_Cleaning/Cleaning/Code.R")
dta=Clean(dta,CommuList)

#Convert
source("/home/keitaro2/Uber_Chicago_Git/Data_Cleaning/Aggregation/Geo/Code_v8.R")
Aggregate(dta)

#Analysis
