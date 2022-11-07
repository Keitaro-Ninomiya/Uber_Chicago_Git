library(dplyr)
library(tidyr)

library(lubridate)
library(hms)

library(fixest)
library(modelsummary)
library(stargazer)

path_remote="/home/keitaro2/Uber_Chicago_Git"
path_remote_OL="/home/keitaro2/Uber_Chicago_OL"

dta = read.csv("/home/keitaro2/Uber_Chicago/Raw_Data/Key_Data/dtaDist_Trn.csv")
#Clean Data
source("/home/keitaro2/Uber_Chicago_Git/Background/DID/Clean_Data/Code.R")
dta=Clean_Data(dta)

#### 2. Estimation####
#### 2.1. Geographic Border ####
dtaGeo=Clean_for_Geo(dta)

#### 2.1.1 Aggregate ####
source("/home/keitaro2/Uber_Chicago_Git/Background/DID/Regression/Code.R")
setwd(paste(path_remote_OL,"/Background/DID",sep=""))
##Additional Charges##
Result=regBD(dtaGeo,"Additional")
fileConn<-file("Results_Add_Geo.tex")
writeLines(Result, fileConn)
close(fileConn)

##Distance Charges##
Result=regBD(dtaGeo,"Dist")
fileConn<-file("Results_Dist_Geo.tex")
writeLines(Result, fileConn)
close(fileConn)

##Time Charges##
Result=regBD(dtaGeo,"Time")
fileConn<-file("Results_Time_Geo.tex")
writeLines(Result, fileConn)
close(fileConn)