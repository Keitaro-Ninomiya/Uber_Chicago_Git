source("/home/keitaro2/Uber_Chicago/Data_Cleaning/Trip_Data/Load_Data/Code_v2.R")
source("/home/keitaro2/Uber_Chicago/Data_Cleaning/Trip_Data/Add_Distance/Code.R")

dta = readDay()
dta = add_distance(dta)

write.csv(dta,"/home/keitaro2/Uber_Chicago/Data_Cleaning/Trip_Data/dtaDay.csv")