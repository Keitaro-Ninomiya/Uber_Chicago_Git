library(lubridate)
library(hms)
library(dplyr)
dta=read.csv("C:/Users/Keitaro Ninomiya/Desktop/MyDailyTravelData/person.csv") %>% 
  mutate(date_completed=ymd(date_completed),
         tnc_purp_desc = case_when(tnc_purp==-9~"Not Ascertained",
                                   tnc_purp==-8~"I don't know",
                                   tnc_purp==-7~"Prefer not to answer",
                                   tnc_purp==-1~"Appropriate skip",
                                   tnc_purp==1~"Commute",
                                   tnc_purp==2~"For Transit in commute",
                                   tnc_purp==3~"Work Travel",
                                   tnc_purp==4~"Personal",
                                   tnc_purp==5~"Late-night service"),
         Year=year(date_completed))

dta %>% 
  filter(tnc_purp>0) %>% 
  count(tnc_purp_desc,wt=wtperfin) %>% 
  mutate(prop = prop.table(n)) 

dta %>% 
  filter(tnc_purp>0,
         year(date_completed)==2019) %>% 
  count(tnc_purp_desc,wt=wtperfin) %>% 
  mutate(prop = prop.table(n))

dta %>% 
  filter(tnc_purp>0,
         year(date_completed)==2018) %>% 
  count(tnc_purp_desc,wt=wtperfin) %>% 
  mutate(prop = prop.table(n))
