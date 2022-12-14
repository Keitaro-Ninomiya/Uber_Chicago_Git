library(modelsummary)
library(fixest)
library(dplyr)

#

regression=function(dta,Dist){
  results = glm(Count~GeoTrt+as.factor(week)+GeoTrt:Post+Post+PairDist,
                data=dta %>% 
                  filter(PU_Dist<Dist|DO_Dist<Dist,
                         PU_BdN==1|DO_BdN==1|PU_BdS==1|DO_BdS==1),
                family = poisson(link = "log"))
  return(results)
}

regression=function(dta,Dist){
  results = fepois(Count~GeoTrt+GeoTrt:Post+Post+PairDist|as.factor(week)+PU_Tract+DO_Tract,
                   data=dta %>%
                     filter(PU_Dist<Dist|DO_Dist<Dist,
                            PU_BdN==1|DO_BdN==1|PU_BdS==1|DO_BdS==1))
  return(results)
}

regBD=function(dta){
  return(modelsummary(lapply(c(500,1000,2000,3000),dta=dta,regression),stars = TRUE,output="latex"))
}