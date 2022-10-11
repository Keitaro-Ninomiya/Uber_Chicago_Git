library(modelsummary)
library(dplyr)

#

regression=function(dta,Dist){
  results = glm(Count~GeoTrt+as_factor(week)+GeoTrt:Post+Post+PairDist,
                data=dta %>% 
                  filter(PU_Dist<Dist|DO_Dist<Dist,
                         PU_BdN==1|DO_BdN==1|PU_BdS==1|DO_BdS==1),
                family = poisson(link = "log"))
}

regBD=function(dta){
  return(modelsummary(lapply(c(500,1000,2000,3000),dta=dta,regression),stars = TRUE,output="latex"))
}