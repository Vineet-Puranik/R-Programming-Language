##5
 library(tidyverse)
 library(mosaic)
 georgia2000 = georgia2000 %>%
   mutate(bush_win = (bush > gore)) 

 diffprop(bush_win~poor, data = georgia2000)
 beta = do(10000) * diffprop(bush_win~poor, data = resample(georgia2000)) %>% round(2)
 confint(beta)
 
 ##6
 georgia2000 = georgia2000 %>%
 mutate(ucountPct = 100 * (ballots -votes)/ballots)
 
beta2 = do(10000) * diffmean(ucountPct~optical, data =resample(georgia2000))
confint(beta2)

mean(ucountPct~optical, data=georgia2000)

##8

beta3 = do(10000) * diffmean(ucountPct~urban, data =resample(georgia2000))
confint(beta3)

diffmean(ucountPct~urban, data=georgia2000)

##9
beta4 = do(10000)*lm(creatclear ~ age, data = resample(creatinine))
confint(beta4)

##10

sd(~age, data = beta4)

