library(tidyverse)
library(mosaic)
## Q1
CPS85 %>%
  group_by(married) %>%
  summarize (count = n())

CPS85 = CPS85 %>%
  mutate(MARRIED = ifelse (married == "Married", yes=  "1", 
                               no = "2"))

sim1 = do (10000) * prop (~MARRIED, data = resample(CPS85))
sd(~prop_1, data = sim1)

confint(sim1) %>%
mutate_if(is.numeric, round, digits=3)

## Q3

sim2 = do (10000) * mean (~wage, data = resample(CPS85))
sd(~mean, data = sim2)

#Q4
  
  CPS85 = CPS85 %>%
    mutate(notinunion = ifelse (union == "Not", no=  "2", 
                             yes = "1"))
  
  sim3 = do (10000) * prop (~notinunion, data = resample(CPS85))
  
  confint(sim3) %>%
    mutate_if(is.numeric, round, digits=3)

#Q5

sim4 = do (10000) * median (~wage, data = resample(CPS85))

confint(sim4) %>%
  mutate_if(is.numeric, round, digits=3)

