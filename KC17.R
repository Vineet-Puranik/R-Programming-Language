##7
library(tidyverse)
library(mosaic)
xtabs(~group + covid, data = vaccine)
185/(185+15025)

##8
11/ (11+15199)

##9
prop.test(covid~group, data = vaccine)
boot_vaccine = do(10000) * diffprop(covid ~ group, data = resample(vaccine))
confint(boot_vaccine)

