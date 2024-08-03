library(tidyverse)
library(mosaic)

#1
prop(married ~ union, data = CPS85)
diffprop(married ~ union, data = CPS85)
xtabs(~married + union, data = CPS85)
boot_cps85 = do(10000) * diffprop(married ~ union, data = resample(CPS85))
confint(boot_cps85)
278/(278+160)
72/(72+24)

##2
lm1 = lm(wage ~ educ, data = CPS85)
coef(lm1)
boot_lin = do(10000) * lm(wage ~ educ, data = resample(CPS85))
confint(boot_lin)

##3
mean(wage ~ sex, data = CPS85)
diffmean(wage ~ sex, data = CPS85)
boot_avg = do(10000) * diffmean(wage ~ sex, data = resample(CPS85))
confint(boot_avg)

##4

CPS85  = CPS85 %>%
  mutate(if_clerical = ifelse(sector == "clerical", "clerical", "not_clerical"))

boot_cler = do(10000) * prop(~if_clerical, data = resample(CPS85))
confint(boot_cler)
prop.test(~if_clerical, data = CPS85)

##5
boot_exper = do(10000) * mean(~exper, data = resample(CPS85))
confint(boot_exper)
prop.test(~exper, data = CPS85)





