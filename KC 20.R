library(tidyverse)
library(mosaic)


##  4

lm2 = lm(Income ~ Gender + Age + Gender:Age, data = GSS)
coef(lm2)
confint(lm2)
7106+16785+684