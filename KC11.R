N= 50
library(tidyverse)
library(mosaic)
mean (~GPA, data = utsat)
theta_GPA = mean (~GPA, data = utsat) 
simutsat = do(10000)*mean(~GPA, data=sample(utsat, size=N))
sd(~mean, data = simutsat)
## yields a standard error of .07
##why is the variable mean instead of GPA 
utsat = utsat %>%
  mutate(BUSINESS = ifelse (School == "BUSINESS", yes = "1", 
                                no = "2"))
simB = do(10000)*prop(~BUSINESS, data=sample(utsat, size=100))
sd(~prop_1, data = simB)
##yields a standard error of .047
warnings()
## do - Repeatedly execute the same statement(s) many times
## sample - take a random sample of rows from a data frame
## prop - computes the sample proportoin for a binary variable
## summarize - simualate a sequence of binary events

simutsat3 = do(10000)*mean(~SAT.V, data=sample(utsat, size=250))
sd(~mean, data = simutsat3)