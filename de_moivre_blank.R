      ##### de Moivre's equation -- Day 16 #####
library(tidyverse)
library(mosaic)

# import NHANES_sleep data file; check for column headers

# visualize the variable for average hours of sleep per night:      

      ggplot(NHANES_sleep) + 
        geom_histogram(aes(x = SleepHrsNight), color = 'linen', binwidth = 1)
      
      
      
# on average, how much sleep are respondents in our sample getting?

      mean(~SleepHrsNight, data = NHANES_sleep)
      
# de Moivre's equation elements:
    nrow(NHANES_sleep)                                   # sample size
    sd(~SleepHrsNight, data = NHANES_sleep)             # SD of individual data point

# put together these values ^^^ to calculate the 
# standard error of the sample mean
std_error = sd(~SleepHrsNight, data = NHANES_sleep)  / sqrt(nrow(NHANES_sleep))
std_error
# compare with bootstrapping
boot_sleep = do(10000)* mean(~SleepHrsNight, data = resample(NHANES_sleep))
   

# calculate bootstrapped standard error: really close

sd(~mean, data = boot_sleep)
   
# confidence interval based on the CLT?
mean_sleep = mean(~SleepHrsNight, data = NHANES_sleep)
mean_sleep -2*std_error
mean_sleep + 2*std_error

   
   # the LL and UL of the interval are two standard errors 
   # to either side of our sample estimate 
                              # 6.88 - 2 * 1.32/sqrt(1991)
                              # 6.88 + 2 * 1.32/sqrt(1991)

# compare with bootstrapping

confint(boot_sleep)


# Take-home lesson: you get a confidence interval that's
# basically identical to what you get when you bootstrap,
# except using math rather than Monte Carlo simulation.


##### t-test shortcut

# it's tedious to have to do these calculations "by hand",
# i.e., using R as a calculator and manually typing out
# the formula for de Moivre's equation

# Luckily, there's a shortcut, using a built-in R function
# called t.test(): 
t.test(~SleepHrsNight, data = NHANES_sleep)

# This function returns the sample estimate and a p-value 
# in the console, but we are primarily interested in the 
# 95% confidence interval

t.test(~Age, data = NHANES_sleep)