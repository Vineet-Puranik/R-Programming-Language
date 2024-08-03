      #########################################
      ###  Plotting and summarizing data    ###
      #########################################

library(tidyverse)
    
library(mosaic)

##### SUMMARIES for NUMERICAL DATA #####

## measures of central tendency (the "typical" value) ##
# calculating a MEAN using summarize

      olympics_top20 %>%
        summarize(avg_age=mean(age))

# other ways to calculate a mean:

      mean (~age, data = olympics_top20)
      
      mean (olympics_top20$age)

# MEDIAN with summarize

      olympics_top20 %>%
        summarize(median_age=median(age))
      
      median(olympics_top20$age)
      
# same stat, multiple variables using summarize
olympics_top20 %>% 
   summarize(
      avg_age = mean(age),
      avg_height = mean(height),
      avg_weight = mean(weight)) 

# multiple summary statistics
olympics_top20 %>% 
   summarize(
      mean = mean(age),
      std_dev = sd(age),
      median = median(age),
      q1 = quantile(age, 0.25),
      q3 = quantile(age, 0.75),
      IQR = q3-q1) 

olympics_top20 %>% 
   select(age) %>% 
   summary()

# install.packages('mosaic') 
# for my 'favorite' summary stats function favstats()
library(mosaic)


favstats(~weight, data = olympics_top20)


## measures of spread/dispersion/variation ##
# STANDARD DEVIATION

sd(olympics_top20$age)

# interquartile range (IQR)

iqr(olympics_top20$weight)

# range using max - min

max(olympics_top20$age) - min (olympics_top20$age)


