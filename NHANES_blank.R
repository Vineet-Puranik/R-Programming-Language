      #################################################
      #####  Bootstrapping Means and Proportions  #####
      #################################################
library(tidyverse)
library(mosaic)
theme_set(theme_classic())  # set ggplot theme (optional)
      
# load NHANES_sleep.csv data  # # Make sure to check for a header row!
glimpse(NHANES_sleep)

      ##### Bootstrapping with sample means #####      
# how well are Americans sleeping, on average? Let's visualize
# the data distribution of sleeping hours per night in the sample
            # ggplot(NHANES_sleep) + 
            #   geom_histogram(aes(x = SleepHrsNight), color = 'linen', 
            #                  binwidth=1) # use binwidth = 1 for one-hour bins

ggplot(NHANES_sleep) +
  geom_histogram(aes(x = SleepHrsNight), color = 'linen', binwidth = 1)

mean (~SleepHrsNight, data = NHANES_sleep)

# for a well-designed rigorous study such as NHANES the sample mean 
# should be a decent estimate for the population mean...



# but how close is it to the right answer for the whole population?
# remember, this is just a survey... 
# a very well-designed survey, but a survey nonetheless!

# let's try a few bootstrap samples by repeating these lines below.
# Looks like our bootstrap estimate is pretty close.
# Doesn't seem like a huge amount of sampling variability...

NHANES_sleep_bootstrap = resample (NHANES_sleep)

mean(~SleepHrsNight, data = NHANES_sleep_bootstrap)


# Let's be more systematic, with 10,000 bootstrap samples
# below we are combining the do() function and the resample() function
   # this combination will be our standard procedure for bootstrapping
boot_sleep = do(10000) * mean (~SleepHrsNight, data =  resample (NHANES_sleep))
   head(boot_sleep)
# What does this boot_sleep object look like?
# Let's examine the first several lines: one column called mean
# these are all the means from our 10000 bootstrap samples

   
# visualize bootstrap sampling distribution (10000 means)
               # ggplot(boot_sleep) + 
               #   geom_histogram(aes(x=mean), 
               #                  color = 'midnightblue', 
               #                  fill = 'oldlace')

# how spread out is the sampling distribution? calculate standard error:

   ggplot (boot_sleep) + 
     geom_histogram(aes (x = mean), color = 'midnightblue', fill = 'oldlace')
   
   2*sd (~mean, data = boot_sleep)
   
   # So it looks like a typical sampling error is about 0.03 hours,
   # or roughly 2 minutes. 

# now let's calculate a confidence interval: a range of plausible values for the
# population estimand, based on the bootstrapped sampling distribution

   confint(boot_sleep, level = .95)

#  interpretation? We can say with 95% confidence that Americans are, on average, 
#  getting between 6.82 and 6.94 hours of sleep. In other words, we are 95% confident
#  that our sampling process will generate an interval containing the true estimand  

      # can we eyeball these lower and upper endpoints of the
      # confidence interval from the histogram?

# a 95% bootstrap confidence interval "chops off" 2.5% of the distribution in
# each tail, "capturing" 95% of the distribution in the middle. The interval limits
# are the 2.5th and 97.5th percentiles (i.e., the 0.025 and 0.975 quantiles)
            # boot_sleep %>% 
            #    summarise(
            #       lower = quantile(mean, 0.025),
            #       upper = quantile(mean, 0.975),
            #       margin_of_error = (upper-lower)/2) %>%
            #          round(2)
   
   boot_sleep %>%
     summarize ( lower = quantile (mean, .025),
                 upper = quantile (mean, .975),
                 margin_of_error = (upper - lower)/2) %>%
     round (2)
   
# the margin of error is the length of the interval divided by 2
# a 95% confidence interval tends to be approximately two standard errors to either 
# side of your "best guess" (i.e., sample estimate) for the estimand 



      #################################################
      ##### Bootstrapping with sample proportions #####  

# let's use the variable 'Depressed', based on a survey questions asking 
# respondents about how many days of the week they have symptoms of depression: 
      # None of the days, Most days, or Several days

# view a table of counts in each category

NHANES_sleep %>%
     group_by(Depressed) %>%
     summarize (count = n())


# let's mutate() a new DepressedAny variable using the ifelse() function 
# to indicate whether or not someone reported Most days/Several days  VS None 
NHANES_sleep = NHANES_sleep %>%
  mutate(DepressedAny = ifelse (Depressed =="None", "None", 
                                "Any"))
   
   
   
# now we have a binary variable and can calculate a sample proportion:

prop (~DepressedAny, data = NHANES_sleep) %>%
  round  (3)
   
   
# How precisely does this survey result characterize the frequency of depression
# among all Americans? Let's bootstrap to understand the likely magnitude of
# estimation error due to sampling variability
boot_depression = do (10000) * prop (~DepressedAny, data = resample(NHANES_sleep))
head(boot_depression)
# view the simulation: single column called prop_Any


   
# histogram of the 10000 different estimates
# for prop_Any

   ggplot (boot_depression) +
     geom_histogram(aes(x = prop_Any))
   
# a 95% confidence interval for the population proportion

   confint(boot_depression) %>%


   ### interpretation of interval? ###

# advanced helpful code to adjust decimal places for confint output:
confint(boot_depression) %>%
   mutate_if(is.numeric, round, digits=3)
      # note also that you can leave out 'level=0.95' from confint()
      # as this is the function default

# even more optional pro moves: use select() to show only 
# essential columns in interval output table:
confint(boot_depression) %>%
   select(-method, -level) %>%   # remove method and level columns
   mutate_if(is.numeric, round, digits=3)

# how spread out is the sampling distribution? calculate standard error:

sd (~prop_Any, data = boot_depression) ## standard error
2 *sd (~prop_Any, data = boot_depression) ## margin of error

# so it looks like a typical sampling error is about 0.009,
# or roughly 1%. 

# margin of error:


# ZZZZzzzzzz-score
# what's more extreme, getting a sample average of 7 hours 
# of sleep or a sample proportion of 25% any depressed?

# z-score for sleep:
(7 -)/

# z-score for depression:
(.25 -)/

