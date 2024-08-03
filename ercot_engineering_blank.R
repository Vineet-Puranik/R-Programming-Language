####################################################   
##### Feature Engineering for Machine Learning #####
####################################################
library(tidyverse)
library(lubridate)
library(modelr)
library(rsample)
library(mosaic)

# Power grid load every hour for 6 1/2 years throughout the 8 ERCOT 
# regions of Texas. Units of grid load are megawatts, representing 
# peak instantaneous demand for power during that hour.
# source: scraped from the ERCOT website

# import dataset: load_ercot.csv
glimpse(load_ercot)

# Now import weather data load_temperature.csv 
# from the KHOU weather station (temps in degrees F)
glimpse(load_temperature)

# Feature engineering step 1:
# Merge the two data sets on their common field: Time
# Now we'll have access to the temperature data to predict power consumption
load_combined = merge(load_ercot, load_temperature, by = "Time")


# Plot power consumption in the COAST region versus
# temperature at Houston Hobby Airport (weather station KHOU)
# We know this is going to be a useful feature, but it doesn't look linear!
# It looks more like a parabola

  ggplot(load_combined)+
    geom_point(aes(x = KHOU, y = COAST), alpha = .1)

# Feature engineering step 2:
# Let's create a really useful feature: the square of temperature!
# This will help us estimate that quadratic-looking relationship
load_combined = mutate(load_combined, KHOU_squared = KHOU^2)


# What about the Time variable?
head(load_combined$Time)

# first things first: let's get the Time variable into a format R understands.
# Right now R thinks it's just a string of characters.
# We need to tell R it's actually a time stamp in a specific format: Y-M-D H:M:S
# We'll do this with the ymd_hms function in the lubridate package
load_combined = mutate(load_combined, Time = ymd_hms(Time))


# Now let's plot power consumption in the COAST region over time

  ggplot(load_combined)+
    geom_line(aes(x = Time, y= COAST))

# We notice strong seasonal trends (winter vs. summer),
# as well as a general upward trend as time marches on (pop growth).  
# We also expect power consumption to vary on a finer time scale:
#   - across the day (wake vs. sleep)
#   - aross the week (weekend vs. weekday)

# Let's use the functions in lubridate to engineer some relevant
# features from the Time variable.
# remember: factor tells R to treat a number as a category and make dummies.
# This is important: we don't want month/day as a number, but as a category.
# the last line will get us a feature that models an overall upward trend
load_combined = mutate(load_combined, 
                       hour = hour(Time) %>% factor(),     # hour of day
                       wday = wday(Time) %>% factor(),     # day of week (1 = Monday)
                       month = month(Time) %>% factor(),   # month of year (1 = Jan)
                       weeks_elapsed = time_length(Time - ymd_hms('2010-01-01 01:00:00'), unit='weeks'))

head(load_combined)


# let's create a train/test split and compare the performance of a few models
# that use our engineered features
load_split =  initial_split(load_combined, prop= .9)
load_train = training(load_split)
load_test  = testing(load_split)

# just KHOU temperature and temp^2
lm1 = lm(COAST ~ KHOU + KHOU_squared, data = load_train)

# check performance on the testing set

rmse(lm1, load_test)
# a medium model that incorporates a time trend and monthly effects, via monthly dummy variables
lm2 = lm(COAST ~ KHOU + KHOU_squared + month + hour + wday + weeks_elapsed, data = load_train)
  
# noticeable improvement on testing set
rmse(lm2, load_train)
rmse(lm2, load_test)

# a bigger model that incorporates hourly and day-of-week effects,
# Note: it's really important that we had month/hour/day as factors.
# we want them to be dummy variables, not linear terms!
lm3 = lm(COAST ~ KHOU + KHOU_squared + month + hour + wday + weeks_elapsed + (KHOU + KHOU_squared):(month+hour+wday) + hour:(wday+month), data = load_train)


# huge improvement on testing set
rmse(lm3, load_train)
rmse(lm3, load_test)

# number of parameters
length(coef(lm2))
length(coef(lm3))

# And remember, this is ML, not stats!
# Don't bother trying to interpret the model coefficients or intervals. 


# with all the different dummy variables, quadratic terms, and interactions, 
# it's nearly impossible to interpret the individual coefficients.
# But it's a pretty good "black box" predictive model! 
# features in, predictions out
