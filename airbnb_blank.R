      ################################      
      ##### Multiple Regression  #####
      ################################      
library(tidyverse)
library(mosaic)
library(moderndive)
library(effectsize) 

# Airbnb listings in Santa Fe, New Mexico
# airbnb = read.csv('../data/airbnb.csv')

### Part 1: Exploratory plots

# PlazaDist
# clear drop off with greater distance.  Maybe not quite linear,
# but assuming linearity isn't wildly off

      ggplot(airbnb, aes(x = PlazaDist, y = Price)) + 
        geom_point() + 
        geom_smooth(method = "lm")
      

# the size of the place explains a lot too:
# price vs bedrooms...
ggplot(airbnb) + 
  geom_jitter(aes(x = Bedrooms , y = Price), width = .05)
      
      
# and price vs bathrooms...

ggplot(airbnb) + 
  geom_jitter(aes(x = Baths , y = Price), width = .05)
      

###
# Part 2: multiple regression
###

lm_airbnb = lm(Price ~ PlazaDist + Bedrooms + Baths, data = airbnb)
   
# regression table: let's go column by column

get_regression_table(lm_airbnb)
# what if we add Superhost to our model?
# look at distribution of price by Superhost

ggplot(airbnb) + 
  geom_jitter(aes(x = Superhost , y = Price), width = .05)

# create new model
lm_airbnb_2 =lm(Price ~ PlazaDist + Bedrooms + Baths + Superhost, data = airbnb)

# regression table for model 2 

get_regression_table(lm_airbnb_2)


# doesn't look like addition of Superhost improves our model    


# standardized coefficients
# this tells us many standard deviations y is expected to change
# for a one-standard-deviation change in the predictor variable.
# see the online book for a longer discussion:
# https://bookdown.org/jgscott/DSGI/regression.html#standardized-coefficients
standardize_parameters(lm_airbnb)


# ANOVA?
# start with R-squared

rsquared(lm_airbnb)
# now look at eta_squared

eta_squared(lm_airbnb, partial = FALSE)
# change order of terms
lm_airbnb_3 =
   