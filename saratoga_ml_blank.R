         ##### Machine Learning -- Day 26 #####
library(tidyverse)
library(modelr) # for common evaluation metrics
library(rsample)  # for creating train/test splits
library(mosaic)

# read in SaratogaHouses.csv

# This "set.seed" function fixes the random-number seed.
# This functions as a "starting place" for Monte Carlo simulation should 
# we want to replicate the analysis
set.seed(1720101424)

# Randomly split the data into a training set and a testing set
# prop = 0.75 says to include 75% of the data in the training set.
# The remaining 25% of the data will be used to measure performance.
# It may not be obvious why we're doing this yet!
saratoga_split =  initial_split(SaratogaHouses, prop = .75)

saratoga_train = training(saratoga_split)

saratoga_test  = testing(saratoga_split)


##### Model training: fit the model only to the training set

# small model fit to the training set
lm_small = lm(price ~ livingArea + lotSize + fireplaces,
              data = saratoga_train)
   
  coef(lm_small) %>% round (0)
# medium model: 11 main effects
# Sometimes it's easier to name the variables we want to leave out
# the dot (.) means "all variables not otherwise named" (use this carefully)
# the minus (-) means "exclude this variable"
lm_medium = lm(price ~ . - sewer - newConstruction ,
               data = saratoga_train)
            
coef(lm_medium) %>% round (0)

   
# big model: all variables + their interactions
# the ()^2 says "include all pairwise interactions among vars in ()"
lm_big = lm(price ~ (.)^2,
            data = saratoga_train)

coef(lm_medium) %>% round (0)

# Compare size/complexity of models: number of free parameters
# in the big model, most of these are coefficients on interactions.

   length(coef(lm_small))
   length(coef(lm_medium))
   length(coef(lm_big))
   
   
##### Evaluate model performance on unseen testing set
   # average error (RMSE) on the training data, 
   # i.e. the model used to fit the data.
   # looks like the big model has lowest error.
   # the units here are dollars -- easy to interpret!

rmse(lm_small, saratoga_train )
rmse(lm_medium, saratoga_train )
rmse(lm_big, saratoga_train )



# but what about on the unseen testing data?
# Very different story!!
# large model gets crushed

rmse(lm_small, saratoga_test)
rmse(lm_medium, saratoga_test )
rmse(lm_big, saratoga_test)



# What happened here?  Was it a statistical fluke?
# I.e. maybe we just randomly split the data into training and testing
# sets where the big model happened to do worse, just by chance.

# So let's repeat this whole pipeline many times in a do loop.
            error_sim = do(25)*{
               
               # re-do the train/test split
               saratoga_split =  initial_split(SaratogaHouses, prop=0.75)
               saratoga_train = training(saratoga_split)
               saratoga_test  = testing(saratoga_split)
               
            #   # fit our three models to the training data
               lm_small = lm(price ~ livingArea + lotSize + fireplaces, 
                               data=saratoga_train)
               lm_medium = lm(price ~ . - sewer - newConstruction, 
                               data=saratoga_train)
               lm_big = lm(price ~ (.)^2, 
                              data=saratoga_train)
              
            #   # collect the model errors in a single output
               model_errors = c(rmse(lm_small, saratoga_test),
                                rmse(lm_medium, saratoga_test),
                                rmse(lm_big, saratoga_test))
            #   
            #   # return the model errors from the do loop
               model_errors
             }

# here are the 25 train/test splits
# the three columns are the small/medium/big models
             error_sim

# what about average performance across all train/test splits?
# medium model should have lowest average RMSE; the large model clearly loses
             colMeans(error_sim)

# So we've observed two things here:
   # 1) In-sample performance (on the training data) does not necessarily
      # generalize to out-of-sample performance (on the testing data)
   # 2) It's possible to include too many variables and interactions!
      # i.e. throwing in more information might actually reduce the 
      # performance of the model

# Why? It's called "overfitting"...
