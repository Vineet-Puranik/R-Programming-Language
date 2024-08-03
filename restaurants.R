   ##############################
   ###  Simple Linear Models  ###
   ##############################
library(tidyverse)
library(mosaic)
   
# Import Dataset: restaurants.csv
glimpse(restaurants)

# We'll focus on two variables:
    # Price = average price of dinner/drinks/tax/tip for one person
    # FoodScore = critics' rating out of 10

# simple scatter plot to visualize the association between price and rating
ggplot(data=restaurants) +
   geom_point(aes(x=FoodScore, y=Price))
  

# FIT a linear model for price versus food score
lm1 = lm(Price ~ FoodScore, 
         data = restaurants)


# add LINE of best fit to scatter plot 
ggplot(data=restaurants, aes(x=FoodScore, y=Price)) +
    geom_point() + 
    geom_smooth(method='lm') +
   theme_classic()
  
  
### make a PREDICTION using new data
    # Let's create a new data frame with FoodScore ratings for 
    # 3 restaurants for which we want to predict market price
  new_data = data.frame(FoodScore = c(6.5, 7.5, 9.0))
  predict(lm1, new_data)


### SUMMARIZE a trend (i.e., look at the model coefficients)
coef(lm1)  %>%  
   round(2)

  # interpretation of coefficients?
        # (Intercept)   FoodScore 
        #      -6.16        7.88 

  
###  make "FAIR" COMPARISONS on food ratings 
###  which adjust for price

# look at the model residuals (actual - predicted)
  resid(lm1)  %>%  round(2)

  
# plotting model residuals
  plot(resid(lm1) ~ FoodScore, data = restaurants)
  + abline(0, 0, col = "tomato1")
  
  
  
# What is the "best value" restaurant?
# which is the lowest residual?  i.e., actual price LESS than what model predicts
  min(resid(lm1))
  
  
# which restaurant has the lowest residual? 
# pipe residuals to which.min
  resid(lm1) %>% which.min

# ^ the lowest residual is on the 3rd row, and that restaurant is:
  restaurants[3,]


### decomposing VARIATION (and evaluating model FIT)
# calculate R-squared for the model
  rsquared(lm1)  %>% round(2)


# R-squared is also given in the "regression table" (more on this later):
  summary(lm1)
  
    #  interpretation of R^2 = 0.28?

# also use summary() to calculate RMSE ("residual standard error"), 
# RMSE is another measure of model fit given in y-variable units
    # interpretation of residual standard error = 23.39 ? 
  
# RMSE in summary is close to the standard deviation of the residuals. 
  sd(resid(lm1))  
  
    # The difference comes the "degrees of freedom" remaining after 
    # we fit the model (more on this idea later in the semester...)
  
