      ###############################################################      
      ### LM with Grouped & Numerical Variables (and Interaction) ###
      ###############################################################      
library(tidyverse)
library(mosaic)

# sclass2011 = read.csv('../data/sclass2011.csv')
# ### CHECK HEADER ROW ON IMPORT DATASET

      
# visualize the relationship between mileage and price
ggplot(sclass2011, aes(x=miles1K, y=price)) + 
   geom_point() + 
   geom_smooth(method='lm') 
 
# change point color by trim 

ggplot(sclass2011) + 
  geom_point(aes(x=miles1K, y=price, color = trim, alpha = 0.25)) + 
  geom_smooth(aes(x = miles1K, y = price),method='lm') 

      
# estimation: how does how price change with mileage?

lm(price ~ miles1K + trim, data = sclass2011) %>% coef() %>% round(0)

   # interpretation:
   # - S550 with 0 miles costs $61526 on average
   # - 63 AMGs cost about $24327 more than that
   # - price goes down about $322 for each addition 1K miles on the odometer
   # - this mileage coefficient is common to both models BY ASSUMPTION

# But we don't think that's right!
# the 63 AMG line should be steeper: higher "penalty" for more miles
# i.e. the effect of mileage on price depends on the trim
# here's a plot that shows this effect:
                      ggplot(sclass2011, aes(x=miles1K, y=price)) + 
                         geom_point(color = 'gray40') + 
                        facet_wrap(~trim) + 
                        geom_smooth(method='lm') 

61526  + -322 * 68.2 + 24327
# let's build a model that incorporates this effect, 
# via an interaction
lm1 = lm(price ~ miles1K + trim + miles1K: trim, data = sclass2011)
coef(lm1) %>% round(0)

## price = 61,123 - 311*m + 29571*a - 183*m*a
## price = 61123 + 29571 * a - 494 * m
   61123 -311*12.3+29571-183*12.3
  price given that for a 63 amg with 12.3 k miles
# we're 95% confident that the sign of the interaction 
# term is negative, so it makes sense to keep 
# the interaction in the model because it seems to be 
# a real effect
    confint(lm1)
# look at model summary
summary(lm1)

# focus on residual standard error
