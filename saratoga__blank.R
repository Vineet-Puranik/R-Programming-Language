      #####################################
      ## Isolating Partial Relationships ##
      #####################################
library(tidyverse)
library(mosaic)
library(effectsize)
library(moderndive)
      options(scipen=999)
      data("SaratogaHouses")
glimpse(SaratogaHouses)

# how much is a fireplace worth? # Start: a plot

ggplot(SaratogaHouses, aes(x = fireplaces, y = price))+
  geom_jitter(width = .2, alpha = .2) + 
  stat_summary(fun = "mean", size = 1, color = "blue") + 
  geom_smooth(method = "lm", se = FALSE, color = "firebrick")

# with group means and a regression line
      # ggplot(data=SaratogaHouses, aes(x=fireplaces, y=price)) + 
      #   geom_jitter(width=0.2, alpha=0.2, color = ) + 
      # 	stat_summary(fun = 'mean',  size=1, color = 'blue') + 
      # 	geom_smooth(method='lm', se=FALSE, color= 'firebrick')

# linear looks pretty good here:
# a straight line passes right through the group means
# (except for four-fireplace houses, of which there are two!)

# what's the slope of that line?
lm0 = lm(price ~ fireplaces, data = SaratogaHouses)
coef(lm0)
# naive answer: 66K per fireplace!
# if true: building more fireplaces = ridiculous ROI

# but it's probably not true!  this is an overall relationship
# we care about a partial relationship, i.e. holding other factors constant

## Think and Examine
# fireplaces is correlated with livingArea
ggplot(data=SaratogaHouses) +
  geom_jitter(aes(x=fireplaces, y= livingArea))
	
# ... which also predicts price
ggplot(data=SaratogaHouses) +
  geom_point(aes(x=livingArea, y= price))


# Estimate:
# let's build a model that incorporates/adjusts for the size of the house and the lot
lm1 = lm(price ~ livingArea + lotSize + fireplaces , data = SaratogaHouses)

get_regression_table(lm1)
   
# Refine:
# what about fuel system type?
# could be a confounder for fireplaces
# houses with gas heat seem to have more fireplaces on average

SaratogaHouses%>%
  group_by(fuel) %>%
  summarize(avg_fp = mean(fireplaces))


# so let's see what happens when we add fuel type to the model
lm2 = lm(price ~ livingArea + lotSize + fireplaces+ fuel , data = SaratogaHouses)
get_regression_table(lm2)



# So two things happen when we added fuel system to the model:
# 1) we saw there's a clear effect on price of fuel type, holding other vars constant 
#   (notice the confidence intervals: electric and oil heating -> lower price)
# 2) the estimated coefficient on the fireplaces variable got smaller:

   coef(lm1) %>% round(0)
   coef(lm2) %>% round(0)
   
# what happened was that some of the "fireplace" premium we estimated in lm1
# was actually a "gas heating" premium.  since houses with more fireplaces were
# more likely have gas heating, we wrongly attributed some of the "gas heating" 
# premium to fireplaces when we left fuel type out of the model

# Still refining
# What about age of the house? 
lm3 = lm(price ~ livingArea + lotSize + fireplaces+ fuel + age , data = SaratogaHouses)
get_regression_table(lm3)
   
   
# fireplace coefficient gets smaller again.
# AND we can confidently identify the sign of the age effect:
# older houses cost less, holding other variables constant

   
# so again we have a situation where we added a variable to the model,
# it has a definite effect on the outcome (see the confidence interval),
# and its inclusion affects the magnitude of the coefficient we care about 
# (in this case, fireplaces).  conclusion: we need age in the model

# standardized coefficients:

standardize_parameters(lm3)
