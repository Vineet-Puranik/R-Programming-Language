      ################################
      ##### LM with grouped data #####
      ################################
library(tidyverse)
library(mosaic)
options(scipen = 999)      
theme_set(theme_classic())

# Import Dataset >> kroger.csv
   # data on weekly cheese sales across 68 weeks 
   # at 11 different Kroger grocery stores
glimpse(kroger)

# Let's work with the DFW store alone;
# create a new data frame using filter:
kroger_dfw = kroger %>%
  filter(city == "Dallas")
   
   
# how do weekly cheese sales differ when a marketing display 
# (`disp`) is present in the DFW store?

# visualization of group differences
ggplot(kroger_dfw) + 
  geom_boxplot(aes(x = disp, y = vol))
   
   
   
# layering a jitter plot on boxplot (note aes() at first layer)
                     # ggplot(kroger_dfw, aes(x=disp, y=vol)) + 
                     #    geom_boxplot() +
                     #    geom_jitter(width=0.1) +         
                     #       xlab("in-store marketing display?") + 
                     #       ylab("") + 
                     #       ggtitle("Weekly cheese sales volume")
   
ggplot(kroger_dfw, aes(x = disp, y = vol)) +
  geom_boxplot()+
  geom_jitter(width = .1) + 
  xlab("in-store marketing display?") + 
  ylab("") + 
  ggtitle ("Weekly cheese sales volume by display")
# how does average sales volume differ when there is a display?

mean(vol~disp, data = kroger_dfw)

diffmean(vol~disp, data = kroger_dfw)

# we can express the same information about group differences 
# with a linear regression model in baseline/offset form

lm(vol~disp, data = kroger_dfw) %>%
  coef()

## no is the baseline, yes is the offset
##when there is a displauy we expect to sell 3236 packages of cheese - offset

# interpreting model coefficients
   # intercept = the baseline, 
         # average sales when no display is present
   # dispyes coefficient = the offset, 
         # difference between display weeks and non-display weeks

# how do we know that 'no' is the baseline and 'yes' 
# is the dummy variable? Make a table if you are not sure...
xtabs(~disp, data = kroger_dfw)
   # by default, R chooses the baseline to correspond to whatever level 
   # of the categorical variable comes first alphabetically. 
lm(vol~disp, data = kroger_dfw) %>%
  summary()
# why use a regression model when mean and diffmean give the same answer?
# because LM can be generalized! It works both with categorical and 
# numerical variables. Also, LM accommodates more than one predictor variable
# (and their interactions!). 
# Also, we gain insight from measures of model fit (R-squared and RSE).

kroger_boot = do(1000) * lm(vol~disp, data= resample(kroger_dfw))

confint(kroger_boot)
# bootstrapping for model confidence intervals:
kroger_boot =


# interpretation of interval? 

# Do our results indicate that the in-store marketing display 
# boosted cheese sales by this large amount? Not necessarily! 
# This relationship is confounded by price.

# Customers buy more cheese when it's cheaper...
                              # ggplot(kroger_dfw) + 
                              #   geom_point(aes(x=price, y=vol))

ggplot(kroger_dfw) + 
  geom_point(aes(x = price, y = vol))
# And stores priced cheese lower when there was a display!
                              # ggplot(kroger_dfw) + 
                              #   geom_boxplot(aes(x=disp, y=price)) + 
                              #    coord_flip()
ggplot(kroger_dfw) + 
  geom_boxplot(aes(x = disp, y = price)) + 
  coord_flip()
# So did people buy more cheese in the display weeks because of 
# the display, or because cheese was cheaper during those weeks?
   # Next week: models with numerical + categorical variables
   # we can use LM to assess the partial relationship between display 
   # and sales while statistically adjusting for price   



##########################################################
# Linear models can also incorporate categorical variables 
# with more than two levels, e.g. store (`city` variable)

   # which city will be the LM baseline (i.e., represented by the intercept)?
xtabs(~city, data = kroger)
# how does cheese sales volume vary across cities?
               # ggplot(kroger, aes(x=city, y=vol)) + 
               #    geom_boxplot() +
               #    geom_jitter(width=0.1, alpha=0.25) +
               #    ylab("") + xlab("") +
               #    ggtitle("Weekly cheese sales volume by city") +
               #    coord_flip()

ggplot(kroger, aes(x = city, y = vol)) + 
  geom_boxplot() + 
  geom_jitter(width = .1, alpha = .25)+
  ylab("") + 
  xlab("") + 
  ggtitle ("Weekly cheese sales volume by city") + 
  coord_flip()
# fitting a model to predict sales volume in terms of city and whether
# or not a marketing display is present in the store

lm(vol ~ disp +city, data = kroger)%>%
  summary()

   # what sales volume do we predict at the Houston store 
      # during a week with a display?  
4348.11 + 1348.46 + 4475.26 =10171
##baseline + offset + city offset
   # what sales volume do we predict at the Nashville store 
      # during a week without a display?
4348.11 + 0 -1711.27 = 2636.84
  ##baseline + city offset
Cincinatti
4348.11 + 750.91
# appreciate how lm() does the work for us in creating dummy variables
# for each (non-baseline) level of a categorical variable. 
# Alternative 'manual' approach to creating dummy variables:
               # kroger = kroger %>% 
               #    mutate(Atlanta = ifelse(city == 'Atlanta', 1, 0),
               #           Birmingham = ifelse(city == 'Birmingham', 1, 0),
               #           Cincinnati = ifelse(city == 'Cincinnati', 1, 0),
               #           Columbus = ifelse(city == 'Columbus', 1, 0),
               #           Dallas = ifelse(city == 'Dallas', 1, 0),
               #           Detroit = ifelse(city == 'Detroit', 1, 0),
               #           Houston = ifelse(city == 'Houston', 1, 0),
               #           Indianapolis = ifelse(city == 'Indianapolis', 1, 0),
               #           Louisville = ifelse(city == 'Louisville', 1, 0),
               #           Nashville = ifelse(city == 'Nashville', 1, 0),
               #           Roanoke = ifelse(city == 'Roanoke', 1, 0)
               #          )

kroger = kroger %>% 
      mutate(Atlanta = ifelse(city == 'Atlanta', 1, 0),
             Birmingham = ifelse(city == 'Birmingham', 1, 0),
             Cincinnati = ifelse(city == 'Cincinnati', 1, 0),
             Columbus = ifelse(city == 'Columbus', 1, 0),
             Dallas = ifelse(city == 'Dallas', 1, 0),
             Detroit = ifelse(city == 'Detroit', 1, 0),
             Houston = ifelse(city == 'Houston', 1, 0),
             Indianapolis = ifelse(city == 'Indianapolis', 1, 0),
             Louisville = ifelse(city == 'Louisville', 1, 0),
             Nashville = ifelse(city == 'Nashville', 1, 0),
             Roanoke = ifelse(city == 'Roanoke', 1, 0)
          )


# model with manually created dummy variables for all stores 
# (other than those in Atlanta, the baseline):
                lm(vol ~ disp + Birmingham + Cincinnati + Columbus + Dallas + Detroit + 
                      Houston + Indianapolis + Louisville + Nashville + Roanoke,
                   data=kroger) %>% 
                   summary()

# ^^ this model gets the same results as when we use the `city` variable:


#########################################################
# advanced/optional: we can specify the baseline city 
# with the relevel() function

   kroger = kroger %>%
                  mutate(city = relevel(as.factor(city), "Dallas"))

# notice now that Atlanta has a coefficient and Dallas does not (because DFW
# stores are represented by the baseline/intercept)

   lm(vol~disp+city, data = kroger) %>%
     summary()
   
# model with manually created dummy variables for all stores 
# (other than those in Dallas, the baseline):
               # lm(vol ~ disp + Atlanta + Birmingham + Cincinnati + Columbus + Detroit + 
               #       Houston + Indianapolis + Louisville + Nashville + Roanoke,
               #    data=kroger) %>% 
               #    summary()
