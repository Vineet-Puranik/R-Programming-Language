      ##### Binomial Model #####
library(tidyverse)

# Binomial distribution functions:
   # - rbinom: for simulating binomial outcomes
   # - dbinom: for calculating the probability of a specific outcome
   # - pbinom: for calculating cumulative probabilities for a range of outcomes

##### Example: binomial for airline no shows #####
# Parameters
p = 0.09      # this is the 'prob' argument for the binom functions^^
n = 140       # this is the 'size' argument for the binom functions^^

# simulate 5 random flights with rbinom()
rbinom(5, n , p)

# visualizing the entire PMF (probability mass function)
# use the tibble() function to create a data frame with two columns:
         # a range of outcomes for the number of no-shows
         # and their associated probabilities from the dbinom() function
airlines = tibble(k=0:30,
                  prob=dbinom(k, size=140, prob=0.09))
airlines

# visualize the probability distribution
ggplot(airlines) + 
   geom_col(aes(x=k, y=prob), fill = 'deepskyblue3') +
   scale_x_continuous("passengers", breaks=seq(0, 30, 1)) +
   scale_y_continuous("probability", breaks=seq(0, 0.12, 0.02)) +
      ggtitle("Passenger no-show outcomes and associated probabilities") + 
         theme_light()
   
# what is the expected value of this Binomial random variable? (i.e., 
# the number of no-shows) = n*p  
n*p 

# what is the standard deviation of this Binomial random variable?
# sqrt(n*p*(1-p))

(n*p*(1-p)) %>% sqrt()


   ##### key Binomial functions #####

# dbinom():  probability of a SPECIFIC outcome, e.g., 7 or 15?
# this is the probability mass function of the binomial, i.e. P(X = k)
# for various possible outcomes k
                     # 3.1% chance of 7 no shows
                     # 8.5% chance of 15 no shows

dbinom(7, n, p)
dbinom(15, n, p)
# pbinom(): probability of a RANGE of outcomes, e.g., more than 15 or at most 7? 
# cumulative tail probabilities, P(X <= k)
                     # 5.8% chance of 7 OR FEWER no shows
                     # 80.1% chance of 15 OR FEWER no shows
pbinom(7,n,p)
pbinom(15, n, p)
# Suppose you're 12th on the standby list.
# You need at least 12 passengers with seats to no-show.
# What is P(X >= 12)?

#  First, use pbinom to calculate P(X <= 11).

pbinom(11, n, p)
1 - .386783

# Now use the Negation Rule (from Day 3!)
# i.e., the fact that P(X >= 12) = 1-P(X <= 11)

pbinom(11, n, p, lower.tail = FALSE)

# Shortcut: you can also use lower.tail = FALSE
# This gives P(X > k) for a specific input value k



   ### What assumptions are we making when we use the Binomial model?
   ### Do these assumptions make sense in the context of flight no-shows?
