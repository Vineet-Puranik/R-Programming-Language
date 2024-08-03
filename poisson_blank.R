      ##### Poisson Model #####
library(tidyverse)

# Example: Poisson for soccer scores. 
# The three key functions to know are:
#   - rpois: for simulating Poisson outcomes
#   - dpois: for calculating the probability of specific outcomes
#   - ppois: for calculating cumulative probabilities for a range of outcomes

# average goal-per-game rate
lambda_arsenal = 1.45

# rpois for simulating 38 games (one EPL season)

rpois(38, lambda_arsenal)

# dpois for calculating specific probabilities
dpois(0, lambda_arsenal)                           # chance of 0 goals
dpois(1, lambda_arsenal)                          # chance of 1 goal
dpois(2, lambda_arsenal)                         # chance of 2 goals

# ppois for calculating cumulative probabilities
ppois(2, lambda_arsenal)                         # P(X <= 2)
ppois(3, lambda_arsenal)                         # P(X <= 3)


# Simulate lots of games between Arsenal and Man City
# under a very simple Poisson model.
# the rates are chosen from average goal-scoring rates last season.
                         NMC = 100000
                         arsenal = rpois(NMC, 1.45)
                         ManCity = rpois(NMC, 2.18)

# Compile the results
xtabs(~arsenal + ManCity)
# Monte Carlo estimates of probabilities
                       
sum(arsenal>ManCity)/NMC

sum(arsenal==ManCity)/NMC

sum(arsenal<ManCity)/NMC


# Compare with a calculation from the PMF assuming independence
dpois(1,1.45) * dpois(1,2.18)                        # Arsenal 1 - 1 Man City
dpois(0,1.45) * dpois(2,2.18)                        # Arsenal 0 - 2 Man City

# See the book for more!
# https://bookdown.org/jgscott/DSGI/probability-models.html#example-modeling-the-score-in-a-soccer-game 