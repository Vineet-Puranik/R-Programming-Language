      ##################################
      ### P-values with the Patriots ###
      ##################################
library(tidyverse)
library(mosaic)
      
# In a 25-game stretch of the 2014-15 NFL seasons, 
# the New England Patriots won 19 coin tosses, for a winning 
# percentage of 76%. The Patriots have a reputation for 
# ethical lapses. Could they have cheated at the coin toss, too?

# we need to first rule out 'blind luck'...
# What does a random set of 25 coin flips look like?

# Use the nflip() function to simulate the number of wins
# in a set of 25 fair coin flips
nflip(n=25, prob = 0.5)

# now, repeat this simulation 10000 times and store the result
sim_flips = do(10000) * nflip(n=25, prob = 0.5)

sim_flips2 = do(10000) * nflip(n=52, prob = 0.33)
# The result is a data frame with one column called "nflip"
# Each entry is the result -- the count of 'wins' for 
# one of the 10000 simulations of flipping a coin 25 times
head(sim_flips)

# Visualize the distribution of results in a histogram

ggplot(sim_flips) +
  geom_histogram(aes(x = nflip), binwidth = 1)


# How many simulations yielded 19 wins or more?
      # You'll likely see a slightly different number
      # because of Monte Carlo variability

sum(sim_flips >=19)

sum(sim_flips2 >=11)
# What proportion of the simulations yielded 19 wins or more?


sum(sim_flips >=19)/10000

sum(sim_flips2 >=11)/10000
# this proportion is a p-value...
# interpretation? 

