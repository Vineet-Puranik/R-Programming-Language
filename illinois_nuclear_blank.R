      ###############################################
      ##### P-values with Illinois Nuclear case #####
      ###############################################
library(mosaic)
library(tidyverse)

# Remember we can use nflip to simulate 
# binary outcomes such as a coin flip:
nflip(n=25, prob=0.5)

# We'll use this approach to model 80515 flips of a metaphorical 
# "coin" that comes up heads 4.7 times per 10,000 flips (prob = 0.00047), 
# on average. This is the base rate of leukemia among Illinois children 
# living MORE than 30 miles from a nuclear plant. In our 'coin' 
# metaphor, each occurrence of "heads" corresponds to a case of cancer.
nflip(n = 80515, prob = .00047)
# run this line of code 4 or 5 times to see the variation we get:


# now let's repeat the simulation 10000 times and store the result.
# this takes awhile (20-30 seconds).
sim_cancer = do(10000) * nflip(n = 80515, prob = .00047)


# As before, the result is a data frame with one column called "nflip"
head(sim_cancer)

# Visualize the distribution of simulation results -- i.e., the number 
# of cancer cases in each set of 80515 

ggplot(sim_cancer) +
  geom_histogram(aes(x = nflip), binwidth = .5)

# How many simulations yield 47 cases of cancer or more, just by chance?
sum(sim_cancer>=47)/10000

# What proportion of total simulations yield 47 cases or more?


# interpretation of p-value?


#################################################################
##### Optional: a shortcut using the Binomial distribution

# the Binomial Model is a probability distribution
# that describes the results of binary outcomes with some given 
# probability of "success" (some event of interest)
sim_cancer_2 = rbinom(n = 10000, size = 80515, prob = 0.00047)
sum(sim_cancer_2 >= 47)/10000

# We'll work more with the Binomial distribution later in the semester
