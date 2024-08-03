      ##################################################
      #####  Discrete Probability Models practice  #####
      ################################################## 
library(tidyverse)

# The file dallas_shelter.csv contains data on animal intake and adoptions 
# for Dallas's central animal shelter in fiscal year 2017. 
# Each row corresponds to a single animal taken in by the shelter. 
glimpse(dallas_shelter)

      ##### POISSON model #####
### What is the probability that the shelter will take in NO MORE THAN 
### 15 cats on a typical day in January?

# Start by counting the number of cats (animal_type == 'CAT') taken in
# by the shelter in January (month = 'JAN'); let's call this number N. 
N = dallas_shelter %>%
  filter(month == "JAN", animal_type == "CAT") %>%
  nrow()
   

dallas_shelter %>%
  filter(month == "JAN", animal_type == "CAT") %>%
  summarize(N = n())

                  # dallas_shelter %>% 
                  #    filter(month == 'JAN', animal_type == 'CAT') %>% 
                  #    summarize(N = n())  
                  #    # N = 383 

# Next, define R = N/31; divide N by 31 to calculate the average daily rate 
# of cat intakes for the month of January (with 31 days)
rate = N/31
                  # average January daily intake = 12.35 cats

# Suppose that we model daily cat intakes for the month of January as following 
# a Poisson distribution with rate 12.35 per day. Based on this assumption:  

# What is the probability that the shelter will take in NO MORE THAN 
# 15 cats on a typical day in January?
     ppois(15, rate)                               # P(cats <= 15)
   ## probability to the left
   # What is the probability that the shelter will take in MORE THAN 
   # 15 cats on a typical day in January?
                                    # P(cats > 15)
  1-   ppois(15, rate)  
  ppois(15, rate, lower.tail = FALSE)
  ## probability to the right exlcuding 15
  
      # What is the probability that the shelter will take in EXACTLY
      # 15 cats on a typical day in January?
                                    # P(cats = 15)

dpois(15, rate) %>% round(2)

ppois(14, rate, lower.tail = FALSE)
##includes 15

      ##### BINOMIAL model #####
### Suppose that today the shelter takes in 37 dogs that have microchips. 
### How many of these 37 dogs will be returned to their owners?
      
# Here we consider the chip_status variable, which indicates whether the animal 
# has a microchip (about the size of a grain of rice) embedded under its skin. 
# Many owners "chip" their pets in order to make them easier to trace if lost, 
# since vets and shelters can noninvasively scan the chips --- and, 
# unlike a traditional collar with an ID tag, the chip can't fall off.

# First filter the data set to include only dogs (animal_type=='DOG') that were 
# found to have microchips upon intake (chip_status=='SCAN CHIP'). 
CHIP_DOG = dallas_shelter %>%
  filter(animal_type == "DOG", chip_status == "SCAN CHIP")


   

# For these dogs with chips, calculate the proportion that were eventually 
# returned to their owners (outcome_type == 'RETURNED TO OWNER'). 
# Let's call this number p_return; you'll need it for the calculation below.

## make a table

   xtabs(~outcome_type, data = CHIP_DOG) %>%
     prop.table() %>%
     round(3)

   
   
# filter and summarize
                        # dallas_shelter %>% 
                        #    filter(animal_type=='DOG', chip_status=='SCAN CHIP') %>% 
                        #    summarize(sum(outcome_type == 'RETURNED TO OWNER')/n())
                        #                # p_return = 0.272
   
   dallas_shelter %>%
     filter(animal_type == "DOG", chip_status == "SCAN CHIP") %>%
     summarize(sum(outcome_type == "RETURNED TO OWNER")/ n()) 
   
# create a data frame with criteria above + also filtering on outcome_type 
                         RETURN_CHIP_DOG = dallas_shelter %>% 
                            filter(animal_type=='DOG', 
                                   chip_status=='SCAN CHIP', 
                                   outcome_type == 'RETURNED TO OWNER') 
   
# then, calculate proportion from the number of cases in the two data frames
p_return = nrow(RETURN_CHIP_DOG) / nrow(CHIP_DOG)
   

# Suppose that today the shelter takes in 37 dogs that have microchips. 
n = 37

# The shelter wonders how many of these 37 dogs will be returned to owners; 
# this number may be considered a Binomial random variable X. 
# Suppose we model X with a binomial distribution with 
# 'success' probability = p_return (calculated above = 0.272). 

# Under this model, what is the probability that AT MOST 10 
# of these n=37 dogs with chips will be returned to their owners?
                                          # P(X <= 10) = 0.58         
pbinom(10,n, p_return)
##to the left of 10
# What is the probability that MORE THAN 10 will be returned to their owners?
                                          # P(X > 10) = 0.42
                                          # P(X > 10) = 1-P(X <= 10) = 0.42
1 - .577
pbinom(10,n, p_return, lower.tail = FALSE)
# What is the probability that FEWER THAN 10 will be returned to their owners?
                                          # P(X < 10) = P(X <= 9) = 0.43
pbinom(9,n, p_return)

# What is the probability that AT LEAST 10 will be returned to their owners?
                                          # P(X >= 10) = 0.57
                                          # P(X >= 10) = 1-P(X <= 9) = 0.57
pbinom(9,n, p_return, lower.tail = FALSE)

      # We can also use the addition rule because the potential outcomes 
      # for binomial random variable X are mutually exclusive

      # P(X=10) + P(X=11)+ ... + P(x=36) + P(X=37)

sum(dbinom(10:37, n, p_return))