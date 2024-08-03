library(tidyverse)
library(mosaic)

##5
  sim_residents = do(10000) * nflip(n=72, prob = 0.19)
  sum(sim_residents>=9)/10000
##.1000

##6
sim_coin = do(100000) * nflip(n=150, prob = 0.75)
sum(sim_coin <=112)/100000
sum(sim_coin >=120)/100000
sum(sim_coin <=100)/100000
sum(sim_coin >=105)/100000

#7
sim_phones = do(100000) * nflip(n=500, prob = 0.006)
sum(sim_phones >=7)/100000

##We'd expect 130 of every 1,000 accounts to make at least 20 purchases in a month. 



