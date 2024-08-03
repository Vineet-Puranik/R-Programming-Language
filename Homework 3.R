library(tidyverse)
library(mosaic)
library(moderndive)
##1 Sam's the Man!
sim_football = do(100000) * nflip(n=56, prob = 0.68)
sum(sim_football>=44)/100000
ggplot(sim_football) + 
  geom_histogram(aes(x = nflip)) +
  labs(x = "Number of successful passes (nflip)",
       y = "Counts",
       title = "The Probability Distribution of the Test Statistic")

##2 Ebay experiment
mean(rev_ratio ~ adwords, data = ebay)
diffmean(rev_ratio ~ adwords, data = ebay)
ebay_exper = do(10000)*diffmean(rev_ratio ~ adwords, data  = resample(ebay))
confint(ebay_exper)
ggplot(ebay) + 
  geom_boxplot(aes(y= rev_ratio, x = adwords)) + 
  labs(x = "Type of Group (adwords)",
       y = "Revenue Ratio",
       title = "The Distribution of Revenue Ratio for Control vs Treatment Groups")

##3 - Manufacturing flaws in circuit boards

#Part A
##1
ggplot(circuit_boards) + 
  geom_boxplot(aes(x = opening, y = skips)) + 
  labs(y = "Number of Solder Skips",
        x = "Size of Opening in Solder Gun",
         title  = "Distribution of skips based on opening size of the solder gun",
       caption = "L, M, and S coordinate to Large, Medium. and Small")
##2
ggplot(circuit_boards) + 
  geom_histogram(aes(x = skips)) + 
  facet_wrap(~alloy) + 
  labs(x = "Number of Solder Skips",
       y = "Count",
       title  = "Distribution of the frequency of skips faceted by thickness of alloy",
       caption = "There are two types of alloy thickness: Thin and Thick")

lm1 = lm(skips ~ opening + alloy + opening:alloy, data = circuit_boards)
coef(lm1) %>% round(3)
confint(lm1)
get_regression_table(lm1)

#4 - Summer is Coming

lm3 = lm(power ~ temperature + weekday + temperature:weekday, data = ERCOT)
coef(lm3) %>% round(0)
get_regression_table(lm3)
rsquared(lm3)
  
ggplot(ERCOT) + 
  geom_point(aes(x = temperature, y = power)) + 
  facet_wrap(~weekday)+ 
  labs(x = "Temperature in degrees Fahrenheit",
       y = "Power Consumption in Megawatts",
       title  = "Relationship between power consumption and
temperature by Weekend vs Weekday")
