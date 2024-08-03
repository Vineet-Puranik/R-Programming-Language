#####################################
##### Grouped & Numerical Data  #####
#####################################
library(tidyverse)
library(mosaic)
theme_set(theme_minimal())

# house = read.csv('../data/house.csv')
ggplot(house) + 
  geom_point(aes(x = sqft, y = price, color = nbhd))

# let's look at a scatter plot of price versus size
# notice we have three separate neighborhoods here

      
      
# if we ignore neighborhood differences, we get a slope of $70/sqft:
lm0 = lm(price~sqft, data= house)
   coef(lm0) %>% round(0)

      -10091 + 70*1300
# if we account for neighborhood differences, we get a slope of $46/sqft:
lm1 = lm(price~sqft + nbhd, data= house)
   coef(lm1) %>% round(0)
   21241 + 46 * 2000
   21241 + 45 * 2000 + 10569 - for neighborhood 2 
   21241 + 46 * 1912 + 41535
# it seems like a paradox: you arrive at one answer by grouping on 
# neighborhood and another answer if you do not group
# Here neighborhood is a CONFOUNDER for the relationship
# between sqft and price because nbhd is correlated with both X and Y  
   # e.g. nbhd3 has the largest, most expensive houses
         # house %>%
         #   group_by(nbhd) %>%
         #   summarize(mean_size = mean(sqft),
         #             mean_price = mean(price))

   house %>%
     group_by(nbhd) %>%
     summarize(avg_size = mean(sqft), avg_price = mean(price))
              
   library(scales)
               # # house price by neighborhood
                ggplot(house, aes(x=nbhd, y=price)) + 
                   geom_boxplot() +
                   geom_jitter(aes(color = nbhd), alpha = 0.4) + 
                   theme(legend.position="none") +
                   ggtitle("House prices by neighborhood") +
                   scale_x_discrete(name = "") + 
                  scale_y_continuous(name = "", labels = dollar,
                                     breaks = seq(0, 300000, 25000))
               
               # # square footage by neighborhood
                ggplot(house) + 
                 geom_histogram(aes(x=sqft, color=nbhd)) + 
                   facet_wrap(~nbhd, nrow=3) +
                  theme(legend.position="none") + 
                   labs(title ="House square footage by neighborhood",
                        x = "",
                        y = 'count of houses') +
                   scale_x_continuous(breaks = seq(1000, 3000, 200))

