      ##### Normal Model #####
library(tidyverse)
library(mosaic)

data(iris)
   # The famous "iris" data set contains measurements (centimeters) of lengths 
   # and widths of petals and sepals for 50 flowers from each of three species
glimpse(iris)

# optional base R shortcut for quick glance at variable distributions:
plot(iris)

# distribution of Sepal Width
                         ggplot(iris) +
                            geom_histogram(aes(x=Sepal.Width),
                                          binwidth = 0.2,
                                           fill = 'orchid2',
                                           color='floralwhite') +
                            labs(x="centimeters", 
                                 title="Sepal width of 150 iris flowers") + 
                            theme_classic()
   
favstats(~Sepal.Width, data=iris) %>% round(2)


# characterize the Normal model for sepal width with two parameters:
mean = mean(~Sepal.Width, data=iris)
stdev = sd(~Sepal.Width, data=iris)

rnorm(5, mean, stdev)
### Normal distribution functions:
      # rnorm(): for simulating normal outcomes
      # randomly generate sepal widths under this model for five flowers:

      # pnorm(): calculate cumulative probabilities for a range of outcomes
      ?pnorm

### use the Normal model to make probabilistic statements about sepal widths:

# What is the probability of observing an iris with sepal width of
# at most 2.6?
pnorm(2.6, mean, stdev)

# What is the probability of observing sepal width greater than 3.4?

                          # P(>3.4) = 1-P(<=3.4)
1 - pnorm(3.4, mean, stdev)
pnorm(3.4, mean, stdev, lower.tail = FALSE)
# What is the probability of observing sepal width 
# BETWEEN 2 and 3?
                pnorm(3, mean, stdev)  -   # P(<3)
                pnorm(2, mean, stdev)     # P(<2)

                pnorm(2, mean, stdev)  + pnorm(3 ,mean, stdev, lower.tail = F)    
# P(SW<3) - P(SW<2) = P(2<SW< 3)

                .448 - .008
