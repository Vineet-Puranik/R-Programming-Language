      #########################################
      ###  Plotting and summarizing data    ###
      ###  Day 6                            ###
      #########################################

library(tidyverse)

##### LINE GRAPHS #####
# power_christmas2015.csv
glimpse(power_christmas2015) ##tells us how many rows and columns
      head (power_christmas2015)
# Basic line graph
ggplot (power_christmas2015) + 
  geom_line(aes(x = hour, y = ERCOT))
  labs (x = "hour of the day",  y = "megawatts,
    title = electricity demand in Texas on Christmas 2015")
      
  ggplot (power_christmas2015) + 
    geom_line(aes(x = hour, y = ERCOT))
  
# adding a title and axis labels

  rudolph = ggplot (power_christmas2015, aes(x = hour, y = ERCOT)) +
    geom_line()
    geom_point(color = "red")+
  labs (x = "hour of the day", y = "megawatts",
    title = "electricity demand in Texas on Christmas 2015")
      
      
      
      
# adding a second geometric layer: markers 
   # notice that we put the aes function in the top layer
   # these mappings apply to both geom_line() and geom_point()

      
      
    rudolph = ggplot (power_christmas2015, aes(x = hour, y = ERCOT)) +
      geom_line()
    geom_point(color = "red")+
      labs(x="hour of the day", y="megawatts",
            title = "Electricity demand in Texas on Christmas 2015") 
      
      
      
      
## OPTIONAL flair and formatting with themes ##
# adding a theme

    rudolph + theme_classic()
    rudolph + theme_fivethirtyeight() + 
      theme (plot.title=element+text(size = 12))
      
# cool optional package: GGTHEMES
install.packages('ggthemes')
library(ggthemes)

# now a larger selection when you browse for themes


# themes are totally awesome but not perfect... try FiveThirtyEight's theme


# sometimes we need to make adjustments after the theme, 
# e.g. here with axis text




##### HISTOGRAMS #####
# olympics_top20.csv
glimpse(olympics_top20)

# basic histogram

ggplot(olympics_top20)+
  geom_histogram(aes(x=age))

# basic histogram; better display of bins

ggplot(olympics_top20)+
  geom_histogram(aes(x=age),color="snow")

# better bin width?

ggplot(olympics_top20)+
  geom_histogram(aes(x=age),color="snow", binwidth = 1)

# density histogram -- add y variable mapping
ggplot(olympics_top20) +
   geom_histogram(aes(x=age, y=..density..), 
                  color = 'darkgreen', 
                  fill = 'gray90', 
                  binwidth = 1) +
   labs(title="Age distribution of Olympic athletes", 
        x="") +
   theme_minimal()

# faceted by sex

ggplot(olympics_top20) +
  geom_histogram(aes(x=age, y=..density..), 
                 color = 'darkgreen', 
                 fill = 'gray90', 
                 binwidth = 1) +
  labs(title="Age distribution of Olympic athletes", 
       x="") +
  theme_minimal() + 
  facet_wrap(~sex)


# Try these plots with another variable, e.g. height or weight
# R makes it easy for us to Find/Replace variable names

ggplot(olympics_top20) +
  geom_histogram(aes(x=height, y=..density..), 
                 color = 'darkgreen', 
                 fill = 'gray90', 
                 binwidth = 1) +
  labs(title="Height distribution of Olympic athletes", 
       x="") +
  theme_minimal() + 
  facet_wrap(~medal)

##### BAR PLOTS #####
# use geom_bar for simple counts
# counts of medals for each sports at Olympics

ggplot(olympics_top20) +
  geom_bar(aes(y=sport))

# now facet for gender

ggplot(olympics_top20) +
  geom_bar(aes(y=sport))+
  facet_wrap(~sex)

# show gender using color instead of facet 
## and use minimal theme

ggplot(olympics_top20) +
  geom_bar(aes(y=sport, fill=sex))
  

##### BOXPLOTS #####

# distribution of heights of Olympians

ggplot(olympics_top20) + 
  geom_boxplot(aes(y=height))+
  facet_wrap(~season)

# we are better off with a histogram here... ^^^
# boxplots are great for comparison of distributions ACROSS categories

# How do heights compare across summer vs. winter olympics?



# a faceted histogram is also useful here




# when we have many categories, boxplots may be better

# Which sports have the oldest athletes?  Which have the youngest?

ggplot(olympics_top20) + 
  geom_boxplot(aes(y=age, x = sport))+
  coord_flip()
  

# flip plot coordinates for readable labels and horizontal orientation





##### SUMMARIES for NUMERICAL DATA #####

## measures of central tendency (the "typical" value) ##
# calculating a MEAN



# other ways to calculate a mean:



# MEDIAN




# same stat, multiple variables
olympics_top20 %>% 
   summarise(
      avg_age = mean(age),
      avg_height = mean(height),
      avg_weight = mean(weight)) 

# multiple summary statistics
olympics_top20 %>% 
   summarise(
      mean = mean(age),
      std_dev = sd(age),
      median = median(age),
      q1 = quantile(age, 0.25),
      q3 = quantile(age, 0.75),
      IQR = q3-q1) 

olympics_top20 %>% 
   select(age) %>% 
   summary()

# install.packages('mosaic') 
library(mosaic)
# for my 'favorite' summary stats function favstats()



## measures of spread/dispersion/variation ##
# STANDARD DEVIATION



# interquartile range (IQR)



# range




