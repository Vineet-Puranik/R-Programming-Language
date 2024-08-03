      ##############################
      ###  more Data Wrangling   ###
      ###  September 20, 2022     ###
      ##############################

library(tidyverse)
library(mosaic)

# Import Dataset >> load the nycflights13.csv data set
glimpse(nycflights13)
     
      ### summarize() variables ###
# what is the average departure delay for NYC flights? 

nycflights13 %>%
  summarize(avg_delay = mean (dep_delay))
      
# why are we getting NA? because 8255 flights have missing (NA) values
                                    # nycflights13 %>% 
                                    #   summarize(favstats(dep_delay))
nycflights13  %>%
  summarize (favstats(dep_delay))
# we can add na.rm=TRUE to summaries to ignore these missing values
      # na.rm stands for "NA remove"

nycflights13 %>%
  summarize(avg_delay = mean (dep_delay, na.rm = TRUE)) 

# using the select() and summary() functions will also remove the NAs for you:
      nycflights13 %>%
        select (dep_delay) %>%
        summary()
      
      ### filter() to examine only certain cases in data frame ###
# what is average departure delay for flights out of LGA?
# select only rows for which origin variable == LGA

      nycflights13 %>%
        filter (origin == "LGA") %>%
        summarize(avg_delay = mean (dep_delay, na.rm = TRUE)) %>%
        round (2)
      
      ### group_by() one or more variables ###
## new data frame
      
      by_origin_monthly = nycflights13 %>%
  group_by(origin, month) %>%
        summarize ( count = n(), 
                    avg_delay = mean (dep_delay, na.rm = TRUE))
 
   
   
# view summaries based on subgroups created above ^^
View (by_origin_monthly)

# now let's make a bar plot
# factor tells R to treat month as a categorical variable,
# even though it's labeled with numbers
ggplot(by_origin_monthly) +
  geom_col(aes (x = factor(month), y = avg_delay)) +
  facet_wrap (~origin) +
  coord_flip()

# see if you like horizontal bars instead with coord_flip()


# all three airports worst in summer and winter holidays,
# best in the fall


      ### mutate() to create new variables from existing variables  ###
      ### arrange() to sort results based on one or more variables  ###
      ### in ascending or descending order                          ###

# create new 'gain' variable from two existing variables

nycflights13 = nycflights13 %>%
  mutate(gain = dep_delay - arr_delay)


# Histogram of gain variable
ggplot(nycflights13) + 
  geom_histogram(aes (x = gain), color = "light sky blue", binwidth = 5)



# which routes from NYC gained the most time in the air, on average?
# need na.rm=TRUE because of missing values

nycflights13 %>%
  group_by(dest) %>%
  summarize (count = n(),avg_gain = mean (gain, na.rm = TRUE)) %>%
  arrange (desc(avg_gain))



# create multiple new variables at once in the same mutate() 
# within same mutate() code we can refer to new variables just created
                                    # nycflights13 = nycflights13 %>% 
                                    #   mutate(
                                    #     gain = dep_delay - arr_delay,
                                    #     hours = air_time / 60,
                                    #     gain_per_hour = gain / hours
                                    #   )
nycflights13 = nycflights13 %>%
  mutate( hours = air_time / 60,
          gain_per_hour = gain/ hours)
# now which routes gained the most per hour?

nycflights13 %>%
  group_by(dest) %>%
  summarize (count = n(),avg_gain_per_hour = mean (gain_per_hour, na.rm = TRUE)) %>%
  arrange (desc(avg_gain_per_hour))

# scatter plot: for each flight out of LGA, plot avg distance (x) by 
  #avg departure delay (y) when delay is over 10 minutes 

d1 = nycflights13 %>%
  filter (origin == "LGA" & dep_delay > 10) %>%
  group_by(flight) %>%
  summarize (avg_delay = mean (dep_delay, na.rm = TRUE),avg_dist = mean (distance))
              
ggplot (d1) +
geom_point(aes(x = avg_dist, y = avg_delay)) +
  scale_x_log10()

# line graph: departure hr (x) by ave delay (y)
d2 = nycflights13 %>%
  group_by(hour) %>%
  summarize ( avg_delay = mean (dep_delay, na.rm = TRUE))

ggplot (d2) +
geom_line(aes (x = hour, y = avg_delay))

# rename and remove variables (columns)

nycflights13 = nycflights13 %>%
  mutate (air_time_hours = hours) %>%
  select (-hours, -time_hour)
