library(tidyverse)
library(mosaic)
library(moderndive)
library(effectsize)
library(rsample)
library(modelr)
options(scipen=999)

##1 - Redlining
lm1 = lm(policies ~ minority + fire + age + income, data = redlining)
get_regression_table(lm1)
standardize_parameters(lm1) 
ggplot(redlining) + 
  geom_point(aes(x = policies, y = minority)) +
  labs(title = "Relationship between policies(Count of FAIR policies)and miniorities(Percentage of self-identified minorities)",
       x = "Number of FAIR plan policies and renewals per 100 housing units",
       y = "Percentage of residents that identify as a racial or ethinic minority")

##2 - Capital Bikeshare

##Plot A
bikeshare2 = bikeshare %>%
  group_by(weekday, hour) %>%
  summarize(median_bikerent = median(total))

ggplot(bikeshare2) + 
  geom_boxplot(aes(x=weekday, y= median_bikerent)) +
  labs(
    x = "Day of the week",
    y = "Median Bike Rent",
    title = "Relationship between the day of week and median biken rent of bike rentals")

##Plot B

bikeshare1 = bikeshare %>%
  group_by(hour, workingday) %>%
  summarize(sd_bike = sd(total))


ggplot(bikeshare1) + 
  geom_line(aes(x = hour, y= sd_bike)) +
            facet_wrap(~workingday) + 
  labs(
       y = "Standard Deviation of Bike Rentals",
       x = "Hour of the Day",
       title = "Relationship between the hour of the day and standard deviation of bike rentals")
            
##Plot C
bikeshare_c = bikeshare %>%
  group_by(workingday, precipitation) %>%
  summarize(avg_total = mean(total))

ggplot(bikeshare_c, aes(x = precipitation, y = avg_total)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  facet_wrap(~workingday)+ 
  labs(
    x = "Precipitation of 4 different weather situations",
    y = "Average count of total bike rentals",
    title = "Relationship between the precipitation of weather situation and the average count of bike rentals")


##Plot D

ggplot(bikeshare) +
  geom_histogram(aes(x = total)) +
  facet_grid(season ~ .) + 
  labs(
    x = "Count of Total Bike Rentals",
    y = "Count",
    title = "Distribution of Total Bike Rentals over the 4 seasons")

#3 - Machine Learning
library(lubridate)


##Model 1

smallmodel = lm(children ~ market_segment + adults + customer_type + is_repeated_guest, data = hotels_train)
rmse(smallmodel, data = hotels_test)
rmse(smallmodel, data = hotels_train)
#Model 2

bigmodel = lm(children ~ (. - arrival_date), data= hotels_train)
rmse(bigmodel, data = hotels_test)
rmse(bigmodel, data = hotels_train)
##Model 3
hugemodel = lm(children ~ (. - arrival_date)^2, data= hotels_train)
rmse(hugemodel, data = hotels_test)
rmse(hugemodel, data = hotels_train)
##Model 4
library(lubridate)
hotels_train = hotels_train %>%
  mutate(month = month(ymd(arrival_date), label=TRUE))
hotels_test = hotels_test %>%
  mutate(month = month(ymd(arrival_date), label=TRUE))


bigmodel2 = lm(children ~ (. - arrival_date), data= hotels_train)
rmse(bigmodel2, data= hotels_test)
rmse(bigmodel2, data= hotels_train)



