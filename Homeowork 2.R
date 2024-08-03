library(tidyverse)
library(mosaic)
##1
ggplot(players_15) + 
  geom_point(aes(x = physic, y = overall)) +
  labs (x = "Physique Score (Point Scale of 1 - 100)", y = "Overall Score (Point Scale of 1 - 100)",
        title = "Physique vs. Overall Score of Players in EA SPORTS FIFA 2020 Video Game ")
##2
capmetro_UT = capmetro_UT %>%
  mutate(day_of_week = factor(day_of_week,
                              levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         month = factor(month,
                        levels=c("Sep", "Oct", "Nov")))

capmetro_UT = capmetro_UT%>%
  group_by(hour_of_day, day_of_week, month) %>%
  mutate(avg_board = mean(boarding))

ggplot(capmetro_UT) +
  geom_line(aes(x = hour_of_day, y = avg_board, color = month)) +
  facet_wrap(~day_of_week) +
  labs (x = "Hour of the Day (Hours)", y = "Average Boarding (Number of People)",
        title = "Distribution of Average Boardings over hour of day, day of week, and month")
##3

##A
sclass2 = sclass %>%
  filter(trim == "550", year == "2015")

boot_price = do(10000)*median(~price, data= resample(sclass2))
confint(boot_price)

##B
sclass3 = sclass %>%
  filter(year == "2013" & price < 70000)

boot_price2 = do(10000)*mean(~mileage, data= resample(sclass3))
confint(boot_price2)

##C
sclass4 = sclass %>%
    mutate(ifBlack = ifelse (color == "Black", yes = "1", no = "0")) %>%
    filter (year == "2014")
          
boot_price3 = do(10000)*prop(~ifBlack, data= resample(sclass4))
  confint(boot_price3)
  
##4

  ##A
westcampus = lm(rent ~ sqft, data = (west_campus))
coef(westcampus)
  
  ##B
lm1 = lm(rent ~ sqft, data = (west_campus))
westcampus2 = do(10000)*lm(rent ~ sqft, data = resample(west_campus))
confint(westcampus2)

  ##C

rsquared(westcampus)

  ##D

sd(resid(westcampus))

  ##E

ggplot(westcampus,aes(x = sqft, y = rent)) +
  geom_point() + 
    geom_smooth(method = "lm") + 
   labs (x = "Square Foot (Square Feet)", y = "Rent (U.S. Dollars)",
        title = "Correlation between montly rent and square foot in west campus")
  
##5
prop(armfold ~ write_hand, data = armfolding)
diffprop(armfold ~ write_hand, data = armfolding)

boot_hands = do(10000)*diffprop(armfold ~ write_hand, data = resample(armfolding))

confint(boot_hands)
  
