new_olympics = olympics_top20 %>%
    filter(sex == "F" & sport == "Athletics") %>%
    group_by(height)
  
new_olympics

quantile(new_olympics$height, .95)

heavy_olympics = olympics_top20 %>%
  filter(sex == "M" & year == 2012) %>%
  group_by(event) %>%
  summarize ( count = n(), 
              med_weight = median (weight, na.rm = TRUE))

heavy_olympics

library(tidyverse)
data(diamonds)

diamonds_dancing = diamonds %>%
  group_by(color) %>%
  summarize ( count = n(), 
              dia_weight = median (carat, na.rm = TRUE))

ggplot (olympics_top20) +
  geom_line(aes(x = year, y = age)) +
  facet_wrap (~sex)

diamonds_dancing_expensive = diamonds %>%
  filter(price >= "10000") %>%
  summarize ( count = n(), 
              dia_expen = median (carat, na.rm = TRUE))

diamonds_dancing_ideal = diamonds %>%
  filter(cut == "Ideal") %>%
  summarize ( count = n(), 
              dia_ideal = iqr (price, na.rm = TRUE))

  women_olympics = olympics_top20 %>%
    filter(sex == "F") %>%
    group_by(event) %>%
    summarize ( count = n(), 
               sd_weight = sd (height, na.rm = TRUE)) %>%
    arrange (desc(sd_weight))

