library(tidyverse)
library(mosaic)

superbowl = superbowl %>%
  filter(!is.na(view_count), 
         !is.na(like_count))

superbowlnew = superbowl %>%
  filter(animals == "TRUE") %>%
  summarize ( count = n(), 
              med_count = median (like_count, na.rm = TRUE))

superbowlbrand = superbowl %>%
  group_by(brand) %>%
  summarize ( count = n(), 
  avg_view_count = mean (view_count, na.rm = TRUE)) %>%
  arrange (desc(avg_view_count))

billboardnew = billboard %>%
  group_by(song_id) %>%
  summarize ( count = n(), 
              max_time_spent = max (weeks_on_chart, na.rm = TRUE)) %>%
  arrange (desc(max_time_spent))

yearlycounts = billboard %>%
  group_by(year, song, performer) %>%
  summarize(n = n()) %>%
  group_by(year) %>% 
  summarize(n=n())

ggplot(yearlycounts) +
  geom_line(aes(x = year, y = n), color = "red") +
  labs (title = "year vs count")


nycflights13new = nycflights13 %>%
  filter(dest == "SAT" & year == "2013") %>%
  group_by(carrier) %>%
  summarize ( count = n())

nycflights13long = nycflights13 %>%
  filter(distance>2500 & year == "2013") %>%
  group_by(origin) %>%
  summarize ( count = n())

