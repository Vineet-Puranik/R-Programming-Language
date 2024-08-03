library(tidyverse)
library(mosaic)
xtabs(~animals,data=superbowl)
xtabs(~animals + celebrity,data=superbowl) %>%
  prop.table (margin=2)
xtabs(~celebrity,data=superbowl)
xtabs(~celebrity + patriotic,data=superbowl) %>%
  prop.table
xtabs(~danger,data=superbowl)
xtabs(~danger + funny,data=superbowl) %>%
  prop.table
xtabs(~patriotic,data=superbowl)
xtabs(~funny,data=superbowl)
ggplot (evals) + 
  geom_boxplot(aes(x = gender, y = eval))
 
 ggplot(evals) +
  geom_histogram(aes(x=eval, y=..density..)) + 
    facet_wrap(~tenure, nrow =2)
 
 ggplot (evals) + 
   geom_point(aes(x = age, y = eval))+
   labs (x = "age of instructor", y = "evaluations of students",
         title = "Evaluation of teachers by students")
 mean (~height, data = SW)
 sd (~height, data = SW)
 (66 -172.8675 )/36.29
 mean (~mass, data = SW)
 sd (~mass, data = SW)
 bigmac = bigmac %>%
   filter(dollar_ex > 0) %>%
   mutate(price_usd = local_price/dollar_ex)
 bigmac = bigmac %>% mutate(year = lubridate::year(date))
 avg_bigmac = bigmac %>%
   group_by(name, year) %>%
   summarise(avg_usd = mean(price_usd, na.rm=TRUE))
 ggplot(avg_bigmac) +
   geom_histogram(aes(x=avg_usd)) +
   labs (x = "average USD prices across each country", y = "Count",
         title = "Count of Average USD prices in countries")
 median (~avg_usd,data = avg_bigmac)
 iqr (~avg_usd,data = avg_bigmac)
   