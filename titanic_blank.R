      ######################################
      ###  Data Wrangling and Bar Plots  ###
      ######################################

library(tidyverse)
      library(mosaic)

# Use the Import Dataset button to read in titanic.csv
glimpse(titanic)
      
   #############################
   ### 6.3 Summary Shortcuts ###
# for these shortcuts to work, you'll need the mosaic library
library(mosaic)

# calculate a mean
# what was the mean age of passengers on the titanic?

mean(~age,data = titanic)

# standard deviation

sd(~age, data=titanic)

# median

median(~age, data = titanic)

# IQR

iqr(~age, data = titanic)

# oldest passenger?

max(~age, data = titanic)

# youngest passenger?

min(~age, data=titanic)

# favstats() for a collection of summary stats

favstats(~age, data = titanic)

# favstats() for subgroups

favstats(age~sex,data =titanic)

# calculate group means
# what was the mean age of male passengers? Of female passengers?

mean(age~sex,data =titanic)


# calculate the difference of two group means
# How different was the mean age of male passengers vs female?

diffmean(age~sex,data =titanic)


# calculate a proportion
# what proportion of passengers did not survive?

prop(~survived, data = titanic)
1-.5917782
# calculate group proportions
# what proportion of male passengers did not survive? what % female passengers?

prop(survived~sex, data = titanic)


# calculate the difference of two group proportions
# Did the female survival rate differ from that of the male passengers? 

diffprop(survived~sex, data =titanic)

   ######################
   ### data wrangling ###

# Our basic contingency table of proportions, conditioning on the column variable
# (passenger class). Tables are great for summarizing categorical data...

xtabs(~survived + passengerClass, data = titanic)%>%
  prop.table(margin = 2)%>%
  round(2)

   # ...but xtabs isn't the best for making plots. To generate 
   # categorical data summaries for plotting, we use a different 
   # approach: pipe + group + summarize.

# Let's see an example of pipe/group/summarize
# We'll group by sex and summarize by
# counting the total passengers and the survivors.
   ## Two notes:
      # 1) n() is a tidyverse function that counts cases
      # 2) We use the double-equals sign (==) to test for equality
## == is filtering
titanic %>%
  group_by(sex) %>%
  summarize(tot_count = n(),
            avg_age = mean(age),
            surv_count = sum(survived == "yes"),
            surv_pct = surv_count/tot_count)

# We could also just skip the "intermediary" variables above and
# go straight to percentages:

titanic %>%
  group_by(sex) %>%
  summarize(avg_age = mean(age),
            surv_pct = sum(survived == "yes")/n())

# use filter() to focus on a subset of the rows.
   # here we look only at female passengers.
   # note we need quotes around the text string 'female'
   # (can be 'single quotes' or "double quotes")

titanic %>%
  filter(sex == "female") %>%
  group_by(passengerClass) %>%
  summarize(avg_age = mean(age),
            tot_count = n(),
            surv_pct = sum(survived == "yes")/n())


# You can filter using more than one criteria
  # look at only female 2nd and 3rd class passengers

titanic %>%
  filter(sex == "female" & passengerClass == "2nd" 
         | sex == "female" & passengerClass == "3rd" ) %>%
  group_by(passengerClass) %>%
  summarize(avg_age = mean(age),
            tot_count = n(),
            surv_pct = sum(survived == "yes")/n())

# create data frame with only children
# use filter() with the age variable

titanic_kids = titanic %>%
  filter(age < 18) %>%
  group_by(passengerClass) %>%
  summarize(tot_count = n(),
            surv_pct = sum(survived == "yes")/n())

view (titanic_kids)
# Using mutate to add a column
   # Let's add an indicator variable for adult or child
   # and overwrite our original data frame with the new, 
   # augmented data frame.

titanic = titanic %>%
  mutate( adult = ifelse(age >= 18, "18+", "under18"))

# after mutate, 'adult' variable is a new column in the data frame

glimpse (titanic)

# We can group by multiple variables

titanic %>%
  group_by(sex,adult,passengerClass) %>%
  summarize(surv_pct = sum(survived == "yes")/n())

# the result is a long-form table of summary statistics,
# one row for each newly formed subgroup (sex/adult/class)

# Let's order the table using arrange() by survival percentage. 
# First ascending. 

titanic %>%
  group_by(sex,adult,passengerClass) %>%
  summarize(surv_pct = sum(survived == "yes")/n()) %>%
  arrange(surv_pct)


# Next descending


titanic %>%
  group_by(sex,adult,passengerClass) %>%
  summarize(tot_count = n(),
            surv_pct = sum(survived == "yes")/n()) %>%
  arrange(desc(surv_pct))


########### INSTAPOLL ###########
# Q1: What is the name of the youngest male passenger who did 
  # not survive?

titanic %>%
 filter(sex == "male" & survived == "no" & adult == "under18") %>%
  arrange(age)
  
  head (tabl)



# Q2: By passenger class, what is the IQR of age for 
  # women who sailed the Titanic? 
  titanic %>%
  filter(sex == "female") %>%
    group_by(passengerClass) %>%
    summarize(age_iqr = iqr(age))
    
   #################
   ### bar plots ###

# Let's store our one-group summary in a data frame called d1

  d1 = titanic %>%
    group_by(sex)%>%
    summarize (surv_pct = sum(survived == "yes")/n())
  
  d1
# Now we can use d1 to make a barplot of survival percentage by sex.
# Use geom_col to make a barplot
ggplot(d1) + 
  geom_col(aes(x= sex, y= surv_pct ))

d2 = titanic %>%
  group_by(sex,adult)%>%
  summarize (surv_pct = sum(survived == "yes")/n())

d2

# This workflow is easy to generalize to make fancier plots.
# Here we we: 
#   1) group by sex and whether the passenger was an adult
#   2) summarize by calculating a survival percentage


# Now the payoff: we use our table of summary stats to make a bar plot.
# position = 'dodge' puts the bars side by side, rather than stacked

ggplot(d2) + 
  geom_col(aes(x= adult, y= surv_pct, fill = sex ),
           position = "dodge")

      # We can make two different comparisons from this plot:
      #   - survival vs sex, holding age constant
      #   - survival vs age, holding sex constant


# Now three conditioning variables!  Add a faceting layer:

      # We can make many different comparisons from this plot:
      #   - survival vs sex, holding age and class constant
      #   - survival vs passengerClass, holding age and sex constant
      #   - survival vs age, holding sex and passengerClass constant

d3 = titanic %>%
  group_by(sex,adult,passengerClass)%>%
  summarize (surv_pct = sum(survived == "yes")/n())

d3

# With some slightly cleaner labels
ggplot(data = d3) + 
  geom_col(mapping = aes(x=adult, y=surv_pct, fill=sex),
           position ='dodge') + 
  facet_wrap(~passengerClass) + 
  labs(title="Survival on the Titanic by passenger class", 
       y="Fraction surviving",
       x = "") +
   theme_minimal()

# an entirely different organization of the same information.
# this one makes comparisons across class for fixed sex and age more immediate
ggplot(data = d3) + 
  geom_col(mapping = aes(x=sex, y=surv_pct, fill=passengerClass),
           position ='dodge') + 
  facet_wrap(~adult)

   ### we also use bar plots for numerical variable summary statistics
   ### use means rather than proportions/counts as mapping to height of bar:

# Mean age by class and sex

d4 = titanic %>%
  group_by(sex, passengerClass) %>%
  summarize (avg_age = mean(age))

d4
# Now a bar plot of mean by age class and sex

ggplot(data = d4) + 
  geom_col(mapping = aes(x=passengerClass, y=avg_age, fill=sex),
           position ='dodge')

#################
### other plots ###

# Make a histogram for age of female passengers and facet by survived



# now filter by male


