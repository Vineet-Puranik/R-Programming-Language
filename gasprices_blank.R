############################################
### bootstrapping differences and models ###  
############################################
library(tidyverse)
library(mosaic)
theme_set(theme_classic()) 

# Import Dataset: gasprices.csv
glimpse(gasprices)

##################################
### REVIEW: Summary Statistics ###

# what is the typical price of a gallon of gas? 
# we could calculate a mean or median 

# visualize the distribution of price per gallon
ggplot(gasprices) + 
  geom_histogram(aes(x=Price), binwidth = 0.01) 

favstats(gasprices$Price) %>%  round(2)

gasprices %>% 
  summarize(mean_ppg = mean(Price),
            median_ppg = median(Price), 
            sd_ppg = sd(Price), 
            IQR_ppg = IQR(Price)) %>% 
  round(2)

# summarize() is great for generating a report of summary stats
# but for bootstrapping we'll use the function(~variable) shortcut:
mean(~Price, data = gasprices)

mean_boot = do(10000) * mean(~Price, 
                             data = resample(gasprices))
head(mean_boot)

# visualize the SAMPLING distribution of sample means
ggplot(mean_boot) + 
  geom_histogram(aes(x=mean), color = "white")

# generate a 95% confidence interval for a mean:
confint(mean_boot)            # interpretation?

# what about bootstrapping a median? Yes, we can! 
median(~Price, data = gasprices)

median_boot = do(10000) * median(~Price, 
                                 data = resample(gasprices))

# visualize the SAMPLING distribution of sample medians
ggplot(median_boot) + 
  geom_histogram(aes(x=median), color = "white")

# distro looks different from mean, let's look at first few rows of raw data:
head(median_boot)

# generate a 95% confidence interval for a median:
confint(median_boot)          # interpretation?

# we can even bootstrap measures of spread, e.g., SD
sd(~Price, data = gasprices)

sd_boot = do(10000) * sd(~Price, 
                         data = resample(gasprices))

# visualize the SAMPLING distribution of sample standard deviations
ggplot(sd_boot) + 
  geom_histogram(aes(x=sd), color = "white") 

# generate a 95% confidence interval for sample standard deviation:
confint(sd_boot)              # interpretation?


###########################
### Difference in Means ###

# Are gasprices cheaper when there are competitors within sight?
# let's look at how the mean price differs across gas
# stations located near competitors VS not near competitors

### how many stations in our sample have competitors within sight?
xtabs(~Competitors, data = gasprices)

# let's use side-by-side boxplots to visualize the difference 
# in price WITH nearby competitors and WITHOUT nearby competitors
# 'width' option controls the extent of horizontal jitter
ggplot(gasprices, aes(x=Competitors, y=Price)) + 
  geom_boxplot() +   
  geom_jitter(width=0.2) 

# does it look like price differs across these groups? Let's put some numbers 
# on it. Here is the mean of Price by the categorical variable Competitors
#*
mean(Price~Competitors, data = gasprices)
# We could calculate this difference 'by hand'
# Y - N = 1.8524-1.8759 = -0.0235

# but the diffmean function can do it for us
# and we want to use diffmean for the bootstrap
# here we are asking RStudio to compare Price grouped by 
# whether or not competitors are located near the station
#*  
diffmean(Price~Competitors, data = gasprices)
# in our sample, stations near competitors charge about 2 cents less
# What might have happened if these data were collected over a 
# different time period? 
# is what we've observed merely a "small-sample difference"?

# Let's bootstrap this sample to understand the variability that we 
# expect to see in mean differences if resampling from population
price_boot = do(10000)* diffmean(Price~Competitors, data = resample(gasprices))
head(price_boot)
  

# Visualizing the sampling distribution of mean differences
ggplot(price_boot) + 
  geom_histogram(aes(x=diffmean), color = "white")

# A 95% confidence interval looks like:
#* 
confint(price_boot)

# interpretation? We can say with 95% confidence that gas stations 
# near competitors charge between 6 cents less to 1 cent more 
# relative to stations with no nearby competition
# can we rule out that there is no difference across groups? 


#################################
### Difference in Proportions ###

# Gas stations often invest in other features (e.g., car wash or store) 
# to attract customers. What proportion of stations in our sample have an 
# interior store or service area?
gasprices %>% 
  group_by(Interior) %>% 
  summarize(prop = n()/nrow(gasprices))

# or use the prop() function; here we get default level of NO interior
#*
#*
prop(~Interior, data = gasprices)

# what proportion of stations are located within line of sight of a competitor?
gasprices %>% 
  group_by(Competitors) %>% 
  summarize(prop = n()/nrow(gasprices))

# does it look like more stations have an interior when they are located near 
# competitors? let's visualize the difference between groups with a bar plot
# first group by Competitors variable and calculate the proportion of 
# stations with an interior in each 'Competitors' group
gasprices %>% 
  group_by(Interior, Competitors) %>% 
  summarize(props = n()/nrow(gasprices)) %>% 
  ggplot() +
  geom_col(aes(x=Competitors, y=props, fill=Interior), 
           position = 'dodge')

# Let's put some numbers on it. We can use the prop() function to 
# calculate proportions grouped by another categorical variable
#*
prop(Interior~Competitors, data = gasprices) %>% round(2)
# We could calculate this difference 'by hand'
# Yes - No = 0.04 - 0.20 = -0.16

# or use diffprop()
#*
diffprop(Interior~Competitors, data = gasprices) %>% round(2)

# remember, both proportions are for stations with NO interiors  
# in our sample, stations NOT near competitors are about 16% less 
# likely to offer a station interior. But wouldn't this be more intuitive
# if we set the default level of this variable to report the proportion 
# of stations that YES do have an interior?
glimpse(gasprices)

# you can specify the default level with the 'success' argument in the 
# prop() or diffprop() function
#*
diffprop(Interior~Competitors, success = "Yes", data = gasprices) %>% round(2)

# Here we are asking RStudio to compare the proportion of 
# stations with an interior grouped by whether competitors are near

# But is this estimate merely a "small-sample difference"? 
# What if we looked at a different sample of stations?
# Let's bootstrap this sample to understand the variability that we 
# expect to see in differences in proportions if resampling from population
interior_boot = do(10000)*diffprop(Interior~Competitors, success = "Yes", data = resample(gasprices))

  

head(interior_boot)

# Visualizing the sampling distribution of differences in group proportions
ggplot(interior_boot) + 
  geom_histogram(aes(x=diffprop), color = "white")

# A 95% confidence interval looks like:
confint(interior_boot)   

# interpretation? We can say with 95% confidence that gas stations 
# near competitors are between 3.6% to 28% more likely to have a station 
# interior relative to stations with no nearby competition


#########################################
###  Bootstrapping Regression Models  ###

# To what extent do gas prices vary by neighborhood demographics? Does the 
# median household income in the zip code where the station is located
# predict price per gallon?

# how can we summarize the distribution of numerical variable Income?
favstats(~Income, data = gasprices) %>%  round(2)


# visualize the distribution of Income
ggplot(gasprices) + 
  geom_histogram(aes(x=Income), 
                 color = 'maroon', fill = 'gray80')  + 
  labs(title = "Median income ($) in gas station zip code", 
       x = "", y = "count of stations") +
  scale_x_continuous(breaks=seq(0, 130000, 25000))

# visualize the association between gas price per gallon VS Income
ggplot(gasprices) + 
  geom_point(aes(x=Income, y = Price)) + 
  labs(title = "How do gas prices relate to local household median income?", 
       y = "price per gallon ($)") +
  scale_x_continuous(name = "Income ($)", breaks=seq(0, 130000, 25000))

# fit a linear model regressing Price on Income
#*
lm(Price ~ Income, data = gasprices)
# interpretation of slope? Is this an intuitive estimate 
# in the context of gas prices?

# adjust estimate to reflect the change in Price that we expect for 
# a 10k increase in Income
10000 * lm(Price ~ Income, data = gasprices)   %>% 
  coef()         # about 1 cent   

# But are these model estimates merely a "small-sample difference"? 
# What if we looked at a different sample of stations?

# let's create a variable with Income in units of $10000 to ease interpretation
gasprices = gasprices %>% 
  mutate(Income10k = Income/10000)
glimpse(gasprices)

favstats(~Income10k, data = gasprices) %>%  
  round(2)

# Let's bootstrap this sample to understand the variability that we 
# expect to see in differences in proportions if resampling from population
income_boot = do(10000)*lm(Price ~ Income10k, data = resample(gasprices))

head(income_boot)
  
# This code ^^^ says to: 
# 1) Create 10,000 bootstrap samples from original gasprices data frame
# 2) For each bootstrap sample, refit a lm() for Price versus Income10k 
# 3) Store the resulting set of 10000 fitted regression models 
#    in an object called income_boot.
head(income_boot) %>%  
  round(3)

# Visualizing the sampling distribution of Income10k slope estimates 
ggplot(income_boot) + 
  geom_histogram(aes(x=Income10k))

# what is standard error for bootstrap slope estimates?
#*
sd(~Income10k, data=income_boot)

# A 95% confidence interval looks like:
confint(income_boot)   

# easier to read output:
confint(income_boot) %>% 
  select(-method, -level) %>% 
  mutate_if(is.numeric, round, digits=3)

# interpretation? With 95% confidence, we expect an increase of between 
# 1 to 2 cents, approximately (0.007, 0.018), in price per gallon 
# for every ten-thousand dollar increase in median household 
# income in the station zip code. 
