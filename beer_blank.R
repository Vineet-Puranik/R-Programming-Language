##### CLT with proportions and differences of means ###

library(tidyverse)
library(mosaic)

# Example 1: what fraction of beers on the market are IPAs?
head(beer)

# what fraction in this sample are IPAs?
  
prop(~IPA, data = beer)

# how big is the sample?
nrow(beer)

# confidence interval for this proportion based on the CLT

# step 1: plug and chug
# p = 0.206 and n = 344
# apply the formula for the standard error
se_prop = sqrt(.206*(1-.206)/344)

# step 2:
# form the confidence interval
.206 - 2 *se_prop
.206 + 2*se_prop
# conclusion: the proportion of beers sold in Texas that are IPAS
# is somewhere between 0.16 and 0.25 with 95% confidence

# compare with prop.test: a shortcut
prop.test(~IPA, data = beer)

# compare with bootstrapping: nearly identical


# Example 2:
# how does price elasticity of demand (ped) differ for IPAs vs non-IPAs?
# Each beer has an estimated elasticity that comes from 
# historical sales data, so we don't have to fit the power law
# ourselves. we can just compare mean ped for IPAs and non-IPAs
# Ped shows change in demand for every 1% increase in price. 
# Ped of -2 means for 1% increase of price, demand falls by 2%.
ggplot(beer) + 
  geom_boxplot(aes(x = IPA, y = ped))

# looks like IPA consumers are more price sensitive
mean(ped~IPA, data = beer)
##first variable must be numerical or used to calculate the mean
##second variable is how you group it
# how much more? about 1.31 percent difference in elasticity
diffmean(ped~IPA, data = beer)


# What's our level of uncertainty about the difference
# in price sensitivity?
# Let's compute a confidence interval for the difference.
# We need these summary statistics for each group:

beer %>%
  group_by(IPA) %>%
  summarize (mu = mean(ped),
             sigma = sd(ped),
             n = n())
# std error formula:
# plug and chug into the big ugly formula

std_err_diff = sqrt((.606^2)/273 + (.623^2)/71)
std_err_diff
# our observe difference was 1.31, so our interval is:
1.31 -2 *std_err_diff
1.31 +2 *std_err_diff
# This kind of by-hand calculation gets so tedious so quickly
# that we almost never bother.  We wanted to show you the details
# for these examples so you see what's going on under the hood.

# Compare with t.test: nearly identical
# (not _exactly_ identical b/c t.test doesn't use _exactly_ 2
# as the multiplier on the standard error)

t.test(ped~IPA, data = beer)

# let's compare to the bootstrap:
boot_IPA = do(10000) * diffmean(ped~IPA, data = resample(beer))

confint(boot_IPA)
# yours will differ slightly due to Monte Carlo variability
# but it should be close to (-1.47, -1.15)
# again, nearly identical to the CLT-based result.
