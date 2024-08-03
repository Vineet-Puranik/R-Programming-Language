      ##### Analyzing Experimental Data #####
library(tidyverse)
library(mosaic)
theme_set(theme_bw())
options(scipen = 999)
      
# Import data: ResumeNames = read.csv('../data/ResumeNames.csv')

# Let's look at callback rates by the "assigned ethnicity" of the name
# recall: the names came from census data, with 18 names that were
# used disproportionately by those self-identifying as Black
# and 18 used disproportionately by those self-identifying as White.
# these are coded as "cauc" and "afam" in the data set and were 
# randomized to each fictitious resume

# how many resumes received a call back? 
ggplot(data = ResumeNames, mapping = aes(x=call)) +
   geom_bar()

# what proportion of resumes received a call back?
prop(~call, data= ResumeNames)


# what proportion of resumes received a call back
# in each ethnicity group?
ggplot(data = ResumeNames, mapping = aes(x=call, fill=ethnicity)) +
   facet_wrap(~ethnicity) +
   geom_bar(position = 'dodge', color = 'black') +
   scale_fill_manual(values=c('black', 'white')) +
   ggtitle('Resume callbacks by perceived race') +
   xlab('') +
   theme(legend.position = 'none')

# difference in proportions

prop(call~ethnicity, data = ResumeNames)
diffprop(call~ethnicity, data = ResumeNames)

# Interpretation:
# - 6.5% of resumes with ethnicity=afam received call backs
# - 9.7 call-back rate for resumes for ethnicity=cauc
# - difference is 3.2%

# the ratio of proportions? (this measure may be more informative 
# than the difference of proportions for a relatively rare event)
0.0965/0.0645
# interpretation: resumes randomized to receive a White name
# were 1.5 times as likely to receive a callback

# confidence interval via bootstrapping
resume_boot = do(1000) * diffprop(call~ethnicity, data = resample(ResumeNames))
                      
   
# let's look at a histogram of the bootstrapped difference
ggplot(resume_boot) +
   geom_histogram(aes(x=diffprop), 
                  color = "white", fill = 'black', bins = 30)

# just looking at the distribution it seems clear that the 
# differences are almost all positive 
# and indeed, a CI for the difference doesn't contain 0
confint(resume_boot)


   # cleaner confint() output:
   confint(resume_boot) %>% 
      select(-level, -method) %>% 
         mutate_if(is.numeric, round, digits=3)


# a confidence interval based on large-sample inference conveys the same 
# information, and gives us a p-value under the null hypothesis of no
# difference in callback proportions
prop.test(call~ethnicity, data= ResumeNames)
prop.test(call~gender, data= ResumeNames)

# Conclusion: our results present clear evidence of racial 
# discrimination in the 1990s job market
