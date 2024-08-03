library (tidyverse)
library (mosaic)
lm2 = lm(creatclear ~ age, creatinine)
coef(lm2)
resid(lm2) %>%
  round(2)
plot (resid(lm2))
sd (resid(lm2))
ggplot (creatinine) +
  geom_point (aes (x = age, y = creatclear))

ggplot (creatinine, aes (x = age, y = creatclear)) +
  geom_point ()+
  geom_smooth(method = "lm") +
  theme_classic()

rsquared(lm2)  %>% round(2)