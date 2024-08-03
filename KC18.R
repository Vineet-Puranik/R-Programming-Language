
epa_cars = filter(epa_cars,
                  !Powertrain %in% c("Bifuel (CNG)", "Bifuel (LPG)", "CNG"))
epa_cars = epa_cars %>% filter(year=="2020")
lm0 = lm(cityMPG ~ Category + Powertrain, data = epa_cars)

##7
coef(lm0)

##8

82.65 + 27.39
