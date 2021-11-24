# model for resilience through time: omit controls
# here, data were standardised across studies (i.e., one standardisation for all data)

library(brms)
library(dplyr)
library(tidyr)

## the data
resilience <- read.delim('/data/idiv_chase/sablowes/steph_micro/data/Resilience-across.txt',
                         sep = " ") %>% as_tibble()


resilience_time50 <- resilience %>% 
  filter(Time < 50) %>% 
  # 43 observations had complete turnover (dispersion = 1), nudge to 0.999
  # to facilitate Beta error distribution
  mutate(value = ifelse(value==1, 0.999, value),
         # centre time for modelling
         cTime = Time - mean(Time)) %>% 
  # time series check
  group_by(Time_series) %>% 
  mutate(n_obs = n_distinct(Time)) %>% 
  ungroup() %>% 
  filter(n_obs > 1)

# 
resil_time_50 <- brm(bf(value ~ Environment * Time + (Time | Study / Time_series)),
                     family = Beta(),
                     data = resilience_time50,
                     control = list(adapt_delta = 0.99, max_treedepth = 12),
                     cores = 4, chains = 40)

resil_time_50_phi2 <- brm(bf(value ~ Environment * Time + (Time | Study / Time_series),
                             phi ~ 1 + (1 | Study)),
                     family = Beta(),
                     data = resilience_time50,
                     control = list(adapt_delta = 0.99, max_treedepth = 12),
                     cores = 4, chains = 4)


save(resil_time_50, 
     resil_time_50_phi2, 
     file=Sys.getenv('OFILE'))