# model for dispersion through time: omit controls
# here, data were standardised across studies (i.e., one standardisation for all data)


library(brms)
library(dplyr)
library(tidyr)

## the data
dispersion <- read.delim('/data/idiv_chase/sablowes/steph_micro/data/dispersions-across.txt',
                         sep = " ") %>% 
  as_tibble() 

dispersion_time50 <- dispersion %>% 
  filter(Rank > 1 & Time < 50) %>% 
  mutate(value = ifelse(value==1, 0.999, value),
         # centre time for modelling
         cTime = Time - mean(Time)) %>% 
  # make sure we have time series
  group_by(Time_series) %>% 
  mutate(n_obs = n_distinct(Time)) %>% 
  ungroup() %>% 
  filter(n_obs > 1)

# 
disp_time_50 <- brm(bf(value ~ Environment * cTime + (1 | Study) + (cTime | Study:Time_series)),
                     family = Beta(),
                     data = dispersion_time50,
                     control = list(adapt_delta = 0.99, max_treedepth = 12),
                     cores = 4, chains = 4)

disp_time_50_phi2 <- brm(bf(value ~ Environment * cTime + (1 | Study) + (cTime | Study:Time_series),
                            phi ~ 1 + (1 | Study)),
                    family = Beta(),
                    data = dispersion_time50,
                    control = list(adapt_delta = 0.99, max_treedepth = 12),
                    cores = 4, chains = 4)

save(disp_time_50, 
     disp_time_50_phi2, file=Sys.getenv('OFILE'))