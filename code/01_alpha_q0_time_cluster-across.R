# model for alpha diversity through time
# here, data were standardised across studies (i.e., one standardisation for all data)


library(brms)
library(dplyr)
library(tidyr)

# the data
alpha <- read.delim('/data/idiv_chase/sablowes/steph_micro/data/Rich-across.txt',
                         sep = " ") %>% 
  as_tibble() 

# fit models to first 50 days following disturbance only
alpha_time50 <- alpha %>% 
  filter(Rank > 1 & Time_since_dist < 50) %>% 
  mutate(cTime = Time_since_dist - mean(Time_since_dist)) %>% 
  # check we've got time series:
  group_by(Time_series) %>% 
  mutate(n_obs = n_distinct(Time_since_dist)) %>% 
  ungroup() %>% 
  filter(n_obs > 1)

# q = 0
alpha_q0_time_50_nbinom2_shape <- brm(bf(Observed ~ Environment * cTime + (1 | Study) + (cTime | Study:Time_series),
                                        shape ~ 1 + (1 | Study)),
                               family = negbinomial(),
                               data = alpha_time50,
                               control = list(adapt_delta = 0.99, max_treedepth = 12),
                               cores = 4, chains = 4)


save(alpha_q0_time_50_nbinom2_shape,
     file=Sys.getenv('OFILE'))
