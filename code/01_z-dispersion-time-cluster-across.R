# model for dispersion z-score through time: omit controls
# here, data were standardised across studies (i.e., one standardisation for all data)

library(brms)
library(dplyr)
library(tidyr)

## the data
z.dispersion <- read.delim('/data/idiv_chase/sablowes/steph_micro/data/dispersions.zscores-across.txt',
                           sep = " ") %>% 
  as_tibble()

z.dispersion_time50 <- z.dispersion %>% 
  filter(Rank > 1 & Time < 50) %>% 
  mutate(# centre time for modelling
         cTime = Time - mean(Time)) %>% 
  group_by(Time_series) %>% 
  mutate(n_obs = n_distinct(cTime)) %>% 
  ungroup()  %>% 
  filter(n_obs > 1 )
  
# model with residual variation as a function of environment with study-level variation
zdisp_time_50_sigma2 <- brm(bf(nmodel.Zscores ~ Environment * cTime + (1 | Study) + (cTime | Study:Time_series),
                            sigma ~ 0 + Environment + (1 | Study)),
                         data = z.dispersion_time50,
                         control = list(adapt_delta = 0.99, max_treedepth = 12),
                         cores = 4, chains = 4)


save(zdisp_time_50_sigma2,
     file=Sys.getenv('OFILE'))
