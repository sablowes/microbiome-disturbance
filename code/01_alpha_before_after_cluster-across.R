# model for alpha diversity before and after disturbance
# here, data were standardised across studies (i.e., one standardisation for all data)

library(brms)
library(dplyr)
library(tidyr)

## the data
alpha <- read.delim('/data/idiv_chase/sablowes/steph_micro/data/Rich-across.txt',
                    sep = " ") %>% 
  as_tibble() %>%
  mutate(before_after = ifelse(Rank==1, 'before',
                               ifelse(Rank==2, 'after', NA))) %>% 
  # remove after observations that are beyond a 4 day threshold
  filter(Time_since_dist < 4 & Rank < 3) 

alpha$before_after <- factor(alpha$before_after,
                             levels = c('before', 'after'))


# some studies don't have a post-disturbance sample 'soon enough' following the perturbation
# to be useful for this question
studies2omit <- tibble(Study = c('Wu', 'Ho', 'Flancman', 'Jurburg_Portugal', 'Lu',  'Li2019'))

# remove the studies2omit, and two deVries time series that don't have matching before-after obs 
alpha <- alpha %>% 
  filter(!Study %in% studies2omit$Study) %>% 
  filter(Time_series!='deVries.Drought.mixLM' & Time_series!='deVries.Drought.mixRL')

# q = 0
alpha_q0_ba_nbinom_shape <- brm(bf(Observed ~ Environment * before_after + (1 | Study) + (before_after | Study:Time_series),
                                   shape ~ 1 + (1 | Study)),
                          family = negbinomial(),
                          data = alpha,
                          # control = list(adapt_delta = 0.99, max_treedepth = 12),
                          cores = 4, chains = 4)

save(alpha_q0_ba_nbinom_shape,
     file=Sys.getenv('OFILE'))