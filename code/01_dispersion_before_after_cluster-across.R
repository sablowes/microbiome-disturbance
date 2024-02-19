# model for dispersion before-after disturbance question
# here, data were standardised across studies (i.e., one standardisation for all data)

library(brms)
library(dplyr)
library(tidyr)

## the data
dispersion <- read.delim('/data/idiv_chase/sablowes/steph_micro/data/dispersions-across.txt',
     sep = " ") %>% 
  as_tibble()

# some studies don't have a post-disturbance sample 'soon enough' following the perturbation
# to be useful for this question
studies2omit <- tibble(Study = c('Wu', 'Ho', 'Flancman', 'Jurburg_Portugal', 
                                 'Kennedy', 'Lu'))

# add before_after covariate for modelling immediate response to disturbance
dispersion <- dispersion %>% 
  mutate(before_after = ifelse(Rank==1, 'before',
                               ifelse(Rank==2, 'after', NA)))

# set before as reference level (this'll make it the intercept, with after the slope)
dispersion$before_after <- factor(dispersion$before_after, 
                                  levels = c('before', 'after'))


disp_before_after_phi <- brm(bf(value ~ Environment * before_after + 
                                  (before_after | Study / Time_series),
                                phi ~ 1 + (1 | Study)),
                         family = Beta(),
                         data = dispersion %>% 
                           # reduce to before-after samples only
                           filter(Time < 4 & Rank < 3) %>% 
                           # remove studies that don't have observations we want 
                           filter(!Study %in% studies2omit$Study) %>% 
                           # four observations had complete turnover (dispersion = 1), nudge to 0.999,
                           mutate(value = ifelse(value==1, 0.999, value)),
                         control = list(adapt_delta = 0.99, max_treedepth = 12),
                         cores = 4, chains = 4)

save(disp_before_after_phi, file=Sys.getenv('OFILE'))