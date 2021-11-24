# model for z.dispersion before-after disturbance question
# these data were standardised within studies

library(brms)
library(dplyr)
library(tidyr)

## the data
z.dispersion <- read.delim('/data/idiv_chase/sablowes/steph_micro/data2/dispersions.zscores-within.txt',
                           sep = " ") %>% 
  as_tibble() 

# some studies don't have a post-disturbance sample 'soon enough' following the perturbation
# to be useful for this question
studies2omit <- tibble(Study = c('Wu', 'Ho', 'Flancman', 'Jurburg_Portugal', 'Kennedy', 'Lu'))

# add before_after covariate for modelling immediate response to disturbance
z.dispersion <- z.dispersion %>% 
  mutate(before_after = ifelse(Rank==1, 'before',
                               ifelse(Rank==2, 'after', NA)))

# set before as reference level (this'll make it the intercept, with after the slope)
z.dispersion$before_after <- factor(z.dispersion$before_after, levels = c('before', 'after'))

# prepare data for before-after only
ba_z.dispersion <- z.dispersion %>% 
  filter(!Study %in% studies2omit) %>% 
  filter(!is.na(before_after))

zdisp_before_after <- brm(bf(nmodel.Zscores ~ Environment * before_after + (1 | Study) + (before_after | Study:Time_series)),
                             data = ba_z.dispersion,
                          prior = c(prior(normal(0, 1), class = b),
                                    prior(normal(0,1), class = sd)),
                             control = list(adapt_delta = 0.99, max_treedepth = 12),
                             cores = 4, chains = 4)
 

# residual variation differs between environments, include in model
zdisp_before_after_sigma2 <- brm(bf(nmodel.Zscores ~ Environment * before_after + (1 | Study) + (before_after | Study:Time_series),
                                sigma ~ 0 + Environment + (1 | Study)),
                         data = ba_z.dispersion,
                         prior = c(prior(normal(0, 1), class = b),
                                   prior(normal(0,1), class = sd)),
                         control = list(adapt_delta = 0.99, max_treedepth = 12),
                         cores = 4, chains = 4)

save(zdisp_before_after,
     zdisp_before_after_sigma2,
     file=Sys.getenv('OFILE'))
