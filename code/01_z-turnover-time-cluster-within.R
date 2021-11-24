# model for resilience through time: omit controls
# these data were standardised within studies

library(brms)
library(dplyr)
library(tidyr)

## the data
z.resilience <- read.delim('/data/idiv_chase/sablowes/steph_micro/data2/Resilience.zscores-within.txt',
                         sep = " ") %>% 
  as_tibble()

z.resilience_time50 <- z.resilience %>% 
  filter(Time < 50) %>% 
  mutate(cTime = Time - mean(Time)) %>% 
  group_by(Time_series) %>% 
  mutate(n_obs = n_distinct(Time)) %>% 
  ungroup() %>% 
  filter(n_obs > 1)

# incorporate environmental and study-level residual variation
z.resil_time_50_sigma <- brm(bf(nmodel.Zscores ~ Environment * cTime + (1 | Study) + (cTime | Study:Time_series),
                                sigma ~ 0 + Environment + (1 | Study)),
                         data = z.resilience_time50,
                         control = list(max_treedepth = 13, adapt_delta = 0.95),
                         cores = 4, chains = 4, iter = 3000, warmup = 1000)

save(z.resil_time_50_sigma, 
     file=Sys.getenv('OFILE'))
