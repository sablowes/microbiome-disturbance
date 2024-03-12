
source('~/Dropbox/1current/microbiome-disturbance/code/00_init_dirs_load_packages.R')

load(paste0(path2wd, 'model-fits-across/alpha_time_q0-7612517.Rdata'))

seed = 123

alpha_t50_dat <- alpha_q0_time_50_nbinom2_shape$data %>% 
  as_tibble() 

# aquatic effect sizes from before-after model
aquatic_t50_ES <- alpha_q0_time_50_nbinom2_shape %>% 
  spread_draws(b_cTime, `r_Study:Time_series`[Time_series, term]) %>% 
  filter(term=='cTime') %>% 
  ungroup() %>% 
  mutate(Environment = 'Aquatic',
         pop_value = b_cTime,
         ts_value = b_cTime + `r_Study:Time_series`) %>% 
  select(.chain, .iteration, .draw, Environment, Time_series, 
         pop_value, ts_value) %>% 
  separate(Time_series, into = c('Study', 'Time_series'), extra = 'merge') %>% 
  # fix 10 names that are screwing up my code
  mutate(Time_series = ifelse(Time_series=='Microcosm_Jurburg_Microcosm.1Heat.Soil',
                              'Jurburg_Microcosm.1Heat.Soil', Time_series),
         Time_series = ifelse(Time_series=='Microcosm_Jurburg_Microcosm.2Heat.Soil',
                              'Jurburg_Microcosm.2Heat.Soil', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Cip_C.diff.Pig_feces',
                              'Jurburg_Pigs.Cip_C.diff.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Cip.Pig_feces', 
                              'Jurburg_Pigs.Cip.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Clin_C.diff.Pig_feces',
                              'Jurburg_Pigs.Clin_C.diff.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Clin.Pig_feces', 
                              'Jurburg_Pigs.Clin.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Drought.Grazed',
                              'Jurburg_Portugal.Drought.Grazed', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Drought.Pristine',
                              'Jurburg_Portugal.Drought.Pristine', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Flood.Grazed', 
                              'Jurburg_Portugal.Flood.Grazed', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Flood.Pristine',
                              'Jurburg_Portugal.Flood.Pristine', Time_series),
         # and now fix study name
         Study = ifelse(Time_series=='Jurburg_Microcosm.1Heat.Soil' |
                          Time_series=='Jurburg_Microcosm.2Heat.Soil',
                        'Jurburg_Microcosm', Study),
         Study = ifelse(Time_series=='Jurburg_Pigs.Cip_C.diff.Pig_feces' |
                          Time_series=='Jurburg_Pigs.Cip.Pig_feces' | 
                          Time_series=='Jurburg_Pigs.Clin_C.diff.Pig_feces' | 
                          Time_series=='Jurburg_Pigs.Clin.Pig_feces', 
                        'Jurburg_Pigs', Study),
         Study = ifelse(Time_series=='Jurburg_Portugal.Drought.Grazed' |
                          Time_series=='Jurburg_Portugal.Drought.Pristine' | 
                          Time_series=='Jurburg_Portugal.Flood.Grazed' | 
                          Time_series=='Jurburg_Portugal.Flood.Pristine', 
                        'Jurburg_Portugal', Study)) 

# mammal 
mammal_t50_ES <- alpha_q0_time_50_nbinom2_shape %>% 
  spread_draws(b_cTime, `b_EnvironmentMammal:cTime`,
               `r_Study:Time_series`[Time_series, term]) %>% 
  filter(term=='cTime') %>% 
  ungroup() %>% 
  mutate(Environment = 'Mammal',
         pop_value = b_cTime + `b_EnvironmentMammal:cTime`, 
         ts_value = b_cTime + `b_EnvironmentMammal:cTime` +
           `r_Study:Time_series`) %>% 
  select(.chain, .iteration, .draw, Environment, Time_series,
         pop_value, ts_value) %>% 
  separate(Time_series, into = c('Study', 'Time_series'), extra = 'merge') %>% 
  # fix 10 names that are screwing up my code
  mutate(Time_series = ifelse(Time_series=='Microcosm_Jurburg_Microcosm.1Heat.Soil',
                              'Jurburg_Microcosm.1Heat.Soil', Time_series),
         Time_series = ifelse(Time_series=='Microcosm_Jurburg_Microcosm.2Heat.Soil',
                              'Jurburg_Microcosm.2Heat.Soil', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Cip_C.diff.Pig_feces',
                              'Jurburg_Pigs.Cip_C.diff.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Cip.Pig_feces',
                              'Jurburg_Pigs.Cip.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Clin_C.diff.Pig_feces',
                              'Jurburg_Pigs.Clin_C.diff.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Clin.Pig_feces',
                              'Jurburg_Pigs.Clin.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Drought.Grazed',
                              'Jurburg_Portugal.Drought.Grazed', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Drought.Pristine',
                              'Jurburg_Portugal.Drought.Pristine', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Flood.Grazed',
                              'Jurburg_Portugal.Flood.Grazed', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Flood.Pristine',
                              'Jurburg_Portugal.Flood.Pristine', Time_series),
         # and now fix study name
         Study = ifelse(Time_series=='Jurburg_Microcosm.1Heat.Soil' | 
                          Time_series=='Jurburg_Microcosm.2Heat.Soil',
                        'Jurburg_Microcosm', Study),
         Study = ifelse(Time_series=='Jurburg_Pigs.Cip_C.diff.Pig_feces' |
                          Time_series=='Jurburg_Pigs.Cip.Pig_feces' | 
                          Time_series=='Jurburg_Pigs.Clin_C.diff.Pig_feces' |
                          Time_series=='Jurburg_Pigs.Clin.Pig_feces',
                        'Jurburg_Pigs', Study),
         Study = ifelse(Time_series=='Jurburg_Portugal.Drought.Grazed' |
                          Time_series=='Jurburg_Portugal.Drought.Pristine' | 
                          Time_series=='Jurburg_Portugal.Flood.Grazed' |
                          Time_series=='Jurburg_Portugal.Flood.Pristine', 
                        'Jurburg_Portugal', Study)) 

# soil 
soil_t50_ES <- alpha_q0_time_50_nbinom2_shape %>% 
  spread_draws(b_cTime, `b_EnvironmentSoil:cTime`, 
               `r_Study:Time_series`[Time_series, term]) %>% 
  filter(term=='cTime') %>% 
  ungroup() %>% 
  mutate(Environment = 'Soil',
         pop_value = b_cTime + `b_EnvironmentSoil:cTime`,
         ts_value = b_cTime + `b_EnvironmentSoil:cTime` + 
           `r_Study:Time_series`) %>% 
  select(.chain, .iteration, .draw, Environment, Time_series,
         pop_value, ts_value) %>% 
  separate(Time_series, into = c('Study', 'Time_series'), extra = 'merge') %>% 
  # fix 10 names that are screwing up my code
  mutate(Time_series = ifelse(Time_series=='Microcosm_Jurburg_Microcosm.1Heat.Soil',
                              'Jurburg_Microcosm.1Heat.Soil', Time_series),
         Time_series = ifelse(Time_series=='Microcosm_Jurburg_Microcosm.2Heat.Soil',
                              'Jurburg_Microcosm.2Heat.Soil', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Cip_C.diff.Pig_feces',
                              'Jurburg_Pigs.Cip_C.diff.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Cip.Pig_feces',
                              'Jurburg_Pigs.Cip.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Clin_C.diff.Pig_feces',
                              'Jurburg_Pigs.Clin_C.diff.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Clin.Pig_feces',
                              'Jurburg_Pigs.Clin.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Drought.Grazed',
                              'Jurburg_Portugal.Drought.Grazed', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Drought.Pristine',
                              'Jurburg_Portugal.Drought.Pristine', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Flood.Grazed',
                              'Jurburg_Portugal.Flood.Grazed', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Flood.Pristine',
                              'Jurburg_Portugal.Flood.Pristine', Time_series),
         # and now fix study name
         Study = ifelse(Time_series=='Jurburg_Microcosm.1Heat.Soil' |
                          Time_series=='Jurburg_Microcosm.2Heat.Soil', 
                        'Jurburg_Microcosm', Study),
         Study = ifelse(Time_series=='Jurburg_Pigs.Cip_C.diff.Pig_feces' |
                          Time_series=='Jurburg_Pigs.Cip.Pig_feces' | 
                          Time_series=='Jurburg_Pigs.Clin_C.diff.Pig_feces' |
                          Time_series=='Jurburg_Pigs.Clin.Pig_feces', 
                        'Jurburg_Pigs', Study),
         Study = ifelse(Time_series=='Jurburg_Portugal.Drought.Grazed' |
                          Time_series=='Jurburg_Portugal.Drought.Pristine' | 
                          Time_series=='Jurburg_Portugal.Flood.Grazed' |
                          Time_series=='Jurburg_Portugal.Flood.Pristine', 
                        'Jurburg_Portugal', Study)) 

study_env <- alpha_t50_dat %>% 
  unite(filter, c(Study, Environment), remove = FALSE)

time_ES <- bind_rows(aquatic_t50_ES %>% 
                       unite(filter, c(Study, Environment), remove = FALSE) %>% 
                       filter(filter %in% study_env$filter),
                     mammal_t50_ES %>% 
                       unite(filter, c(Study, Environment), remove = FALSE) %>% 
                       filter(filter %in% study_env$filter),
                     soil_t50_ES %>% 
                       unite(filter, c(Study, Environment), remove = FALSE) %>% 
                       filter(filter %in% study_env$filter))

load('~/Dropbox/1current/microbiome-disturbance/data/alpha_sf_meta.Rdata')

time_ES <- left_join(time_ES, alpha_sf_meta)

# population (fixed) effects: 
alpha_time_pop_ES_post <- alpha_q0_time_50_nbinom2_shape %>% 
  spread_draws(., 
               b_cTime, `b_EnvironmentMammal:cTime`,
               `b_EnvironmentSoil:cTime`,
               seed = seed) 

alpha_time_ES <- tibble(ES = alpha_time_pop_ES_post$b_cTime,
                      Environment = 'Aquatic') %>% 
  bind_rows(tibble(ES = alpha_time_pop_ES_post$b_cTime +
                     alpha_time_pop_ES_post$`b_EnvironmentMammal:cTime`,
                   Environment = 'Mammal')) %>% 
  bind_rows(tibble(ES = alpha_time_pop_ES_post$b_cTime +
                     alpha_time_pop_ES_post$`b_EnvironmentSoil:cTime`,
                   Environment = 'Soil'))

library(ggridges)

ggplot() +
  facet_wrap(~Environment, scales = 'free_y') + 
  geom_density_ridges_gradient(data = time_ES,
                               aes(x = pop_value + ts_value,
                                   y = Selective_Factor,
                                   fill = stat(quantile)
                               ),
                               quantiles = c(1),
                               calc_ecdf = T,
                               scale = 0.9,
                               linetype = 0) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_vline(data = alpha_time_ES %>% 
               group_by(Environment) %>% 
               summarise(ES = median(ES)),
             aes(xintercept = ES)) +
  geom_rect(data = alpha_time_ES %>% 
              group_by(Environment) %>% 
              summarise(q97.5 = quantile(ES, prob = 0.975),
                        q2.5 = quantile(ES, prob = 0.025)),
            aes(ymin = -Inf, ymax = Inf,
                xmin = q2.5, xmax = q97.5),
            alpha = 0.2) +
  geom_text(data = alpha_sf_meta %>% 
              group_by(Environment, Selective_Factor) %>%
              summarise(n_ts = n_distinct(Time_series)) %>% 
              ungroup(),
            aes(x=0.075, y=Selective_Factor, 
                label=paste('n[time~series] == ', n_ts)),
            size=2,
            nudge_y = 0.25, parse = T) +
  scale_fill_manual(name = '',
                    guide = 'none',
                    values = '#cccccc') +
  labs(y = 'Disturbance type',
       x = 'Recovery rate following disturbance [log(richness) / day]') +
  theme_bw()

ggsave('~/Dropbox/1current/Steph/revision-figures/time_q0_disturbance_type_ES.pdf',
       width = 290, height = 180, units = 'mm')

# es (forest) plot for each time series
time_ES %>% 
  ggplot() +
  facet_wrap(~Environment, scales = 'free') +
  geom_hline(yintercept = 0, lty = 2) + 
  stat_summary(aes(x = Time_series,
                   y = pop_value + ts_value),
               fun = 'median',
               fun.max = function(x) quantile(x, probs = 0.975),
               fun.min = function(x) quantile(x, probs = 0.025)) +
  labs(x = 'Time series ID',
       y = 'Richness effect size (response following disturbance)') +
  coord_flip() +
  theme_bw() +
  theme(axis.text.y = element_blank())

ggsave('~/Dropbox/1current/Steph/revision-figures/richness_t50_ES-per-ts.pdf',
       width = 290, height = 180, units = 'mm')

# table for counts of directional (and no) trends
richness_t50_table <-
  time_ES %>% 
  mutate(es = pop_value + ts_value) %>% 
  group_by(Environment, Study, Time_series) %>% 
  summarise(median_es = median(es),
            q97.5 = quantile(es, probs = 0.975),
            q2.5 = quantile(es, probs = 0.025)) %>% 
  ungroup() %>% 
  mutate(qual_change = case_when(q2.5 > 0 & q97.5 > 0 ~ 'up',
                                 q2.5 < 0 & q97.5 > 0 ~ 'neutral',
                                 q2.5 < 0 & q97.5 < 0 ~ 'down')) %>% 
  group_by(Environment, qual_change) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  complete(Environment, qual_change,
           fill = list(n = 0)) %>%   
  mutate(metric = 'Richness',
         ba_or_time = 'time')

# put all the tables together and save as csv
bind_rows(richness_ba_table,
          richness_t50_table,
          dispersion_ba_table,
          dispersion_t50_table,
          turnover_t50_table) %>% 
  write_csv('~/Dropbox/1current/Steph/revision-figures/es-counts.csv')
