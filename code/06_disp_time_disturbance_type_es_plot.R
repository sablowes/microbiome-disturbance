source('~/Dropbox/1current/microbiome-disturbance/code/00_init_dirs_load_packages.R')

load(paste0(path2wd, 'model-fits-across/dispersion_time2-7612520.Rdata'))

seed = 123

disp_t50_dat <- disp_time_50_2_phi2$data %>% 
  as_tibble() 

# aquatic effect sizes from before-after model
aquatic_t50_ES <- disp_time_50_2_phi2 %>% 
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
mammal_t50_ES <- disp_time_50_2_phi2 %>% 
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
                          Time_series=='Jurburg_Portugal.Flood.Pristine', 'Jurburg_Portugal', Study)) 

# soil 
soil_t50_ES <- disp_time_50_2_phi2 %>% 
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

study_env <- disp_t50_dat %>% 
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

load('~/Dropbox/1current/microbiome-disturbance/data/disp_sf_meta.Rdata')

time_ES <- left_join(time_ES, disp_sf_meta)

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
  geom_vline(data = time_ES %>% 
               group_by(Environment) %>% 
               summarise(ES = median(pop_value)),
             aes(xintercept = ES)) +
  geom_rect(data = time_ES %>% 
              group_by(Environment) %>% 
              summarise(q97.5 = quantile(pop_value, prob = 0.975),
                        q2.5 = quantile(pop_value, prob = 0.025)),
            aes(ymin = -Inf, ymax = Inf,
                xmin = q2.5, xmax = q97.5),
            alpha = 0.2) +
  geom_text(data = time_ES %>%
              group_by(Environment, Selective_Factor) %>%
              summarise(n_ts = n_distinct(Time_series)) %>%
              ungroup(),
            aes(x=0.1, y=Selective_Factor,
                label=paste('n[time~series] == ', n_ts)),
            size=2,
            nudge_y = 0.25, parse = T) +
  scale_fill_manual(name = '',
                    guide = 'none',
                    values = '#cccccc') +
  labs(y = 'Disturbance type',
       x = 'Dispersion change following disturbance') +
  theme_bw()

ggsave('~/Dropbox/1current/Steph/revision-figures/dispersion_time_disturbance_type_ES.pdf',
       width = 290, height = 180, units = 'mm')

