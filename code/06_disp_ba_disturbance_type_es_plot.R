source('~/Dropbox/1current/microbiome-disturbance/code/00_init_dirs_load_packages.R')

load(paste0(path2wd, 'model-fits-across/dispersion_ba2-7612519.Rdata'))

seed = 123

disp_ba_dat <- disp_before_after_2_phi2$data %>% 
  as_tibble() 

# aquatic effect sizes from before-after model
aquatic_ba_ES <- disp_before_after_2_phi2 %>% 
  spread_draws(b_before_afterafter, `r_Study:Time_series`[Time_series, term],
               seed = 123) %>% 
  filter(term=='before_afterafter') %>% 
  ungroup() %>% 
  mutate(Environment = 'Aquatic',
         before_after = 'after',
         pop_value = b_before_afterafter, 
         ts_value = b_before_afterafter + `r_Study:Time_series`) %>% 
  select(.chain, .iteration, .draw, Environment, Time_series, before_after, pop_value, ts_value) %>% 
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
         Study = ifelse(Time_series=='Jurburg_Microcosm.1Heat.Soil' | Time_series=='Jurburg_Microcosm.2Heat.Soil', 
                        'Jurburg_Microcosm', Study),
         Study = ifelse(Time_series=='Jurburg_Pigs.Cip_C.diff.Pig_feces' | Time_series=='Jurburg_Pigs.Cip.Pig_feces' | 
                          Time_series=='Jurburg_Pigs.Clin_C.diff.Pig_feces' | Time_series=='Jurburg_Pigs.Clin.Pig_feces', 
                        'Jurburg_Pigs', Study),
         Study = ifelse(Time_series=='Jurburg_Portugal.Drought.Grazed' | Time_series=='Jurburg_Portugal.Drought.Pristine' | 
                          Time_series=='Jurburg_Portugal.Flood.Grazed' | Time_series=='Jurburg_Portugal.Flood.Pristine', 
                        'Jurburg_Portugal', Study)) 

# mammal 
mammal_ba_ES <- disp_before_after_2_phi2 %>% 
  spread_draws(b_before_afterafter, `b_EnvironmentMammal:before_afterafter`, `r_Study:Time_series`[Time_series, term]) %>% 
  filter(term=='before_afterafter') %>% 
  ungroup() %>% 
  mutate(Environment = 'Mammal',
         before_after = 'after',
         pop_value = b_before_afterafter + `b_EnvironmentMammal:before_afterafter`,
         ts_value = b_before_afterafter + `b_EnvironmentMammal:before_afterafter` + `r_Study:Time_series`) %>% 
  select(.chain, .iteration, .draw, Environment, Time_series, before_after, pop_value, ts_value) %>% 
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
soil_ba_ES <- disp_before_after_2_phi2 %>% 
  spread_draws(b_before_afterafter, `b_EnvironmentSoil:before_afterafter`, 
               `r_Study:Time_series`[Time_series, term]) %>% 
  filter(term=='before_afterafter') %>% 
  ungroup() %>% 
  mutate(Environment = 'Soil',
         before_after = 'after',
         pop_value = b_before_afterafter + `b_EnvironmentSoil:before_afterafter`,
         ts_value = b_before_afterafter + `b_EnvironmentSoil:before_afterafter` +
           `r_Study:Time_series`) %>% 
  select(.chain, .iteration, .draw, Environment, Time_series, before_after,
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

study_env <- disp_ba_dat %>% 
  unite(filter, c(Study, Environment), remove = FALSE)

disp_ba_ES <- bind_rows(aquatic_ba_ES %>% 
                     unite(filter, c(Study, Environment), remove = FALSE) %>% 
                     filter(filter %in% study_env$filter),
                   mammal_ba_ES %>% 
                     unite(filter, c(Study, Environment), remove = FALSE) %>% 
                     filter(filter %in% study_env$filter),
                   soil_ba_ES %>% 
                     unite(filter, c(Study, Environment), remove = FALSE) %>% 
                     filter(filter %in% study_env$filter))

load('~/Dropbox/1current/microbiome-disturbance/data/disp_sf_meta.Rdata')

disp_ba_ES <- left_join(disp_ba_ES, disp_sf_meta)

# two time series are missing selective factor metadata
disp_ba_ES <- disp_ba_ES %>% 
  mutate(Selective_Factor = case_when(Time_series %in% c('deVries.Drought.mixLM',
                                                         'deVries.Drought.mixRL') ~
                                        'Drought',
                                      TRUE ~ as.character(Selective_Factor)))

library(ggridges)

ggplot() +
  facet_wrap(~Environment, scales = 'free_y') + 
  geom_density_ridges_gradient(data = disp_ba_ES,
                               aes(x = pop_value + ts_value,
                                   y = Selective_Factor,
                                   fill = stat(quantile)
                               ),
                               quantiles = c(1),
                               calc_ecdf = T,
                               scale = 0.9,
                               linetype = 0) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_vline(data = disp_ba_ES %>% 
               group_by(Environment) %>% 
               summarise(ES = median(pop_value)),
             aes(xintercept = ES)) +
  geom_rect(data = disp_ba_ES %>% 
              group_by(Environment) %>% 
              summarise(q97.5 = quantile(pop_value, prob = 0.975),
                        q2.5 = quantile(pop_value, prob = 0.025)),
            aes(ymin = -Inf, ymax = Inf,
                xmin = q2.5, xmax = q97.5),
            alpha = 0.2) +
  geom_text(data = disp_ba_ES %>% 
              group_by(Environment, Selective_Factor) %>%
              summarise(n_ts = n_distinct(Time_series)) %>% 
              ungroup(),
            aes(x=1.3, y=Selective_Factor, 
                label=paste('n[time~series] == ', n_ts)),
            size=2,
            nudge_y = 0.15, parse = T) +
  scale_fill_manual(name = '',
                    guide = 'none',
                    values = '#cccccc') +
  labs(y = 'Disturbance type',
       x = 'Dispersion response to disturbance effect size') +
  theme_bw()

ggsave('~/Dropbox/1current/Steph/revision-figures/dispersion_disturbance_type_ES.pdf',
       width = 290, height = 180, units = 'mm')


# want forest plot and counts of time series increasing/decreasing/no change
disp_ba_ES %>% 
  ggplot() +
  facet_wrap(~Environment, scales = 'free') +
  geom_hline(yintercept = 0, lty = 2) + 
  stat_summary(aes(x = Time_series,
                   y = pop_value + ts_value),
               fun = 'median',
               fun.max = function(x) quantile(x, probs = 0.975),
               fun.min = function(x) quantile(x, probs = 0.025)) +
  labs(x = 'Time series ID',
       y = 'Dispersion effect size (immediate response to disturbance)') +
  coord_flip() +
  theme_bw() +
  theme(axis.text.y = element_blank())

ggsave('~/Dropbox/1current/Steph/revision-figures/disp_ba_ES-per-ts.pdf',
       width = 290, height = 180, units = 'mm')

# table for counts of directional (and no) trends
dispersion_ba_table <- disp_ba_ES %>% 
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
  mutate(metric = 'Dispersion',
         ba_or_time = 'ba')
