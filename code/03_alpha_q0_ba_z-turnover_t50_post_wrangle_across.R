# code to produce figure 6

# combine effect sizes from alpha-scale before_after and resilience time series analyses
# these models were fit to data standardised across studies

source('~/Dropbox/1current/microbiome-disturbance/code/00_init_dirs_load_packages.R')

load(paste0(path2wd, 'model-fits-across/alpha_ba2-7386305.Rdata'))
load(paste0(path2wd, 'model-fits-across/z-resil-time-6401876.Rdata'))

alpha_ba_dat <- alpha_q0_ba_nbinom_shape$data %>% 
  as_tibble() 

# aquatic effect sizes from before-after model
aquatic_ba_ES <- alpha_q0_ba_nbinom_shape %>% 
  spread_draws(b_before_afterafter, `r_Study:Time_series`[Time_series, term]) %>% 
  filter(term=='before_afterafter') %>% 
  ungroup() %>% 
  mutate(Environment = 'Aquatic',
         before_after = 'after',
         pop_value = b_before_afterafter, 
         ts_value = b_before_afterafter + `r_Study:Time_series`) %>% 
  select(.chain, .iteration, .draw, Environment, Time_series, before_after, pop_value, ts_value) %>% 
  separate(Time_series, into = c('Study', 'Time_series'), extra = 'merge') %>% 
  # fix 10 names that are screwing up my code
  mutate(Time_series = ifelse(Time_series=='Microcosm_Jurburg_Microcosm.1Heat.Soil', 'Jurburg_Microcosm.1Heat.Soil', Time_series),
         Time_series = ifelse(Time_series=='Microcosm_Jurburg_Microcosm.2Heat.Soil', 'Jurburg_Microcosm.2Heat.Soil', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Cip_C.diff.Pig_feces', 'Jurburg_Pigs.Cip_C.diff.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Cip.Pig_feces', 'Jurburg_Pigs.Cip.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Clin_C.diff.Pig_feces', 'Jurburg_Pigs.Clin_C.diff.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Clin.Pig_feces', 'Jurburg_Pigs.Clin.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Drought.Grazed', 'Jurburg_Portugal.Drought.Grazed', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Drought.Pristine', 'Jurburg_Portugal.Drought.Pristine', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Flood.Grazed', 'Jurburg_Portugal.Flood.Grazed', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Flood.Pristine', 'Jurburg_Portugal.Flood.Pristine', Time_series),
         # and now fix study name
         Study = ifelse(Time_series=='Jurburg_Microcosm.1Heat.Soil' | Time_series=='Jurburg_Microcosm.2Heat.Soil', 'Jurburg_Microcosm', Study),
         Study = ifelse(Time_series=='Jurburg_Pigs.Cip_C.diff.Pig_feces' | Time_series=='Jurburg_Pigs.Cip.Pig_feces' | 
                          Time_series=='Jurburg_Pigs.Clin_C.diff.Pig_feces' | Time_series=='Jurburg_Pigs.Clin.Pig_feces', 'Jurburg_Pigs', Study),
         Study = ifelse(Time_series=='Jurburg_Portugal.Drought.Grazed' | Time_series=='Jurburg_Portugal.Drought.Pristine' | 
                          Time_series=='Jurburg_Portugal.Flood.Grazed' | Time_series=='Jurburg_Portugal.Flood.Pristine', 'Jurburg_Portugal', Study)) 

# mammal 
mammal_ba_ES <- alpha_q0_ba_nbinom_shape %>% 
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
  mutate(Time_series = ifelse(Time_series=='Microcosm_Jurburg_Microcosm.1Heat.Soil', 'Jurburg_Microcosm.1Heat.Soil', Time_series),
         Time_series = ifelse(Time_series=='Microcosm_Jurburg_Microcosm.2Heat.Soil', 'Jurburg_Microcosm.2Heat.Soil', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Cip_C.diff.Pig_feces', 'Jurburg_Pigs.Cip_C.diff.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Cip.Pig_feces', 'Jurburg_Pigs.Cip.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Clin_C.diff.Pig_feces', 'Jurburg_Pigs.Clin_C.diff.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Clin.Pig_feces', 'Jurburg_Pigs.Clin.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Drought.Grazed', 'Jurburg_Portugal.Drought.Grazed', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Drought.Pristine', 'Jurburg_Portugal.Drought.Pristine', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Flood.Grazed', 'Jurburg_Portugal.Flood.Grazed', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Flood.Pristine', 'Jurburg_Portugal.Flood.Pristine', Time_series),
         # and now fix study name
         Study = ifelse(Time_series=='Jurburg_Microcosm.1Heat.Soil' | Time_series=='Jurburg_Microcosm.2Heat.Soil', 'Jurburg_Microcosm', Study),
         Study = ifelse(Time_series=='Jurburg_Pigs.Cip_C.diff.Pig_feces' | Time_series=='Jurburg_Pigs.Cip.Pig_feces' | 
                          Time_series=='Jurburg_Pigs.Clin_C.diff.Pig_feces' | Time_series=='Jurburg_Pigs.Clin.Pig_feces', 'Jurburg_Pigs', Study),
         Study = ifelse(Time_series=='Jurburg_Portugal.Drought.Grazed' | Time_series=='Jurburg_Portugal.Drought.Pristine' | 
                          Time_series=='Jurburg_Portugal.Flood.Grazed' | Time_series=='Jurburg_Portugal.Flood.Pristine', 'Jurburg_Portugal', Study)) 

# soil 
soil_ba_ES <- alpha_q0_ba_nbinom_shape %>% 
  spread_draws(b_before_afterafter, `b_EnvironmentSoil:before_afterafter`, `r_Study:Time_series`[Time_series, term]) %>% 
  filter(term=='before_afterafter') %>% 
  ungroup() %>% 
  mutate(Environment = 'Soil',
         before_after = 'after',
         pop_value = b_before_afterafter + `b_EnvironmentSoil:before_afterafter`,
         ts_value = b_before_afterafter + `b_EnvironmentSoil:before_afterafter` + `r_Study:Time_series`) %>% 
  select(.chain, .iteration, .draw, Environment, Time_series, before_after, pop_value, ts_value) %>% 
  separate(Time_series, into = c('Study', 'Time_series'), extra = 'merge') %>% 
  # fix 10 names that are screwing up my code
  mutate(Time_series = ifelse(Time_series=='Microcosm_Jurburg_Microcosm.1Heat.Soil', 'Jurburg_Microcosm.1Heat.Soil', Time_series),
         Time_series = ifelse(Time_series=='Microcosm_Jurburg_Microcosm.2Heat.Soil', 'Jurburg_Microcosm.2Heat.Soil', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Cip_C.diff.Pig_feces', 'Jurburg_Pigs.Cip_C.diff.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Cip.Pig_feces', 'Jurburg_Pigs.Cip.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Clin_C.diff.Pig_feces', 'Jurburg_Pigs.Clin_C.diff.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Clin.Pig_feces', 'Jurburg_Pigs.Clin.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Drought.Grazed', 'Jurburg_Portugal.Drought.Grazed', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Drought.Pristine', 'Jurburg_Portugal.Drought.Pristine', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Flood.Grazed', 'Jurburg_Portugal.Flood.Grazed', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Flood.Pristine', 'Jurburg_Portugal.Flood.Pristine', Time_series),
         # and now fix study name
         Study = ifelse(Time_series=='Jurburg_Microcosm.1Heat.Soil' | Time_series=='Jurburg_Microcosm.2Heat.Soil', 'Jurburg_Microcosm', Study),
         Study = ifelse(Time_series=='Jurburg_Pigs.Cip_C.diff.Pig_feces' | Time_series=='Jurburg_Pigs.Cip.Pig_feces' | 
                          Time_series=='Jurburg_Pigs.Clin_C.diff.Pig_feces' | Time_series=='Jurburg_Pigs.Clin.Pig_feces', 'Jurburg_Pigs', Study),
         Study = ifelse(Time_series=='Jurburg_Portugal.Drought.Grazed' | Time_series=='Jurburg_Portugal.Drought.Pristine' | 
                          Time_series=='Jurburg_Portugal.Flood.Grazed' | Time_series=='Jurburg_Portugal.Flood.Pristine', 'Jurburg_Portugal', Study)) 

study_env <- alpha_ba_dat %>% 
  unite(filter, c(Study, Environment), remove = FALSE)

ba_ES <- bind_rows(aquatic_ba_ES %>% 
                     unite(filter, c(Study, Environment), remove = FALSE) %>% 
                     filter(filter %in% study_env$filter),
                   mammal_ba_ES %>% 
                     unite(filter, c(Study, Environment), remove = FALSE) %>% 
                     filter(filter %in% study_env$filter),
                   soil_ba_ES %>% 
                     unite(filter, c(Study, Environment), remove = FALSE) %>% 
                     filter(filter %in% study_env$filter)) %>% 
  mutate(pop_after = pop_value,
         ts_after = ts_value) %>% 
  select(-before_after, -pop_value, -ts_value)

alpha_q0_ba_ES_summary <- ba_ES %>% 
  group_by(Environment, Study, Time_series) %>% 
  summarise(pop_ba = median(pop_after),
            pop_ba_upper = quantile(pop_after, probs = 0.975),
            pop_ba_lower = quantile(pop_after, probs = 0.025),
            # ts summary
            ts_ba = median(ts_after),
            ts_ba_upper = quantile(ts_after, probs = 0.975),
            ts_ba_lower = quantile(ts_after, probs = 0.025))

# wrangle resilience
resil_t50_dat <- z.resil_time_50_sigma$data %>% 
  as_tibble() 

# aquatic effect sizes from before-after model
aquatic_t50_ES <- z.resil_time_50_sigma %>% 
  spread_draws(b_cTime, `r_Study:Time_series`[Time_series, term]) %>% 
  filter(term=='cTime') %>% 
  ungroup() %>% 
  mutate(Environment = 'Aquatic',
         pop_value = b_cTime,
         ts_value = b_cTime + `r_Study:Time_series`) %>% 
  select(.chain, .iteration, .draw, Environment, Time_series, pop_value, ts_value) %>% 
  separate(Time_series, into = c('Study', 'Time_series'), extra = 'merge') %>% 
  # fix 10 names that are screwing up my code
  mutate(Time_series = ifelse(Time_series=='Microcosm_Jurburg_Microcosm.1Heat.Soil', 'Jurburg_Microcosm.1Heat.Soil', Time_series),
         Time_series = ifelse(Time_series=='Microcosm_Jurburg_Microcosm.2Heat.Soil', 'Jurburg_Microcosm.2Heat.Soil', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Cip_C.diff.Pig_feces', 'Jurburg_Pigs.Cip_C.diff.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Cip.Pig_feces', 'Jurburg_Pigs.Cip.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Clin_C.diff.Pig_feces', 'Jurburg_Pigs.Clin_C.diff.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Clin.Pig_feces', 'Jurburg_Pigs.Clin.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Drought.Grazed', 'Jurburg_Portugal.Drought.Grazed', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Drought.Pristine', 'Jurburg_Portugal.Drought.Pristine', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Flood.Grazed', 'Jurburg_Portugal.Flood.Grazed', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Flood.Pristine', 'Jurburg_Portugal.Flood.Pristine', Time_series),
         # and now fix study name
         Study = ifelse(Time_series=='Jurburg_Microcosm.1Heat.Soil' | Time_series=='Jurburg_Microcosm.2Heat.Soil', 'Jurburg_Microcosm', Study),
         Study = ifelse(Time_series=='Jurburg_Pigs.Cip_C.diff.Pig_feces' | Time_series=='Jurburg_Pigs.Cip.Pig_feces' | 
                          Time_series=='Jurburg_Pigs.Clin_C.diff.Pig_feces' | Time_series=='Jurburg_Pigs.Clin.Pig_feces', 'Jurburg_Pigs', Study),
         Study = ifelse(Time_series=='Jurburg_Portugal.Drought.Grazed' | Time_series=='Jurburg_Portugal.Drought.Pristine' | 
                          Time_series=='Jurburg_Portugal.Flood.Grazed' | Time_series=='Jurburg_Portugal.Flood.Pristine', 'Jurburg_Portugal', Study)) 

# mammal 
mammal_t50_ES <- z.resil_time_50_sigma %>% 
  spread_draws(b_cTime, `b_EnvironmentMammal:cTime`, `r_Study:Time_series`[Time_series, term]) %>% 
  filter(term=='cTime') %>% 
  ungroup() %>% 
  mutate(Environment = 'Mammal',
         pop_value = b_cTime + `b_EnvironmentMammal:cTime`, 
         ts_value = b_cTime + `b_EnvironmentMammal:cTime` + `r_Study:Time_series`) %>% 
  select(.chain, .iteration, .draw, Environment, Time_series, pop_value, ts_value) %>% 
  separate(Time_series, into = c('Study', 'Time_series'), extra = 'merge') %>% 
  # fix 10 names that are screwing up my code
  mutate(Time_series = ifelse(Time_series=='Microcosm_Jurburg_Microcosm.1Heat.Soil', 'Jurburg_Microcosm.1Heat.Soil', Time_series),
         Time_series = ifelse(Time_series=='Microcosm_Jurburg_Microcosm.2Heat.Soil', 'Jurburg_Microcosm.2Heat.Soil', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Cip_C.diff.Pig_feces', 'Jurburg_Pigs.Cip_C.diff.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Cip.Pig_feces', 'Jurburg_Pigs.Cip.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Clin_C.diff.Pig_feces', 'Jurburg_Pigs.Clin_C.diff.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Clin.Pig_feces', 'Jurburg_Pigs.Clin.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Drought.Grazed', 'Jurburg_Portugal.Drought.Grazed', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Drought.Pristine', 'Jurburg_Portugal.Drought.Pristine', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Flood.Grazed', 'Jurburg_Portugal.Flood.Grazed', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Flood.Pristine', 'Jurburg_Portugal.Flood.Pristine', Time_series),
         # and now fix study name
         Study = ifelse(Time_series=='Jurburg_Microcosm.1Heat.Soil' | Time_series=='Jurburg_Microcosm.2Heat.Soil', 'Jurburg_Microcosm', Study),
         Study = ifelse(Time_series=='Jurburg_Pigs.Cip_C.diff.Pig_feces' | Time_series=='Jurburg_Pigs.Cip.Pig_feces' | 
                          Time_series=='Jurburg_Pigs.Clin_C.diff.Pig_feces' | Time_series=='Jurburg_Pigs.Clin.Pig_feces', 'Jurburg_Pigs', Study),
         Study = ifelse(Time_series=='Jurburg_Portugal.Drought.Grazed' | Time_series=='Jurburg_Portugal.Drought.Pristine' | 
                          Time_series=='Jurburg_Portugal.Flood.Grazed' | Time_series=='Jurburg_Portugal.Flood.Pristine', 'Jurburg_Portugal', Study)) 

# soil 
soil_t50_ES <- z.resil_time_50_sigma %>% 
  spread_draws(b_cTime, `b_EnvironmentSoil:cTime`, `r_Study:Time_series`[Time_series, term]) %>% 
  filter(term=='cTime') %>% 
  ungroup() %>% 
  mutate(Environment = 'Soil',
         pop_value = b_cTime + `b_EnvironmentSoil:cTime`, 
         ts_value = b_cTime + `b_EnvironmentSoil:cTime` + `r_Study:Time_series`) %>% 
  select(.chain, .iteration, .draw, Environment, Time_series, pop_value, ts_value) %>% 
  separate(Time_series, into = c('Study', 'Time_series'), extra = 'merge') %>% 
  # fix 10 names that are screwing up my code
  mutate(Time_series = ifelse(Time_series=='Microcosm_Jurburg_Microcosm.1Heat.Soil', 'Jurburg_Microcosm.1Heat.Soil', Time_series),
         Time_series = ifelse(Time_series=='Microcosm_Jurburg_Microcosm.2Heat.Soil', 'Jurburg_Microcosm.2Heat.Soil', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Cip_C.diff.Pig_feces', 'Jurburg_Pigs.Cip_C.diff.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Cip.Pig_feces', 'Jurburg_Pigs.Cip.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Clin_C.diff.Pig_feces', 'Jurburg_Pigs.Clin_C.diff.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Pigs_Jurburg_Pigs.Clin.Pig_feces', 'Jurburg_Pigs.Clin.Pig_feces', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Drought.Grazed', 'Jurburg_Portugal.Drought.Grazed', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Drought.Pristine', 'Jurburg_Portugal.Drought.Pristine', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Flood.Grazed', 'Jurburg_Portugal.Flood.Grazed', Time_series),
         Time_series = ifelse(Time_series=='Portugal_Jurburg_Portugal.Flood.Pristine', 'Jurburg_Portugal.Flood.Pristine', Time_series),
         # and now fix study name
         Study = ifelse(Time_series=='Jurburg_Microcosm.1Heat.Soil' | Time_series=='Jurburg_Microcosm.2Heat.Soil', 'Jurburg_Microcosm', Study),
         Study = ifelse(Time_series=='Jurburg_Pigs.Cip_C.diff.Pig_feces' | Time_series=='Jurburg_Pigs.Cip.Pig_feces' | 
                          Time_series=='Jurburg_Pigs.Clin_C.diff.Pig_feces' | Time_series=='Jurburg_Pigs.Clin.Pig_feces', 'Jurburg_Pigs', Study),
         Study = ifelse(Time_series=='Jurburg_Portugal.Drought.Grazed' | Time_series=='Jurburg_Portugal.Drought.Pristine' | 
                          Time_series=='Jurburg_Portugal.Flood.Grazed' | Time_series=='Jurburg_Portugal.Flood.Pristine', 'Jurburg_Portugal', Study)) 

study_env <- resil_t50_dat %>% 
  unite(filter, c(Study, Environment), remove = FALSE)

resil_time_ES <- bind_rows(aquatic_t50_ES %>% 
                             unite(filter, c(Study, Environment), remove = FALSE) %>% 
                             filter(filter %in% study_env$filter),
                           mammal_t50_ES %>% 
                             unite(filter, c(Study, Environment), remove = FALSE) %>% 
                             filter(filter %in% study_env$filter),
                           soil_t50_ES %>% 
                             unite(filter, c(Study, Environment), remove = FALSE) %>% 
                             filter(filter %in% study_env$filter)) %>% 
  mutate(pop_slope = pop_value,
         ts_slope = ts_value) %>% 
  select(-pop_value, -ts_value)


resil_ES_summary <- resil_time_ES %>% 
  group_by(Environment, Study, Time_series) %>% 
  summarise(pop_slope_median = median(pop_slope),
            pop_slope_upper = quantile(pop_slope, probs = 0.975),
            pop_slope_lower = quantile(pop_slope, probs = 0.025),
            # ts summary
            ts_slope_median = median(ts_slope),
            ts_slope_upper = quantile(ts_slope, probs = 0.975),
            ts_slope_lower = quantile(ts_slope, probs = 0.025))

# join 'em together and plot, recall we don't have estimates for all time series
# in the before-after (i.e., response to) disturbance analysis
alpha_ba_resil_t50 <- left_join(alpha_q0_ba_ES_summary, 
                                resil_ES_summary)

ggplot() +
  # facet_wrap(~Environment) +
  geom_point(data = alpha_ba_resil_t50,
             aes(x = ts_ba, y = ts_slope_median, colour = Environment), alpha = 0.25) +
  geom_linerange(data = alpha_ba_resil_t50,
                 aes(x = ts_ba, ymin = ts_slope_lower, ymax = ts_slope_upper,
                     colour = Environment), alpha = 0.25) +
  geom_linerange(data = alpha_ba_resil_t50,
                 aes(y = ts_slope_median, xmin = ts_ba_lower, xmax = ts_ba_upper,
                     colour = Environment), alpha = 0.25) +
  # overall effects
  geom_point(data = alpha_ba_resil_t50,
             aes(x = pop_ba, y = pop_slope_median, colour = Environment), size = 3) +
  geom_linerange(data = alpha_ba_resil_t50,
                 aes(x = pop_ba, ymin = pop_slope_lower, ymax = pop_slope_upper, colour = Environment),
                 size = 1.25) +
  geom_linerange(data = alpha_ba_resil_t50,
                 aes(y = pop_slope_median, xmin = pop_ba_lower, xmax = pop_ba_upper, colour = Environment),
                 size = 1.25) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = expression(paste(alpha, '-scale richness response to treatment')),
       y = 'Turnover effect size') +
  scale_color_viridis_d(option = 'cividis') +
  theme_minimal() +
  theme(legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.direction = 'vertical')

