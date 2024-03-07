# compare coefficients from models fit to richness (q = 0),
# and Simpson diversity (q = 2)

source('~/Dropbox/1current/microbiome-disturbance/code/00_init_dirs_load_packages.R')

seed = 42

# these models have varying intercept for study, 
# and varying intercept and slope for Study:Time_series
# study-level variation in the shape paramenter of the negative binomial distribution

# q = 0, initial response to disturbance
load(paste0(path2wd, 'model-fits-across/alpha_ba2-7386305.Rdata'))
# q = 2, initial response to disturbance
load(paste0(path2wd, 'model-fits-across/alpha_ba_2.Rdata'))

# q = 0, recovery following disturbance
load(paste0(path2wd, 'model-fits-across/alpha_time_q0-7612517.Rdata'))
# q = 2, recovery following disturbance
load(paste0(path2wd, 'model-fits-across/alpha_t50_q2_2.Rdata'))

# want population (fixed) parameter estimates
ba_q0_pars <- alpha_q0_ba_nbinom2_shape %>% 
  gather_draws(b_Intercept,
               b_EnvironmentMammal,
               b_EnvironmentSoil,
               b_before_afterafter,
               `b_EnvironmentMammal:before_afterafter`,
               `b_EnvironmentSoil:before_afterafter`,
               seed = seed,
               ndraws = 1000) %>% 
  ungroup() %>% 
  mutate(metric = 'Richness (q = 0)',
         model = 'Immediate response to disturbance (diversity before and after)')

ba_q2_pars <- alpha_q2_ba2_sigma %>% 
  gather_draws(b_Intercept,
               b_EnvironmentMammal,
               b_EnvironmentSoil,
               b_before_afterafter,
               `b_EnvironmentMammal:before_afterafter`,
               `b_EnvironmentSoil:before_afterafter`,
               seed = seed,
               ndraws = 1000) %>% 
  ungroup() %>% 
  mutate(metric = 'Simpson (q = 2)',
         model = 'Immediate response to disturbance (diversity before and after)')

t50_q0_pars <- alpha_q0_time_50_nbinom2_shape %>% 
  gather_draws(b_Intercept,
               b_EnvironmentMammal,
               b_EnvironmentSoil,
               b_cTime,
               `b_EnvironmentMammal:cTime`,
               `b_EnvironmentSoil:cTime`,
               seed = seed,
               ndraws = 1000) %>% 
  ungroup() %>% 
  mutate(metric = 'Richness (q = 0)',
         model = 'Recovery following disturbance (diversity change for 50 days following disturbance)')

t50_q2_pars <- alpha_q2_time_50_2_sigma %>% 
  gather_draws(b_Intercept,
               b_EnvironmentMammal,
               b_EnvironmentSoil,
               b_cTime,
               `b_EnvironmentMammal:cTime`,
               `b_EnvironmentSoil:cTime`,
               seed = seed,
               ndraws = 1000) %>% 
  ungroup() %>% 
  mutate(metric = 'Simpson (q = 2)',
         model = 'Recovery following disturbance (diversity change for 50 days following disturbance)')

ba_coefs <- bind_rows(ba_q0_pars,
          ba_q2_pars) %>% 
  ggplot() + 
  facet_wrap(~model, labeller = label_wrap_gen()) + 
  stat_summary(aes(y = .value, 
                   x = factor(.variable,
                              levels = c('b_Intercept',
                                         'b_EnvironmentMammal',
                                         'b_EnvironmentSoil',
                                         'b_before_afterafter',
                                         'b_EnvironmentMammal:before_afterafter',
                                         'b_EnvironmentSoil:before_afterafter'),
                              labels = c('Intercept (aquatic)',
                                         'Environment (mammal)',
                                         'Environment (soil)',
                                         'before_after (aquatic)',
                                         'Environment (mammal):before_after',
                                         'Environment (soil):before_after')),
                   colour = metric, group = metric),
               position = position_dodge(width = 0.5),
               fun = 'median',
               fun.min = function(x) quantile(x, prob = 0.025),
               fun.max = function(x) quantile(x, prob = 0.975)) +
  labs(x = 'Parameter',
       y = 'Estimate',
       tag = 'a') +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = c(1,0.9),
        legend.justification = c(1,1),
        strip.text = element_text(hjust = 0, size = 12, face = 'bold'))
  

t50_coefs <- bind_rows(t50_q0_pars,
          t50_q2_pars) %>% 
  ggplot() + 
  facet_wrap(~model, labeller = label_wrap_gen()) + 
  stat_summary(aes(y = .value, 
                   x = factor(.variable,
                              levels = c('b_Intercept',
                                         'b_EnvironmentMammal',
                                         'b_EnvironmentSoil',
                                         'b_cTime',
                                         'b_EnvironmentMammal:cTime',
                                         'b_EnvironmentSoil:cTime'),
                              labels = c('Intercept (aquatic)',
                                         'Environment (mammal)',
                                         'Environment (soil)',
                                         'Time (aquatic)',
                                         'Environment (mammal):Time',
                                         'Environment (soil):Time')),
                   colour = metric, group = metric),
               position = position_dodge(width = 0.5),
               fun = 'median',
               fun.min = function(x) quantile(x, prob = 0.025),
               fun.max = function(x) quantile(x, prob = 0.975)) +
  labs(x = 'Parameter',
       y = 'Estimate', 
       tag = 'b') +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = c(1,0.9),
        legend.justification = c(1,1),
        strip.text = element_text(hjust = 0, size = 12, face = 'bold'))

cowplot::plot_grid(ba_coefs,
                   t50_coefs, 
                   nrow = 2)

ggsave('~/Dropbox/1current/Steph/figs/alpha_q0_q2_coefs.pdf',
       width = 125, height = 250, units = 'mm')
