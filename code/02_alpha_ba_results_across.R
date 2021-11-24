# alpha diversity before after results for the models fit to data standardised across studies 
# i.e., one standardisation for all data.

source('~/Dropbox/1current/microbiome-disturbance/code/00_init_dirs_load_packages.R')

# these models have varying intercept for study, 
# and varying intercept and slope for Study:Time_series
# study-level variation in the shape paramenter of the negative binomial distribution
load(paste0(path2wd, 'model-fits-across/alpha_ba2-7386305.Rdata'))

# this looks good
mcmc_plot(alpha_q0_ba_nbinom_shape, type = 'trace')

# study-level variation in overdispersion for the win :)
pp_check(alpha_q0_ba_nbinom_shape) +
  scale_x_continuous(trans = 'log2')

# examine fit to individual time series
pp_check(alpha_q0_ba_nbinom_shape, type = 'scatter_avg_grouped', group = 'Study:Time_series') +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2') +
  geom_abline(intercept = 0, slope = 1, lty = 2)

# another look (model fit to central tendency of time series)
pp_check(alpha_q0_ba_nbinom_shape, type = 'stat_grouped', group = 'Study:Time_series')

# more model inspection
alpha_q0_ba_dat <- alpha_q0_ba_nbinom_shape$data %>% 
  as_tibble()

# residual check
resid_alpha_q0_ba <- residuals(alpha_q0_ba_nbinom_shape) %>% 
  as_tibble() %>% 
  bind_cols(alpha_q0_ba_dat) %>% 
  rename(resid = Estimate) %>% 
  select(-Est.Error, -Q2.5, -Q97.5) 

alpha_q0_ba_predicted <- predict(alpha_q0_ba_nbinom_shape) %>% 
  as_tibble()

# add predicted values to residual df 
resid_alpha_q0_ba <- resid_alpha_q0_ba %>% 
  mutate(predicted = alpha_q0_ba_predicted$Estimate)

# residuals look ok
par(mfrow=c(2,3))
with(resid_alpha_q0_ba, plot(resid ~ Environment));abline(h = 0, lty = 2)
with(resid_alpha_q0_ba, plot(resid ~ before_after));abline(h = 0, lty = 2)
with(resid_alpha_q0_ba, plot(resid ~ Study));abline(h = 0, lty = 2)
with(resid_alpha_q0_ba, plot(resid ~ Time_series));abline(h = 0, lty = 2)
with(resid_alpha_q0_ba, plot(resid ~ predicted));abline(h = 0, lty = 2)
with(resid_alpha_q0_ba, plot(Observed, predicted));abline(c(0,1), lty = 2)

# population (fixed) effects: 
alpha_q0_ba_fixed <- fixef(alpha_q0_ba_nbinom_shape)

# predictions at the population level (not including uncertainty associated with study-level variation)
alpha_q0_ba_fitted <- fitted(alpha_q0_ba_nbinom_shape, re_formula = NA) %>% 
  as_tibble() %>% 
  # NB: estimate is predicted value, value is observed
  bind_cols(alpha_q0_ba_dat)

# expected richness in each environment before and after the experiment
alpha_q0_ba_fitted %>%
  distinct(Environment, before_after, Estimate, Q2.5, Q97.5)

# prelim plot
alpha_ba_plot <- ggplot() +
  facet_wrap(~Environment) +
  # the data
  geom_point(data = alpha_q0_ba_dat,
             aes(x = before_after, y = Observed, group = Study, colour = Study),
             position = position_dodge(width = 0.5),
             size = 0.75) +
  # expected values for each environment from the model
  geom_point(data = alpha_q0_ba_fitted %>% 
               distinct(Environment, Estimate, before_after),
             aes(x = before_after, y = Estimate),
             size = 2) +
  geom_linerange(data = alpha_q0_ba_fitted %>% 
                   distinct(Environment, Estimate, Q2.5, Q97.5, before_after),
                 aes(x = before_after, ymin = Q2.5, ymax = Q97.5)) +
  labs(x = 'Disturbance',
       y = 'Richness') +
  scale_color_viridis_d(guide = FALSE, option = 'magma') +
  scale_y_continuous(trans = 'log2') +
  theme_bw() +
  theme(axis.title = element_text(size = 10))

# ggsave('~/Dropbox/1current/Steph/figs/alpha_q0_ba.png',
#        width = 210, height = 70, units = 'mm')  
