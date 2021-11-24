# dispersion before after results
# within study effort standardisation

source('~/Dropbox/1current/microbiome-disturbance/code/00_init_dirs_load_packages.R')

load(paste0(path2wd, 'model-fits-within/disp-ba-8086863.Rdata'))

# these look ok (there is some autocorrelation left in the intercept-slope correlation )
mcmc_plot(disp_before_after_phi2, type = 'trace')

# model reproduces data ok
pp_check(disp_before_after_phi2) 

# data model was fit to
disp_ba_dat <- disp_before_after_phi2$data %>% 
  as_tibble()

# residual check
resid_disp_ba <- residuals(disp_before_after_phi2) %>% 
  as_tibble() %>% 
  bind_cols(disp_ba_dat) %>% 
  rename(resid = Estimate) %>% 
  select(-Est.Error, -Q2.5, -Q97.5) 

disp_ba_predicted <- predict(disp_before_after_phi2) %>% 
  as_tibble()

# add predicted values to residual df 
resid_disp_ba <- resid_disp_ba %>% 
  mutate(predicted = disp_ba_predicted$Estimate)

# residuals look ok given constrained response
par(mfrow=c(2,3))
with(resid_disp_ba, plot(resid ~ Environment));abline(h = 0, lty = 2)
with(resid_disp_ba, plot(resid ~ before_after));abline(h = 0, lty = 2)
with(resid_disp_ba, plot(resid ~ Study));abline(h = 0, lty = 2)
with(resid_disp_ba, plot(resid ~ Time_series));abline(h = 0, lty = 2)
with(resid_disp_ba, plot(resid ~ predicted));abline(h = 0, lty = 2)
with(resid_disp_ba, plot(predicted, value, ylab = 'Observed'));abline(c(0,1), lty = 2)

# population (fixed) effects
disp_ba_fixed <- fixef(disp_before_after_phi2)

# predictions at the population level (not including uncertainty associated with study-level variation)
disp_ba_fitted <- fitted(disp_before_after_phi2, re_formula = NA) %>% 
  as_tibble() %>% 
  # NB: estimate is predicted value, value is observed
  bind_cols(disp_ba_dat)

# expected richness in each environment before and after the experiment
disp_ba_fitted %>%
  distinct(Environment, before_after, Estimate, Q2.5, Q97.5)

# prelim plot
disp_ba_plot <- ggplot() +
  facet_wrap(~Environment) +
  # the data
  geom_point(data = disp_ba_dat,
             aes(x = before_after, y = value, group = Study, colour = Study),
             position = position_dodge(width = 0.5),
             size = 0.75) +
  # expected values for each environment from the model
  geom_point(data = disp_ba_fitted %>% 
               distinct(Environment, Estimate, before_after),
             aes(x = before_after, y = Estimate),
             size = 2) +
  geom_linerange(data = disp_ba_fitted %>% 
                   distinct(Environment, Estimate, Q2.5, Q97.5, before_after),
                 aes(x = before_after, ymin = Q2.5, ymax = Q97.5)) +
  labs(x = 'Disturbance',
       y = 'Dispersion (Bray-Curtis dissimilarity)') +
  scale_color_viridis_d(guide = FALSE) +
  theme_bw() +
  theme(axis.title = element_text(size = 10))

disp_ba_plot
