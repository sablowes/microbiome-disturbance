# dispersion before after results
# within study standardisation
source('~/Dropbox/1current/microbiome-disturbance/code/00_init_dirs_load_packages.R')

load(paste0(path2wd, 'model-fits-within/z-disp-ba-8087013.Rdata'))

# these look ok
mcmc_plot(zdisp_before_after_sigma2, type = 'trace')

# model reproduces data reasonably
pp_check(zdisp_before_after_sigma2) 

# data model was fit to
disp_ba_dat <- zdisp_before_after_sigma2$data %>% 
  as_tibble()

# how many studies in each environment
disp_ba_dat %>% 
  group_by(Environment) %>% 
  summarise(n_study = n_distinct(Study))

# residual check
resid_disp_ba <- residuals(zdisp_before_after_sigma2) %>% 
  as_tibble() %>% 
  bind_cols(disp_ba_dat) %>% 
  rename(resid = Estimate) %>% 
  select(-Est.Error, -Q2.5, -Q97.5) 

disp_ba_predicted <- predict(zdisp_before_after_sigma2) %>% 
  as_tibble()

# add predicted values to residual df 
resid_disp_ba <- resid_disp_ba %>% 
  mutate(predicted = disp_ba_predicted$Estimate)

# residuals look ok (we've done the best we can with the residual heteroscedasticity)
par(mfrow=c(2,3))
with(resid_disp_ba, plot(resid ~ Environment));abline(h = 0, lty = 2) 
with(resid_disp_ba, plot(resid ~ before_after));abline(h = 0, lty = 2)
with(resid_disp_ba, plot(resid ~ Study));abline(h = 0, lty = 2)
with(resid_disp_ba, plot(resid ~ Time_series));abline(h = 0, lty = 2)
with(resid_disp_ba, plot(resid ~ predicted));abline(h = 0, lty = 2)
with(resid_disp_ba, plot(predicted, nmodel.Zscores, ylab = 'Observed'));abline(c(0,1), lty = 2)


# population (fixed) effects
zdisp_ba_fixed <- fixef(zdisp_before_after_sigma2)

# predictions at the population level (not including uncertainty associated with study-level variation)
disp_ba_fitted <- fitted(zdisp_before_after_sigma2, re_formula = NA) %>% 
  as_tibble() %>% 
  # NB: estimate is predicted value, value is observed
  bind_cols(disp_ba_dat)

# expected richness in each environment before and after the experiment
disp_ba_fitted %>%
  distinct(Environment, before_after, Estimate, Q2.5, Q97.5)

# prelim plot
z_disp_ba_plot <- ggplot() +
  facet_wrap(~Environment, scales = 'free_y') +
  # the data
  geom_point(data = disp_ba_dat,
             aes(x = before_after, y = nmodel.Zscores, group = Study, colour = Study),
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
       y = 'Dispersion (z-score)') +
  scale_color_viridis_d(guide = FALSE, option = 'magma') +
  theme_bw() +
  theme(axis.title = element_text(size = 10))
