# results of the dispersion through time analyses: model fit to days < 50
# effort standardisation within each study

source('~/Dropbox/1current/microbiome-disturbance/code/00_init_dirs_load_packages.R')

load(paste0(path2wd, 'model-fits-within/z-disp-time-8091371.Rdata'))

# chains have mixed ok
mcmc_plot(zdisp_time_50_sigma2, type = 'trace')

# model reproduces data ok
pp_check(zdisp_time_50_sigma2)

# need time on the original scale (not centred), and also the min and max time for each study
dispersion <- read.delim(paste0(path2wd, 'data/dispersions.zscores-within.txt'),
                         sep = " ") %>% 
  as_tibble() %>% 
  # throw out controls, nudge ones, and add mean centred time
  filter(Rank != 1) %>% 
  mutate(value = ifelse(value==1, 0.999, value),
         # centre time for modelling
         cTime = Time - mean(Time)) 

dispersion_time50 <- dispersion %>% 
  filter(Rank > 1 & Time < 50) %>% 
  mutate(value = ifelse(value==1, 0.999, value),
         # centre time for modelling
         cTime = Time - mean(Time)) %>% 
  group_by(Time_series) %>% 
  mutate(n_obs = n_distinct(Time)) %>% 
  ungroup() %>% 
  filter(n_obs > 1)


# data in model object
disp_50_dat <- zdisp_time_50_sigma2$data %>% 
  as_tibble()

# residual check
resid_disp_50 <- residuals(zdisp_time_50_sigma2) %>% 
  as_tibble() %>% 
  bind_cols(disp_50_dat) %>% 
  rename(resid = Estimate) %>% 
  select(-Est.Error, -Q2.5, -Q97.5) 

disp_50_predicted <- predict(zdisp_time_50_sigma2) %>% 
  as_tibble()

# add predicted values to residual df 
resid_disp_50 <- resid_disp_50 %>% 
  mutate(predicted = disp_50_predicted$Estimate)

# residuals look ok
par(mfrow=c(2,3))
with(resid_disp_50, boxplot(resid ~ Environment));abline(h = 0, lty = 2)
with(resid_disp_50, plot(resid ~ cTime));abline(h = 0, lty = 2)
with(resid_disp_50, boxplot(resid ~ Study));abline(h = 0, lty = 2)
with(resid_disp_50, boxplot(resid ~ Time_series));abline(h = 0, lty = 2)
with(resid_disp_50, plot(resid ~ predicted));abline(h = 0, lty = 2) 
with(resid_disp_50, plot(nmodel.Zscores, predicted));abline(c(0,1), lty = 2)


# population (fixed) effects
disp_50_fixed <- fixef(zdisp_time_50_sigma2)

hypothesis(zdisp_time_50_sigma2, 'cTime + EnvironmentMammal:cTime = 0')

# predictions at the population level (not including uncertainty associated with study-level variation)
disp_50_fitted <- fitted(zdisp_time_50_sigma2, re_formula = NA, scale = 'response') %>% 
  as_tibble() %>% 
  # NB: estimate is predicted value, value is observed
  bind_cols(dispersion_time50)

disp_50_study <- coef(zdisp_time_50_sigma2)
disp_50_study2 <- bind_rows(
  tibble(Study = rownames(disp_50_study$Study),
         Environment = 'Aquatic',
         Intercept = disp_50_study$Study[,,'Intercept'][,'Estimate'],
         Slope = disp_50_study$Study[,,'cTime'][,'Estimate']
  ),
  tibble(Study = rownames(disp_50_study$Study),
         Environment = 'Mammal',
         Intercept = disp_50_study$Study[,,'Intercept'][,'Estimate'] + disp_50_study$Study[,,'EnvironmentMammal'][,'Estimate'],
         Slope = disp_50_study$Study[,,'cTime'][,'Estimate'] + disp_50_study$Study[,,'EnvironmentMammal:cTime'][,'Estimate']
  ),
  tibble(Study = rownames(disp_50_study$Study),
         Environment = 'Soil',
         Intercept = disp_50_study$Study[,,'Intercept'][,'Estimate'] + disp_50_study$Study[,,'EnvironmentSoil'][,'Estimate'],
         Slope = disp_50_study$Study[,,'cTime'][,'Estimate'] + disp_50_study$Study[,,'EnvironmentSoil:cTime'][,'Estimate']
  )
)

# what are the study-evironment combinations in the data
study_env <- disp_50_dat %>% 
  distinct(Study, Environment) %>% 
  # create filter
  unite(filter, c(Study, Environment))

disp_50_study2 <- disp_50_study2 %>% 
  unite(filter, c(Study, Environment), remove = F) %>% 
  filter(filter %in% study_env$filter) %>% 
  select(-filter)

# get study-level info for plotting
study_min_max <- dispersion %>% 
  filter(Time < 50) %>% 
  group_by(Study, Environment) %>% 
  summarise(Time = seq(from = min(Time), to = max(Time), length.out = 50),
            cTime = seq(from = min(cTime), to = max(cTime), length.out = 50)) %>% 
  ungroup()

disp_50_study2 <- left_join(disp_50_study2,
                            study_min_max)

# want time_series predictions too...
disp_50_ts <- dispersion_time50 %>% 
  group_by(Environment, Study, Time_series) %>% 
  summarise(Time = seq(from = min(Time), to = max(Time), by = 1),
            cTime = seq(from = min(cTime), to = max(cTime), by = 1)) %>% 
  ungroup() %>% 
  # get fitted values
  tidybayes::add_fitted_draws(model = zdisp_time_50_sigma2)


z_disp_50 <- ggplot() +
  facet_wrap(~Environment, scales = 'free_y') +
  # the data
  geom_point(data = dispersion_time50,
             aes(x = Time, y = nmodel.Zscores, group = Study, colour = Study),
             position = position_dodge(width = 0.5),
             size = 0.75, alpha = 0.5) +
  # expected values for each environment from the model
  geom_line(data = disp_50_fitted,
            aes(x = Time, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = disp_50_fitted,
              aes(x = Time, ymax = Q97.5, ymin = Q2.5),
              alpha = 0.5) +
  # study-level variation
  # geom_line(data = disp_50_study2,
  #           aes(x = Time,
  #               y = inv_logit_scaled(Intercept + Slope*cTime),
  #               colour = Study),
  #           size = 0.8) +
  # Time_series-level variation
  geom_line(data = disp_50_ts %>% 
              tidybayes::median_qi(),
            aes(x = Time,
                y = .value,
                colour = Study,
                group = Time_series),
            size = 0.8) +
  labs(x = 'Days since disturbance',
       y = 'Dispersion (z-score)') +
  scale_color_viridis_d(guide = FALSE, option = 'magma') +
  # scale_y_continuous(trans = 'logit') +
  theme_bw() +
  theme(axis.title = element_text(size = 10))

# need to have run code to produce before-after plot
cowplot::plot_grid(z_disp_ba_plot,
                   z_disp_50,
                   labels = 'auto', 
                   nrow = 2)
