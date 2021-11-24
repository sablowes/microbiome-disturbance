# results of the turnover through time analyses: model fit to days < 50
# data standardised across studies
source('~/Dropbox/1current/microbiome-disturbance/code/00_init_dirs_load_packages.R')

load(paste0(path2wd, 'model-fits-across/resilience_time2-7612527.Rdata'))

# looking for approximately uniform distributions in these...looks good to me
mcmc_plot(resil_time_50_phi2_2, type = 'rank_hist')

# model reproduces the data well
pp_check(resil_time_50_phi2_2)


pp_check(resil_time_50_phi2_2, type = 'scatter_avg_grouped', group = 'Study:Time_series') +
  geom_abline(intercept = 0, slope = 1, lty = 2)

pp_check(resil_time_50_phi2_2, type = 'stat_grouped', group = 'Study:Time_series')


# need time on the original scale (not centred), and also the min and max time for each study
resil_dat <- read.delim(load(path2wd, 'data/Resilience-across.txt'),
                                      sep = " ") %>% 
  as_tibble() %>% 
  filter(Time < 50) %>% 
  # 43 observations had complete turnover (dispersion = 1), nudge to 0.999 for simplicity,
  mutate(value = ifelse(value==1, 0.999, value),
         # centre time for modelling
         cTime = Time - mean(Time)) %>% 
  group_by(Time_series) %>% 
  mutate(n_obs = n_distinct(Time)) %>% 
  ungroup() %>% 
  filter(n_obs > 1)

# data in model object
resil_50_dat <- resil_time_50_phi2_2$data %>% 
  as_tibble()

# residual check
resid_resil_50 <- residuals(resil_time_50_phi2_2) %>% 
  as_tibble() %>% 
  bind_cols(resil_50_dat) %>% 
  rename(resid = Estimate) %>% 
  select(-Est.Error, -Q2.5, -Q97.5) 

resil_50_predicted <- predict(resil_time_50_phi2_2) %>% 
  as_tibble()

# add predicted values to residual df 
resid_resil_50 <- resid_resil_50 %>% 
  mutate(predicted = resil_50_predicted$Estimate)

# residuals look ok
par(mfrow=c(2,3))
with(resid_resil_50, boxplot(resid ~ Environment));abline(h = 0, lty = 2)
with(resid_resil_50, plot(resid ~ Time));abline(h = 0, lty = 2)
with(resid_resil_50, boxplot(resid ~ Study));abline(h = 0, lty = 2)
with(resid_resil_50, boxplot(resid ~ Time_series));abline(h = 0, lty = 2)
with(resid_resil_50, plot(resid ~ predicted));abline(h = 0, lty = 2)
with(resid_resil_50, plot(value, predicted));abline(c(0,1), lty = 2)

# population (fixed) effects: 
resil_50_fixed <- fixef(resil_time_50_phi2_2) %>% 
  as_tibble() %>% 
  mutate(par = rownames(fixef(resil_time_50_phi2_2)))

hypothesis(resil_time_50_phi2_2, 'Time = 0')
hypothesis(resil_time_50_phi2_2, 'Time + EnvironmentMammal:Time = 0')
hypothesis(resil_time_50_phi2_2, 'Time + EnvironmentSoil:Time = 0')

# predictions at the population level (not including uncertainty associated with study-level variation)
resil_50_fitted <- fitted(resil_time_50_phi2_2, re_formula = NA, scale = 'response') %>% 
  as_tibble() %>% 
  # NB: estimate is predicted value, value is observed
  bind_cols(resil_dat)

resil_50_study <- coef(resil_time_50_phi2_2)
resil_50_study2 <- bind_rows(
  tibble(Study = rownames(resil_50_study$Study),
         Environment = 'Aquatic',
         Intercept = resil_50_study$Study[,,'Intercept'][,'Estimate'],
         Slope = resil_50_study$Study[,,'Time'][,'Estimate']
  ),
  tibble(Study = rownames(resil_50_study$Study),
         Environment = 'Mammal',
         Intercept = resil_50_study$Study[,,'Intercept'][,'Estimate'] + resil_50_study$Study[,,'EnvironmentMammal'][,'Estimate'],
         Slope = resil_50_study$Study[,,'Time'][,'Estimate'] + resil_50_study$Study[,,'EnvironmentMammal:Time'][,'Estimate']
  ),
  tibble(Study = rownames(resil_50_study$Study),
         Environment = 'Soil',
         Intercept = resil_50_study$Study[,,'Intercept'][,'Estimate'] + resil_50_study$Study[,,'EnvironmentSoil'][,'Estimate'],
         Slope = resil_50_study$Study[,,'Time'][,'Estimate'] + resil_50_study$Study[,,'EnvironmentSoil:Time'][,'Estimate']
  )
)

# what are the study-evironment combinations in the data
study_env <- resil_50_dat %>% 
  distinct(Study, Environment) %>% 
  # create filter
  unite(filter, c(Study, Environment))

resil_50_study2 <- resil_50_study2 %>% 
  unite(filter, c(Study, Environment), remove = F) %>% 
  filter(filter %in% study_env$filter) %>% 
  select(-filter)

# get study-level info for plotting (time was not centred for this model!)
study_min_max <- resil_50_dat %>% 
  group_by(Study, Environment) %>% 
  summarise(Time = seq(from = min(Time), to = max(Time), length.out = 50)) %>% 
  ungroup()

resil_50_study2 <- left_join(resil_50_study2,
                              study_min_max)

# want time_series predictions too...
resil_50_ts <- resil_50_dat %>% 
  group_by(Environment, Study, Time_series) %>% 
  summarise(Time = seq(from = min(Time), to = max(Time), by = 1)) %>% 
  ungroup() %>% 
  # get fitted values
  tidybayes::add_fitted_draws(model = resil_time_50_phi2_2)

# ok, on average aquatic studies move away from controls (less resilient),
# no overall trend in mammals or soil.
ggplot() +
  facet_wrap(~Environment) +
  geom_ribbon(data = resil_50_fitted,
              aes(x = Time, ymax = Q97.5, ymin = Q2.5), 
              alpha = 0.5) +
  # the data
  geom_point(data = resil_50_dat,
             aes(x = Time, y = value, colour = Study),
             # position = position_dodge(width = 0.5),
             size = 0.75, alpha = 0.5) +
  # expected values for each environment from the model
  geom_line(data = resil_50_fitted,
            aes(x = Time, y = Estimate),
            size = 1.5) +
  # study-level variation
  # geom_line(data = resil_50_study2,
  #              aes(x = Time,
  #                  y = inv_logit_scaled(Intercept + Slope*Time),
  #                  colour = Study),
  #              size = 0.8) +
  # time series variation
  geom_line(data = resil_50_ts %>% 
              median_qi(),
            aes(x = Time, y = .value, group = Time_series, colour = Study),
            size = 0.5) +
  labs(x = 'Time [days]',
       y = 'Compositional change from pre-disturbance [Bray Curtis distance]') +
  scale_color_viridis_d(guide = FALSE) +
  # scale_y_continuous(trans = 'logit') +
  theme_bw() +
  theme(axis.title = element_text(size = 10))
