# results of the resilience through time analyses: model fit to days < 50
# effort standardisation across all studies

source('~/Dropbox/1current/microbiome-disturbance/code/00_init_dirs_load_packages.R')

load(paste0(path2wd, 'model-fits-across/z-resil-time-6401876.Rdata'))

# looking for approximately uniform distributions in these...looks good to me
mcmc_plot(z.resil_time_50_sigma, type = 'trace')

pp_check(z.resil_time_50_sigma)

# inferences at the time series look ok:
pp_check(z.resil_time_50_sigma, type = 'scatter_avg_grouped', group = 'Study:Time_series') +
  geom_abline(intercept = 0, slope = 1, lty = 2)

pp_check(z.resil_time_50_sigma, type = 'stat_grouped', group = 'Study:Time_series')

# need time on the original scale (not centred), and also the min and max time for each study
z.resilience <- read.delim(paste0(path2wd, 'data/Resilience.zscores-across.txt'),
                           sep = " ") %>% 
  as_tibble()

z.resilience_time50 <- z.resilience %>% 
  filter(Time < 50) %>% 
  # 43 observations had complete turnover (dispersion = 1), nudge to 0.999 for simplicity,
  mutate(cTime = Time - mean(Time)) %>% 
  group_by(Time_series) %>% 
  mutate(n_obs = n_distinct(Time)) %>% 
  ungroup() %>% 
  filter(n_obs > 1) %>%
  mutate(cTime = Time - mean(Time))

# data in model object
resil_50_dat <- z.resil_time_50_sigma$data %>% 
  as_tibble()

# residual check
resid_resil_50 <- residuals(z.resil_time_50_sigma) %>% 
  as_tibble() %>% 
  bind_cols(resil_50_dat) %>% 
  rename(resid = Estimate) %>% 
  select(-Est.Error, -Q2.5, -Q97.5) 

resil_50_predicted <- predict(z.resil_time_50_sigma) %>% 
  as_tibble()

# add predicted values to residual df 
resid_resil_50 <- resid_resil_50 %>% 
  mutate(predicted = resil_50_predicted$Estimate)

# residuals look ok-ish
par(mfrow=c(2,3))
with(resid_resil_50, boxplot(resid ~ Environment));abline(h = 0, lty = 2)
with(resid_resil_50, plot(resid ~ cTime));abline(h = 0, lty = 2)
with(resid_resil_50, boxplot(resid ~ Study));abline(h = 0, lty = 2)
with(resid_resil_50, boxplot(resid ~ Time_series));abline(h = 0, lty = 2)
with(resid_resil_50, plot(resid ~ predicted));abline(h = 0, lty = 2)
with(resid_resil_50, plot(nmodel.Zscores, predicted));abline(c(0,1), lty = 2)

# population (fixed) effects: 
resil_50_fixed <- fixef(z.resil_time_50_sigma) %>% 
  as_tibble() %>% 
  mutate(par = rownames(fixef(z.resil_time_50_sigma)))

hypothesis(z.resil_time_50_sigma, 'cTime = 0')
hypothesis(z.resil_time_50_sigma, 'cTime + EnvironmentMammal:cTime = 0')
hypothesis(z.resil_time_50_sigma, 'cTime + EnvironmentSoil:cTime = 0')


# predictions at the population level (not including uncertainty associated with study-level variation)
resil_50_fitted <- fitted(z.resil_time_50_sigma, re_formula = NA, scale = 'response') %>% 
  as_tibble() %>% 
  # NB: estimate is predicted value, value is observed
  bind_cols(z.resilience_time50)

resil_50_study <- coef(z.resil_time_50_sigma)
resil_50_study2 <- bind_rows(
  tibble(Study = rownames(resil_50_study$Study),
         Environment = 'Aquatic',
         Intercept = resil_50_study$Study[,,'Intercept'][,'Estimate'],
         Slope = resil_50_study$Study[,,'cTime'][,'Estimate']
  ),
  tibble(Study = rownames(resil_50_study$Study),
         Environment = 'Mammal',
         Intercept = resil_50_study$Study[,,'Intercept'][,'Estimate'] + resil_50_study$Study[,,'EnvironmentMammal'][,'Estimate'],
         Slope = resil_50_study$Study[,,'cTime'][,'Estimate'] + resil_50_study$Study[,,'EnvironmentMammal:cTime'][,'Estimate']
  ),
  tibble(Study = rownames(resil_50_study$Study),
         Environment = 'Soil',
         Intercept = resil_50_study$Study[,,'Intercept'][,'Estimate'] + resil_50_study$Study[,,'EnvironmentSoil'][,'Estimate'],
         Slope = resil_50_study$Study[,,'cTime'][,'Estimate'] + resil_50_study$Study[,,'EnvironmentSoil:cTime'][,'Estimate']
  )
)

# what are the study-evironment combinations in the data
study_env <- z.resilience_time50 %>% 
  distinct(Study, Environment) %>% 
  # create filter
  unite(filter, c(Study, Environment))

resil_50_study2 <- resil_50_study2 %>% 
  unite(filter, c(Study, Environment), remove = F) %>% 
  filter(filter %in% study_env$filter) %>% 
  select(-filter)

# get study-level info for plotting (time was not centred for this model!)
study_min_max <- z.resilience_time50 %>% 
  group_by(Study, Environment) %>% 
  summarise(Time = seq(from = min(Time), to = max(Time), length.out = 50)) %>% 
  ungroup()

resil_50_study2 <- left_join(resil_50_study2,
                             study_min_max)

# want time_series predictions too...
resil_50_ts <- z.resilience_time50 %>% 
  group_by(Environment, Study, Time_series) %>% 
  summarise(cTime = seq(from = min(cTime), to = max(cTime), by = 1),
            Time = seq(from = min(Time), to = max(Time), by = 1)) %>% 
  ungroup() %>% 
  # get fitted values
  tidybayes::add_fitted_draws(model = z.resil_time_50_sigma)



ggplot() +
  facet_wrap(~Environment, scales = 'free_y') +
  geom_ribbon(data = resil_50_fitted,
              aes(x = Time, ymax = Q97.5, ymin = Q2.5), 
              alpha = 0.5) +
  # the data
  geom_point(data = z.resilience_time50,
             aes(x = Time, y = nmodel.Zscores, colour = Study),
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
              tidybayes::median_qi(),
            aes(x = Time, y = .value, group = Time_series, colour = Study),
            size = 0.5) +
  labs(x = 'Days since disturbance',
       y = 'Turnover (z-score)') +
  scale_color_viridis_d(guide = FALSE, option = 'magma') +
  # scale_y_continuous(trans = 'logit') +
  theme_bw() +
  theme(axis.title = element_text(size = 10))
