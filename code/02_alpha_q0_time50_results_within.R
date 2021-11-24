# within study standardisation
# results of alpha richness time analyses: model fit to days < 50

source('~/Dropbox/1current/microbiome-disturbance/code/00_init_dirs_load_packages.R')

load(paste0(path2wd, 'model-fits-within/alpha_time_q0-8086840.Rdata'))

# focus on models fit to first 50 days
mcmc_plot(alpha_q0_time_50_nbinom2_shape, type = 'trace')

# model reproduces data well
pp_check(alpha_q0_time_50_nbinom2_shape)

pp_check(alpha_q0_time_50_nbinom2_shape, type = 'stat_grouped', group = 'Study:Time_series') +
  theme(strip.text = element_text(size = 8))

# need time on the original scale (not centred), and also the min and max time for each study
# for results plots
alpha_dat <- read.delim(paste0(path2wd, 'data/Rich-within.txt'),
                        sep = " ") %>% 
  as_tibble() %>% 
  # throw out controls, remove observations beyond 50 days, and add mean centred time
  filter(Rank > 1 & Time_since_dist < 50) %>% 
  mutate(cTime = Time_since_dist - mean(Time_since_dist)) %>% 
  group_by(Time_series) %>% 
  mutate(n_obs = n_distinct(Time_since_dist)) %>% 
  ungroup() %>% 
  filter(n_obs > 1) 


# data in model object
alpha_50_dat <- alpha_q0_time_50_nbinom2_shape$data %>% 
  as_tibble()

# residual check
resid_alpha_50 <- residuals(alpha_q0_time_50_nbinom2_shape) %>% 
  as_tibble() %>% 
  bind_cols(alpha_50_dat) %>% 
  rename(resid = Estimate) %>% 
  select(-Est.Error, -Q2.5, -Q97.5) 

alpha_50_predicted <- predict(alpha_q0_time_50_nbinom2_shape) %>% 
  as_tibble()

# add predicted values to residual df 
resid_alpha_50 <- resid_alpha_50 %>% 
  mutate(predicted = alpha_50_predicted$Estimate)

# residuals look ok
par(mfrow=c(2,3))
with(resid_alpha_50, boxplot(resid ~ Environment));abline(h = 0, lty = 2)
with(resid_alpha_50, plot(resid ~ cTime));abline(h = 0, lty = 2)
with(resid_alpha_50, boxplot(resid ~ Study));abline(h = 0, lty = 2)
with(resid_alpha_50, boxplot(resid ~ Time_series));abline(h = 0, lty = 2)
with(resid_alpha_50, plot(resid ~ predicted));abline(h = 0, lty = 2) 
with(resid_alpha_50, plot(Observed, predicted));abline(c(0,1), lty = 2)

# population (fixed) effects
alpha_50_fixed <- fixef(alpha_q0_time_50_nbinom2_shape)

# predictions at the population level (not including uncertainty associated with study-level variation)
alpha_50_fitted <- fitted(alpha_q0_time_50_nbinom2_shape, re_formula = NA, scale = 'response') %>% 
  as_tibble() %>% 
  # NB: estimate is predicted value, value is observed
  bind_cols(alpha_dat)

# calculate slopes and CIs for each environment
hypothesis(alpha_q0_time_50_nbinom2_shape, 'cTime = 0') # 
hypothesis(alpha_q0_time_50_nbinom2_shape, 'cTime + EnvironmentMammal:cTime = 0')
hypothesis(alpha_q0_time_50_nbinom2_shape, 'cTime + EnvironmentSoil:cTime = 0')

alpha_50_study <- coef(alpha_q0_time_50_nbinom2_shape)
alpha_50_study2 <- bind_rows(
  tibble(Study = rownames(alpha_50_study$Study),
         Environment = 'Aquatic',
         Intercept = alpha_50_study$Study[,,'Intercept'][,'Estimate'],
         Slope = alpha_50_study$Study[,,'cTime'][,'Estimate']
  ),
  tibble(Study = rownames(alpha_50_study$Study),
         Environment = 'Mammal',
         Intercept = alpha_50_study$Study[,,'Intercept'][,'Estimate'] + alpha_50_study$Study[,,'EnvironmentMammal'][,'Estimate'],
         Slope = alpha_50_study$Study[,,'cTime'][,'Estimate'] + alpha_50_study$Study[,,'EnvironmentMammal:cTime'][,'Estimate']
  ),
  tibble(Study = rownames(alpha_50_study$Study),
         Environment = 'Soil',
         Intercept = alpha_50_study$Study[,,'Intercept'][,'Estimate'] + alpha_50_study$Study[,,'EnvironmentSoil'][,'Estimate'],
         Slope = alpha_50_study$Study[,,'cTime'][,'Estimate'] + alpha_50_study$Study[,,'EnvironmentSoil:cTime'][,'Estimate']
  )
)

# what are the study-evironment combinations in the data
study_env <- alpha_50_dat %>% 
  distinct(Study, Environment) %>% 
  # create filter
  unite(filter, c(Study, Environment))

alpha_50_study2 <- alpha_50_study2 %>% 
  unite(filter, c(Study, Environment), remove = F) %>% 
  filter(filter %in% study_env$filter) %>% 
  select(-filter)

# get study-level info for plotting
study_min_max <- alpha_dat %>% 
  group_by(Study, Environment) %>% 
  summarise(Time = seq(from = min(Time_since_dist), to = max(Time_since_dist), length.out = 50),
            cTime = seq(from = min(cTime), to = max(cTime), length.out = 50)) %>% 
  ungroup()

alpha_50_study2 <- left_join(alpha_50_study2,
                             study_min_max)

# want time_series predictions too...
alpha_50_ts <- alpha_dat %>% 
  group_by(Environment, Study, Time_series) %>% 
  summarise(Time = seq(from = min(Time_since_dist), to = max(Time_since_dist), by = 1),
            cTime = seq(from = min(cTime), to = max(cTime), by = 1)) %>% 
  ungroup() %>% 
  # get fitted values
  tidybayes::add_fitted_draws(model = alpha_q0_time_50_nbinom2_shape)

alpha_t50_plot <- ggplot() +
  facet_wrap(~Environment) +
  # the data
  geom_point(data = alpha_dat,
             aes(x = Time_since_dist, y = Observed, group = Study, colour = Study),
             position = position_dodge(width = 0.5),
             size = 0.75, alpha = 0.5) +
  # expected values for each environment from the model
  geom_line(data = alpha_50_fitted,
            aes(x = Time_since_dist, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = alpha_50_fitted,
              aes(x = Time_since_dist, ymax = Q97.5, ymin = Q2.5),
              alpha = 0.5) +
  # study-level variation
  # geom_line(data = alpha_50_study2,
  #           aes(x = Time,
  #               y = exp(Intercept + Slope*cTime),
  #               colour = Study),
  #           size = 0.8) +
  # time_series level variation
  geom_line(data = alpha_50_ts %>% 
              median_qi(),
            aes(x = Time,
                y = .value,
                colour = Study,
                group = Time_series),
            size = 0.75) +
  labs(x = 'Days since disturbance',
       y = 'Richness') +
  scale_color_viridis_d(guide = FALSE, option = 'magma') +
  scale_y_continuous(trans = 'log2') +
  theme_bw() +
  theme(axis.title = element_text(size = 10))


# need to run code to produce before-after plot
cowplot::plot_grid(alpha_ba_plot,
                   alpha_t50_plot,
                   labels = 'auto',
                   nrow = 2)
