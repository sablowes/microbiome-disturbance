# compare results of models fit to within- versus across-study effort standardisations

source('~/Dropbox/1current/microbiome-disturbance/code/00_init_dirs_load_packages.R')

# across study standardisation
load(paste0(path2wd, 'model-fits-across/alpha_ba2-7386305.Rdata'))

alpha_ba_across = alpha_q0_ba_nbinom_shape
rm(alpha_q0_ba_nbinom_shape)

# within study standardisation
load(paste0(path2wd, 'model-fits-within/alpha_ba2-8086862.Rdata'))
alpha_ba_within = alpha_q0_ba_nbinom_shape
rm(alpha_q0_ba_nbinom_shape)

fixef_across <- fixef(alpha_ba_across) %>% 
  as_tibble() %>% 
  rename(estimate_across = Estimate,
         across_q2.5 = Q2.5,
         across_q97.5 = Q97.5) %>% 
  mutate(par = rownames(fixef(alpha_ba_across)))

fixef_within <- fixef(alpha_ba_within) %>% 
  as_tibble() %>% 
  rename(estimate_within = Estimate,
         within_q2.5 = Q2.5,
         within_q97.5 = Q97.5) %>% 
  mutate(par = rownames(fixef(alpha_ba_within)))

# seven colour palette to use for parameters
# will need to modify slightly to make colours consistent for each model 
# (parameters and their names varying slightly between models)
par_pal <- left_join(fixef_across, fixef_within,
          by = 'par') %>% 
  distinct(par) %>% 
  mutate(par = factor(par,
                      levels = c('Intercept',
                                 'EnvironmentMammal',
                                 'EnvironmentSoil',
                                 'before_afterafter',
                                 'EnvironmentMammal:before_afterafter',
                                 'EnvironmentSoil:before_afterafter',
                                 'shape_Intercept'))) %>% 
  arrange(par) %>% 
  mutate(col = c('#ffffcc',
             '#d9f0a3',
             '#addd8e',
             '#78c679',
             '#41ab5d',
             '#238443',
             '#005a32')) 


# no qualitative differences between the standardisations for alpha richness response to disturbance

estimates <-   left_join(fixef_across, fixef_within,
          by = 'par') %>% 
  mutate(par = factor(par,
                      levels = c('Intercept',
                                 'EnvironmentMammal',
                                 'EnvironmentSoil',
                                 'before_afterafter',
                                 'EnvironmentMammal:before_afterafter',
                                 'EnvironmentSoil:before_afterafter',
                                 'shape_Intercept'))) %>% 
  arrange(par)


alpha_ba_compare <- ggplot() +
  geom_point(data = estimates,
             aes(x = estimate_across, y = estimate_within, colour = par)) +
  geom_linerange(data = estimates,
                 aes(x = estimate_across, ymin = within_q2.5, ymax = within_q97.5,
                     colour = par)) +
  geom_linerange(data = estimates,
                 aes(xmin = across_q2.5, xmax = across_q97.5, y = estimate_within, 
                     colour = par)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  labs(subtitle = expression(paste(alpha, '-scale richness response to disturbance'))) +
  scale_color_manual(name = 'Parameter',
                     values = deframe(par_pal)) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        legend.background = element_blank())


## repeat for alpha richness through time following disturbance
load(paste0(path2wd, 'model-fits-across/alpha_time_q0-7612517.Rdata'))
alpha_t50_across = alpha_q0_time_50_nbinom2_shape
rm(alpha_q0_time_50_nbinom2_shape)

load(paste0(path2wd, 'model-fits-within/alpha_time_q0-8086840.Rdata'))
alpha_t50_within = alpha_q0_time_50_nbinom2_shape
rm(alpha_q0_time_50_nbinom2_shape)

fixef_across = fixef(alpha_t50_across) %>% 
  as_tibble() %>% 
  rename(estimate_across = Estimate,
         across_q2.5 = Q2.5,
         across_q97.5 = Q97.5) %>% 
  mutate(par = rownames(fixef(alpha_t50_across)))

fixef_within <- fixef(alpha_t50_within) %>% 
  as_tibble() %>% 
  rename(estimate_within = Estimate,
         within_q2.5 = Q2.5,
         within_q97.5 = Q97.5) %>% 
  mutate(par = rownames(fixef(alpha_t50_within)))

# seven colour palette to use for parameters
# will need to modify slightly to make colours consistent for each model 
# (parameters and their names varying slightly between models)
par_pal <- left_join(fixef_across, fixef_within,
                     by = 'par') %>% 
  distinct(par) %>% 
  mutate(par = factor(par,
                      levels = c('Intercept',
                                 'EnvironmentMammal',
                                 'EnvironmentSoil',
                                 'cTime',
                                 'EnvironmentMammal:cTime',
                                 'EnvironmentSoil:cTime',
                                 'shape_Intercept'))) %>% 
  arrange(par) %>% 
  mutate(col = c('#ffffcc',
                 '#d9f0a3',
                 '#addd8e',
                 '#78c679',
                 '#41ab5d',
                 '#238443',
                 '#005a32')) 


estimates <-   left_join(fixef_across, fixef_within,
                         by = 'par') %>% 
  mutate(par = factor(par,
                      levels = c('Intercept',
                                 'EnvironmentMammal',
                                 'EnvironmentSoil',
                                 'cTime',
                                 'EnvironmentMammal:cTime',
                                 'EnvironmentSoil:cTime',
                                 'shape_Intercept'))) %>% 
  arrange(par)


alpha_time_compare <- ggplot() +
geom_point(data = estimates,
           aes(x = estimate_across, y = estimate_within, colour = par)) +
  geom_linerange(data = estimates,
                 aes(x = estimate_across, ymin = within_q2.5, ymax = within_q97.5,
                     colour = par)) +
  geom_linerange(data = estimates,
                 aes(xmin = across_q2.5, xmax = across_q97.5, y = estimate_within, 
                     colour = par)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  labs(subtitle = expression(paste(alpha, '-scale richness change following disturbance'))) +
  scale_color_manual(name = 'Parameter',
                     values = deframe(par_pal)) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        legend.background = element_blank())


## repeat for dispersion before and after disturbance
load(paste0(path2wd, 'model-fits-across/dispersion_ba2-7612519.Rdata'))

disp_ba_across = disp_before_after_2_phi2
rm(disp_before_after_2_phi2)

# within study standardisation
load(paste0(path2wd, 'model-fits-within/disp-ba-8086863.Rdata'))

disp_ba_within = disp_before_after_phi2
rm(disp_before_after_phi2)

fixef_across <- fixef(disp_ba_across) %>% 
  as_tibble() %>% 
  rename(estimate_across = Estimate,
         across_q2.5 = Q2.5,
         across_q97.5 = Q97.5) %>% 
  mutate(par = rownames(fixef(disp_ba_across)))

fixef_within <- fixef(disp_ba_within) %>% 
  as_tibble() %>% 
  rename(estimate_within = Estimate,
         within_q2.5 = Q2.5,
         within_q97.5 = Q97.5) %>% 
  mutate(par = rownames(fixef(disp_ba_within)))


# seven colour palette to use for parameters
# will need to modify slightly to make colours consistent for each model 
# (parameters and their names varying slightly between models)
par_pal <- left_join(fixef_across, fixef_within,
                     by = 'par') %>% 
  distinct(par) %>% 
  mutate(par = factor(par,
                      levels = c('Intercept',
                                 'EnvironmentMammal',
                                 'EnvironmentSoil',
                                 'before_afterafter',
                                 'EnvironmentMammal:before_afterafter',
                                 'EnvironmentSoil:before_afterafter',
                                 'phi_Intercept'))) %>% 
  arrange(par) %>% 
  mutate(col = c('#ffffcc',
                 '#d9f0a3',
                 '#addd8e',
                 '#78c679',
                 '#41ab5d',
                 '#238443',
                 '#005a32')) 


estimates <-   left_join(fixef_across, fixef_within,
                         by = 'par') %>% 
  mutate(par = factor(par,
                      levels = c('Intercept',
                                 'EnvironmentMammal',
                                 'EnvironmentSoil',
                                 'before_afterafter',
                                 'EnvironmentMammal:before_afterafter',
                                 'EnvironmentSoil:before_afterafter',
                                 'phi_Intercept'))) %>% 
  arrange(par)


disp_ba_compare <- ggplot() +
  geom_point(data = estimates,
             aes(x = estimate_across, y = estimate_within, colour = par)) +
  geom_linerange(data = estimates,
                 aes(x = estimate_across, ymin = within_q2.5, ymax = within_q97.5,
                     colour = par)) +
  geom_linerange(data = estimates,
                 aes(xmin = across_q2.5, xmax = across_q97.5, y = estimate_within, 
                     colour = par)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  labs(subtitle = expression(paste('Dispersion response to disturbance'))) +
  scale_color_manual(name = 'Parameter',
                     values = deframe(par_pal)) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        legend.background = element_blank())

## repeat for z-dispersion before and after disturbance
load(paste0(path2wd, 'model-fits-across/z-disp-ba-6280436.Rdata'))

disp_ba_across = zdisp_before_after_sigma2
rm(zdisp_before_after_sigma2)

# within study standardisation
load(paste0(path2wd, 'model-fits-within/z-disp-ba-8087013.Rdata'))

disp_ba_within = zdisp_before_after_sigma2
rm(zdisp_before_after_sigma2)

fixef_across <- fixef(disp_ba_across) %>% 
  as_tibble() %>% 
  rename(estimate_across = Estimate,
         across_q2.5 = Q2.5,
         across_q97.5 = Q97.5) %>% 
  mutate(par = rownames(fixef(disp_ba_across)))

fixef_within <- fixef(disp_ba_within) %>% 
  as_tibble() %>% 
  rename(estimate_within = Estimate,
         within_q2.5 = Q2.5,
         within_q97.5 = Q97.5) %>% 
  mutate(par = rownames(fixef(disp_ba_within)))


# seven colour palette to use for parameters
# will need to modify slightly to make colours consistent for each model 
# (parameters and their names varying slightly between models)
par_pal <- left_join(fixef_across, fixef_within,
                     by = 'par') %>% 
  distinct(par) %>% 
  mutate(par = factor(par,
                      levels = c('Intercept',
                                 'EnvironmentMammal',
                                 'EnvironmentSoil',
                                 'before_afterafter',
                                 'EnvironmentMammal:before_afterafter',
                                 'EnvironmentSoil:before_afterafter',
                                 'sigma_EnvironmentAquatic',
                                 'sigma_EnvironmentMammal',
                                 'sigma_EnvironmentSoil'))) %>% 
  arrange(par) %>% 
  mutate(col = c('#ffffe5',
                 '#f7fcb9',
                 '#d9f0a3',
                 '#addd8e',
                 '#78c679',
                 '#41ab5d',
                 '#238443',
                 '#006837',
                 '#004529')) 


estimates <-   left_join(fixef_across, fixef_within,
                         by = 'par') %>% 
  mutate(par = factor(par,
                      levels = c('Intercept',
                                 'EnvironmentMammal',
                                 'EnvironmentSoil',
                                 'before_afterafter',
                                 'EnvironmentMammal:before_afterafter',
                                 'EnvironmentSoil:before_afterafter',
                                 'sigma_EnvironmentAquatic',
                                 'sigma_EnvironmentMammal',
                                 'sigma_EnvironmentSoil'))) %>% 
  arrange(par)


zdisp_ba_compare <- ggplot() +
  geom_point(data = estimates %>% 
               # drop the parameters for residuals
               filter(!par %in% c('sigma_EnvironmentAquatic',
                                 'sigma_EnvironmentMammal',
                                 'sigma_EnvironmentSoil')),
             aes(x = estimate_across, y = estimate_within, colour = par)) +
  geom_linerange(data = estimates %>% 
                   filter(!par %in% c('sigma_EnvironmentAquatic',
                                      'sigma_EnvironmentMammal',
                                      'sigma_EnvironmentSoil')),
                 aes(x = estimate_across, ymin = within_q2.5, ymax = within_q97.5,
                     colour = par)) +
  geom_linerange(data = estimates %>% 
                   filter(!par %in% c('sigma_EnvironmentAquatic',
                                      'sigma_EnvironmentMammal',
                                      'sigma_EnvironmentSoil')),
                 aes(xmin = across_q2.5, xmax = across_q97.5, y = estimate_within, 
                     colour = par)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  labs(subtitle = expression(paste('Dispersion (z-score) response to disturbance'))) +
  scale_color_manual(name = 'Parameter',
                     values = deframe(par_pal)) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        legend.background = element_blank())

## repeat for dispersion through time following disturbance
load(paste0(path2wd, 'model-fits-across/dispersion_time2-7612520.Rdata'))

disp_t50_across = disp_time_50_2_phi2
rm(disp_time_50_2_phi2)

# within study standardisation
load(paste0(path2wd, 'model-fits-within/disp-time-8106678.Rdata'))

disp_t50_within = disp_time_50_2_phi
rm(disp_time_50_2_phi)

fixef_across <- fixef(disp_t50_across) %>% 
  as_tibble() %>% 
  rename(estimate_across = Estimate,
         across_q2.5 = Q2.5,
         across_q97.5 = Q97.5) %>% 
  mutate(par = rownames(fixef(disp_t50_across)))

fixef_within <- fixef(disp_t50_within) %>% 
  as_tibble() %>% 
  rename(estimate_within = Estimate,
         within_q2.5 = Q2.5,
         within_q97.5 = Q97.5) %>% 
  mutate(par = rownames(fixef(disp_t50_within)))


# seven colour palette to use for parameters
# will need to modify slightly to make colours consistent for each model 
# (parameters and their names varying slightly between models)
par_pal <- left_join(fixef_across, fixef_within,
                     by = 'par') %>% 
  distinct(par) %>% 
  mutate(par = factor(par,
                      levels = c('Intercept',
                                 'EnvironmentMammal',
                                 'EnvironmentSoil',
                                 'cTime',
                                 'EnvironmentMammal:cTime',
                                 'EnvironmentSoil:cTime',
                                 'phi_Intercept'))) %>% 
  arrange(par) %>% 
  mutate(col = c('#ffffcc',
                 '#d9f0a3',
                 '#addd8e',
                 '#78c679',
                 '#41ab5d',
                 '#238443',
                 '#005a32')) 


estimates <-   left_join(fixef_across, fixef_within,
                         by = 'par') %>% 
  mutate(par = factor(par,
                      levels = c('Intercept',
                                 'EnvironmentMammal',
                                 'EnvironmentSoil',
                                 'cTime',
                                 'EnvironmentMammal:cTime',
                                 'EnvironmentSoil:cTime',
                                 'phi_Intercept'))) %>% 
  arrange(par)


disp_time_compare <- ggplot() +
  geom_point(data = estimates,
             aes(x = estimate_across, y = estimate_within, colour = par)) +
  geom_linerange(data = estimates,
                 aes(x = estimate_across, ymin = within_q2.5, ymax = within_q97.5,
                     colour = par)) +
  geom_linerange(data = estimates,
                 aes(xmin = across_q2.5, xmax = across_q97.5, y = estimate_within, 
                     colour = par)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  labs(subtitle = expression(paste('Dispersion change following disturbance'))) +
  scale_color_manual(name = 'Parameter',
                     values = deframe(par_pal)) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        legend.background = element_blank())

## repeat for z-dispersion through time following disturbance
load(paste0(path2wd, 'model-fits-across/z-disp-time-6077323.Rdata'))

disp_t50_across = zdisp_time_50_sigma2
rm(zdisp_time_50_sigma2)

# within study standardisation
load(paste0(path2wd, 'model-fits-within/z-disp-time-8091371.Rdata'))

disp_t50_within = zdisp_time_50_sigma2
rm(zdisp_time_50_sigma2)

fixef_across <- fixef(disp_t50_across) %>% 
  as_tibble() %>% 
  rename(estimate_across = Estimate,
         across_q2.5 = Q2.5,
         across_q97.5 = Q97.5) %>% 
  mutate(par = rownames(fixef(disp_t50_across)))

fixef_within <- fixef(disp_t50_within) %>% 
  as_tibble() %>% 
  rename(estimate_within = Estimate,
         within_q2.5 = Q2.5,
         within_q97.5 = Q97.5) %>% 
  mutate(par = rownames(fixef(disp_t50_within)))


# seven colour palette to use for parameters
# will need to modify slightly to make colours consistent for each model 
# (parameters and their names varying slightly between models)
par_pal <- left_join(fixef_across, fixef_within,
                     by = 'par') %>% 
  distinct(par) %>% 
  mutate(par = factor(par,
                      levels = c('Intercept',
                                 'EnvironmentMammal',
                                 'EnvironmentSoil',
                                 'cTime',
                                 'EnvironmentMammal:cTime',
                                 'EnvironmentSoil:cTime',
                                 'sigma_EnvironmentAquatic',
                                 'sigma_EnvironmentMammal',
                                 'sigma_EnvironmentSoil'))) %>% 
  arrange(par) %>% 
  mutate(col = c('#ffffe5',
                 '#f7fcb9',
                 '#d9f0a3',
                 '#addd8e',
                 '#78c679',
                 '#41ab5d',
                 '#238443',
                 '#006837',
                 '#004529')) 


estimates <-   left_join(fixef_across, fixef_within,
                         by = 'par') %>% 
  mutate(par = factor(par,
                      levels = c('Intercept',
                                 'EnvironmentMammal',
                                 'EnvironmentSoil',
                                 'cTime',
                                 'EnvironmentMammal:cTime',
                                 'EnvironmentSoil:cTime',
                                 'sigma_EnvironmentAquatic',
                                 'sigma_EnvironmentMammal',
                                 'sigma_EnvironmentSoil'))) %>% 
  arrange(par)


zdisp_time_compare <- ggplot() +
  geom_point(data = estimates %>% 
               filter(!par %in% c('sigma_EnvironmentAquatic',
                                  'sigma_EnvironmentMammal',
                                  'sigma_EnvironmentSoil')),
             aes(x = estimate_across, y = estimate_within, colour = par)) +
  geom_linerange(data = estimates %>% 
                   filter(!par %in% c('sigma_EnvironmentAquatic',
                                      'sigma_EnvironmentMammal',
                                      'sigma_EnvironmentSoil')),
                 aes(x = estimate_across, ymin = within_q2.5, ymax = within_q97.5,
                     colour = par)) +
  geom_linerange(data = estimates %>% 
                   filter(!par %in% c('sigma_EnvironmentAquatic',
                                      'sigma_EnvironmentMammal',
                                      'sigma_EnvironmentSoil')),
                 aes(xmin = across_q2.5, xmax = across_q97.5, y = estimate_within, 
                     colour = par)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  labs(subtitle = expression(paste('Dispersion (z-score) change following disturbance'))) +
  scale_color_manual(name = 'Parameter',
                     values = deframe(par_pal)) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        legend.background = element_blank())


## repeat for resilience through time following disturbance
load(paste0(path2wd, 'model-fits-across/resilience_time2-7612527.Rdata'))

resil_t50_across = resil_time_50_phi2_2
rm(resil_time_50_phi2_2)

# within study standardisation
load(paste0(path2wd, 'model-fits-within/resil-time-8087032.Rdata'))

resil_t50_within = resil_time_50_phi2_2
rm(resil_time_50_phi2_2)

fixef_across <- fixef(resil_t50_across) %>% 
  as_tibble() %>% 
  rename(estimate_across = Estimate,
         across_q2.5 = Q2.5,
         across_q97.5 = Q97.5) %>% 
  mutate(par = rownames(fixef(resil_t50_across)))

fixef_within <- fixef(resil_t50_within) %>% 
  as_tibble() %>% 
  rename(estimate_within = Estimate,
         within_q2.5 = Q2.5,
         within_q97.5 = Q97.5) %>% 
  mutate(par = rownames(fixef(resil_t50_within)))


# seven colour palette to use for parameters
# will need to modify slightly to make colours consistent for each model 
# (parameters and their names varying slightly between models)
par_pal <- left_join(fixef_across, fixef_within,
                     by = 'par') %>% 
  distinct(par) %>% 
  mutate(par = factor(par,
                      levels = c('Intercept',
                                 'EnvironmentMammal',
                                 'EnvironmentSoil',
                                 'Time',
                                 'EnvironmentMammal:Time',
                                 'EnvironmentSoil:Time',
                                 'phi_Intercept'))) %>% 
  arrange(par) %>% 
  mutate(col = c('#ffffcc',
                 '#d9f0a3',
                 '#addd8e',
                 '#78c679',
                 '#41ab5d',
                 '#238443',
                 '#005a32')) 


estimates <-   left_join(fixef_across, fixef_within,
                         by = 'par') %>% 
  mutate(par = factor(par,
                      levels = c('Intercept',
                                 'EnvironmentMammal',
                                 'EnvironmentSoil',
                                 'Time',
                                 'EnvironmentMammal:Time',
                                 'EnvironmentSoil:Time',
                                 'phi_Intercept'))) %>% 
  arrange(par)


resil_time_compare <- ggplot() +
  geom_point(data = estimates,
             aes(x = estimate_across, y = estimate_within, colour = par)) +
  geom_linerange(data = estimates,
                 aes(x = estimate_across, ymin = within_q2.5, ymax = within_q97.5,
                     colour = par)) +
  geom_linerange(data = estimates,
                 aes(xmin = across_q2.5, xmax = across_q97.5, y = estimate_within, 
                     colour = par)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  labs(subtitle = expression(paste('Turnover change following disturbance'))) +
  scale_color_manual(name = 'Parameter',
                     values = deframe(par_pal)) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        legend.background = element_blank())

## repeat for z-resilience through time following disturbance
load(paste0(path2wd, 'model-fits-across/z-resil-time-6401876.Rdata'))

resil_t50_across = z.resil_time_50_sigma
rm(z.resil_time_50_sigma)

# within study standardisation
load(paste0(path2wd, 'model-fits-within/z-resil-time-8087033.Rdata'))

resil_t50_within = z.resil_time_50_sigma
rm(z.resil_time_50_sigma)

fixef_across <- fixef(resil_t50_across) %>% 
  as_tibble() %>% 
  rename(estimate_across = Estimate,
         across_q2.5 = Q2.5,
         across_q97.5 = Q97.5) %>% 
  mutate(par = rownames(fixef(resil_t50_across)))

fixef_within <- fixef(resil_t50_within) %>% 
  as_tibble() %>% 
  rename(estimate_within = Estimate,
         within_q2.5 = Q2.5,
         within_q97.5 = Q97.5) %>% 
  mutate(par = rownames(fixef(resil_t50_within)))


# seven colour palette to use for parameters
# will need to modify slightly to make colours consistent for each model 
# (parameters and their names varying slightly between models)
par_pal <- left_join(fixef_across, fixef_within,
                     by = 'par') %>% 
  distinct(par) %>% 
  mutate(par = factor(par,
                      levels = c('Intercept',
                                 'EnvironmentMammal',
                                 'EnvironmentSoil',
                                 'cTime',
                                 'EnvironmentMammal:cTime',
                                 'EnvironmentSoil:cTime',
                                 'sigma_EnvironmentAquatic',
                                 'sigma_EnvironmentMammal',
                                 'sigma_EnvironmentSoil'))) %>% 
  arrange(par) %>% 
  mutate(col = c('#ffffe5',
                 '#f7fcb9',
                 '#d9f0a3',
                 '#addd8e',
                 '#78c679',
                 '#41ab5d',
                 '#238443',
                 '#006837',
                 '#004529')) 


estimates <-   left_join(fixef_across, fixef_within,
                         by = 'par') %>% 
  mutate(par = factor(par,
                      levels = c('Intercept',
                                 'EnvironmentMammal',
                                 'EnvironmentSoil',
                                 'cTime',
                                 'EnvironmentMammal:cTime',
                                 'EnvironmentSoil:cTime',
                                 'sigma_EnvironmentAquatic',
                                 'sigma_EnvironmentMammal',
                                 'sigma_EnvironmentSoil'))) %>% 
  arrange(par)


zresil_time_compare <- ggplot() +
  geom_point(data = estimates %>% 
               filter(!par %in% c('sigma_EnvironmentAquatic',
                                  'sigma_EnvironmentMammal',
                                  'sigma_EnvironmentSoil')),
             aes(x = estimate_across, y = estimate_within, colour = par)) +
  geom_linerange(data = estimates %>% 
                   filter(!par %in% c('sigma_EnvironmentAquatic',
                                      'sigma_EnvironmentMammal',
                                      'sigma_EnvironmentSoil')),
                 aes(x = estimate_across, ymin = within_q2.5, ymax = within_q97.5,
                     colour = par)) +
  geom_linerange(data = estimates %>% 
                   filter(!par %in% c('sigma_EnvironmentAquatic',
                                      'sigma_EnvironmentMammal',
                                      'sigma_EnvironmentSoil')),
                 aes(xmin = across_q2.5, xmax = across_q97.5, y = estimate_within, 
                     colour = par)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  labs(subtitle = expression(paste('Turnover (z-score) change following disturbance'))) +
  scale_color_manual(name = 'Parameter',
                     values = deframe(par_pal)) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        legend.background = element_blank())


cowplot::plot_grid(alpha_ba_compare,
                   alpha_time_compare,
                   disp_ba_compare,
                   zdisp_ba_compare,
                   disp_time_compare,
                   zdisp_time_compare,
                   resil_time_compare,
                   zresil_time_compare, ncol = 2) +
  cowplot::draw_label(y = 0.001, label = 'Effort standardised across studies', vjust = 0.5) +
  cowplot::draw_label(x = 0.001, label = 'Effort standardised within studies', angle = 90) 

