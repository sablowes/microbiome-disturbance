alpha <- read.delim('~/Dropbox/1current/microbiome-disturbance/data/Rich-across.txt',
                    sep = " ") %>% 
  as_tibble() 

alpha_time50 <- alpha %>% 
  filter(Rank > 1 & Time_since_dist < 50) %>% 
  mutate(cTime = Time_since_dist - mean(Time_since_dist)) %>% 
  # check we've got time series:
  group_by(Time_series) %>% 
  mutate(n_obs = n_distinct(Time_since_dist)) %>% 
  ungroup() %>% 
  filter(n_obs > 1)

  
alpha_time50 %>% 
  filter(is.na(Selective_Factor)) %>% 
  distinct(Study, Time_series)

alpha_sf_meta <- alpha_time50 %>% 
  distinct(Study, Time_series, Environment, Selective_Factor) %>% 
  # two time series name has a space in it
  mutate(Time_series = case_when(Time_series=='Kennedy.0.2 TCC.Rat_Feces' ~ 
                                   'Kennedy.0.2.TCC.Rat_Feces',
                                 Time_series=='Kennedy.0.5 TCC.Rat_Feces' ~ 
                                   'Kennedy.0.5.TCC.Rat_Feces',
                                 TRUE ~ as.character(Time_series)))

save(alpha_sf_meta,
     file = '~/Dropbox/1current/microbiome-disturbance/data/alpha_sf_meta.Rdata')

# might be better to get the metadata
dispersion <- read.delim('~/Dropbox/1current/microbiome-disturbance/data/dispersions-across.txt',
                           sep = " ") %>% 
  as_tibble()

dispersion_time50 <- dispersion %>% 
  filter(Rank > 1 & Time < 50) %>% 
  mutate(# centre time for modelling
    cTime = Time - mean(Time)) %>% 
  group_by(Time_series) %>% 
  mutate(n_obs = n_distinct(cTime)) %>% 
  ungroup()  %>% 
  filter(n_obs > 1 )

# any missing?  
dispersion_time50 %>% 
  filter(is.na(Selective_Factor)) %>% 
  distinct(Study, Time_series)

# any multiples?
dispersion_time50 %>% 
  group_by(Study, Time_series) %>% 
  summarise(n_sf = n_distinct(Selective_Factor)) %>% 
  ungroup() %>% 
  filter(n_sf > 1)

disp_sf_meta <- dispersion_time50 %>% 
  distinct(Study, Time_series, Environment, Selective_Factor) %>% 
  # two time series name has a space in it
  mutate(Time_series = case_when(Time_series=='Kennedy.0.2 TCC.Rat_Feces' ~ 
                                   'Kennedy.0.2.TCC.Rat_Feces',
                                 Time_series=='Kennedy.0.5 TCC.Rat_Feces' ~ 
                                   'Kennedy.0.5.TCC.Rat_Feces',
                                 TRUE ~ as.character(Time_series)))
save(disp_sf_meta,
     file = '~/Dropbox/1current/microbiome-disturbance/data/disp_sf_meta.Rdata')
