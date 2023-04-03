require(tidyverse)
require(here)
require(ggpubr)
require(ragg)
obs <- tibble(species = c("pinksal", "atlsal", "trout", "char"), nfish = c(5603, 18, 3, 1), 
              bfish  = c(5603*1.5, 18*3, 3*1.5, 1*2.5), totfish = sum(5603, 18, 3, 0)) 
obs

samples_meta <- readxl::read_xlsx(here("data", "eDNAsamples_2021.xlsx")) %>%
  dplyr::select(Prøvenr, sample_rep)

edna <- readxl::read_xlsx(here("data", "eDNAconc_salmonids_GrenseJRiver.xlsx"))



edna_obs <- left_join(edna, obs, by = "species") %>% 
  mutate(species = factor(species, ordered = T, levels = c("pinksal", "atlsal", "trout", "char"))) %>% 
  left_join(., samples_meta, by = c("Sample" = "Prøvenr"))

head(edna_obs)
obs

edna_obs %>% group_by(species, dist_ocean) %>% 
  summarise(across(.cols = target_dna_conc_ngmL, .fns = median, na.rm = T)) %>%
  group_by(species) %>% 
  summarise(across(.cols = target_dna_conc_ngmL, .fns = range, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(minmax = rep(c("min", "max"), 4)) %>% 
  pivot_wider(names_from = minmax, values_from = target_dna_conc_ngmL) %>% 
  mutate(folddiff = log10(max/min))

rank_median <- edna_obs %>% group_by(species, dist_ocean) %>% 
  summarise(across(.cols = target_dna_conc_ngmL, .fns = median, na.rm = T)) %>%
  group_by(species) %>%
  mutate(rankja = rank(target_dna_conc_ngmL))
