require(tidyverse)
require(here)
require(ggpubr)
require(ragg)
require(readxl)
library(patchwork)

specieslist <- c("oncgor", "salsal", "saltru", "salalp")

#### Empirically estimated shedding rates ####

shed_rates_median <- read_xlsx(here("data", "shed_rates_median.xlsx"))
shed_rates_median

shed_rates_median %>% group_by(species) %>% summarise(median = median(median_shed))

shed_overallmedian <- read_xlsx(here("data", "shed_overallmedian.xlsx"))

#### Shedding rates corresponding to best fit models ####

list.dirs(here("Data_for_figures"))
param_linear <- read.csv(here("Data_for_figures/Linear_dist_best", "Param_data_Linear.csv"))
param_linear

param_expo <- read.csv(here("Data_for_figures/Exponential_dist_best", "Param_data_Exponential.csv"))
param_expo

param_unimod <- read.csv(here("Data_for_figures/Unimodal_dist_best", "Param_data_Unimodal.csv"))
param_unimod

param_linear_og <- param_linear %>% select(starts_with("oncgor")) %>% 
  mutate(species = specieslist[1]) %>% 
  mutate(rate = c("shed", "decay")) %>% 
  mutate(model = "linear")

names(param_linear_og) <- gsub(paste0(specieslist[1], "_"), "", names(param_linear_og))
param_linear_og


param_linear_ss <- param_linear %>% select(starts_with("salsal")) %>% 
  mutate(species = specieslist[2]) %>% 
  mutate(rate = c("shed", "decay")) %>% 
  mutate(model = "linear")

names(param_linear_ss) <- gsub(paste0(specieslist[2], "_"), "", names(param_linear_ss))
param_linear_ss


param_linear_st <- param_linear %>% select(starts_with("saltru")) %>% 
  mutate(species = specieslist[3]) %>% 
  mutate(rate = c("shed", "decay")) %>% 
  mutate(model = "linear")

names(param_linear_st) <- gsub(paste0(specieslist[3], "_"), "", names(param_linear_st))
param_linear_st

param_linear_sa <- param_linear %>% select(starts_with("salalp")) %>% 
  mutate(species = specieslist[4]) %>% 
  mutate(rate = c("shed", "decay")) %>% 
  mutate(model = "linear")

names(param_linear_sa) <- gsub(paste0(specieslist[4], "_"), "", names(param_linear_sa))
param_linear_sa

#expo


param_expo_og <- param_expo %>% select(starts_with("oncgor")) %>% 
  mutate(species = specieslist[1]) %>% 
  mutate(rate = c("shed", "decay")) %>% 
  mutate(model = "expo")

names(param_expo_og) <- gsub(paste0(specieslist[1], "_"), "", names(param_expo_og))
param_expo_og


param_expo_ss <- param_expo %>% select(starts_with("salsal")) %>% 
  mutate(species = specieslist[2]) %>% 
  mutate(rate = c("shed", "decay")) %>% 
  mutate(model = "expo")

names(param_expo_ss) <- gsub(paste0(specieslist[2], "_"), "", names(param_expo_ss))
param_expo_ss


param_expo_st <- param_expo %>% select(starts_with("saltru")) %>% 
  mutate(species = specieslist[3]) %>% 
  mutate(rate = c("shed", "decay")) %>% 
  mutate(model = "expo")

names(param_expo_st) <- gsub(paste0(specieslist[3], "_"), "", names(param_expo_st))
param_expo_st

param_expo_sa <- param_expo %>% select(starts_with("salalp")) %>% 
  mutate(species = specieslist[4]) %>% 
  mutate(rate = c("shed", "decay")) %>% 
  mutate(model = "expo")

names(param_expo_sa) <- gsub(paste0(specieslist[4], "_"), "", names(param_expo_sa))
param_expo_sa

# unimod
param_unimod_og <- param_unimod %>% select(starts_with("oncgor")) %>% 
  mutate(species = specieslist[1]) %>% 
  mutate(rate = c("shed", "decay")) %>% 
  mutate(model = "unimod")

names(param_unimod_og) <- gsub(paste0(specieslist[1], "_"), "", names(param_unimod_og))
param_unimod_og


param_unimod_ss <- param_unimod %>% select(starts_with("salsal")) %>% 
  mutate(species = specieslist[2]) %>% 
  mutate(rate = c("shed", "decay")) %>% 
  mutate(model = "unimod")

names(param_unimod_ss) <- gsub(paste0(specieslist[2], "_"), "", names(param_unimod_ss))
param_unimod_ss


param_unimod_st <- param_unimod %>% select(starts_with("saltru")) %>% 
  mutate(species = specieslist[3]) %>% 
  mutate(rate = c("shed", "decay")) %>% 
  mutate(model = "unimod")

names(param_unimod_st) <- gsub(paste0(specieslist[3], "_"), "", names(param_unimod_st))
param_unimod_st

param_unimod_sa <- param_unimod %>% select(starts_with("salalp")) %>% 
  mutate(species = specieslist[4]) %>% 
  mutate(rate = c("shed", "decay")) %>% 
  mutate(model = "unimod")

names(param_unimod_sa) <- gsub(paste0(specieslist[4], "_"), "", names(param_unimod_sa))
param_unimod_sa


param_best <- rbind.data.frame(param_unimod_og, param_expo_ss, param_expo_st, param_unimod_sa) %>%
  mutate(species = factor(species, ordered = T, levels = c("oncgor", "salsal", "saltru", "salalp")))

ggplot(param_best, aes(x = species, ymin = param_min, ymax = param_max, lower = param_min, upper = param_max, middle = param_best))+
  geom_boxplot(stat = "identity")+
  facet_grid(rows = vars(rate), scales = "free")


shed_best <- as_tibble(param_best) %>% dplyr::filter(rate == "shed") %>% 
  left_join(., shed_overallmedian, by = "species") %>% 
  mutate(species = factor(species, ordered = T, levels = c("oncgor", "salsal", "saltru", "salalp"))) %>% 
  rename(obs = overallmedian)

shed_plot <- ggplot(shed_best, aes(x = species, ymin = param_min, ymax = param_max, lower = param_min, upper = param_max, middle = param_best))+
  geom_boxplot(stat = "identity")+
  theme_bw()+
  scale_x_discrete(labels = c("Pink salmon", "Atlantic salmon", "Brown trout", "Arctic char"))+
  ylab(expression(paste("eDNA shedding rate (ng ", kg^-1, min^-1,")")))+
  xlab("")+
  geom_text(aes(y = param_best, label = round(param_best, 1)), vjust = -1)+
  geom_text(aes(y = obs, label = round(obs, 1)), vjust = -1)+
  #geom_crossbar(aes(y = obs), color = "blue")+
  geom_segment(aes(x = 0.55, xend = 1.45, y = obs[1], yend = obs[1]), size = 2, col = "green")+
geom_segment(aes(x = 1.55, xend = 2.45, y = obs[2], yend = obs[2]), size = 2, col = "green")+
  geom_segment(aes(x = 2.55, xend = 3.45, y = obs[3], yend = obs[3]), size = 2, col = "green")+
  geom_segment(aes(x = 3.55, xend = 4.45, y = obs[4], yend = obs[4]), size = 2, col = "green")+
  geom_segment(aes(x = 0.55, xend = 1.45, y = param_best[1], yend = param_best[1]), size = 2, col = "blue")+
  geom_segment(aes(x = 1.55, xend = 2.45, y = param_best[2], yend = param_best[2]), size = 2, col = "blue")+
  geom_segment(aes(x = 2.55, xend = 3.45, y = param_best[3], yend = param_best[3]), size = 2, col = "blue")+
  geom_segment(aes(x = 3.55, xend = 4.45, y = param_best[4], yend = param_best[4]), size = 2, col = "blue")+
  geom_jitter(data = shed_rates_median, aes(x = species, y = median_shed, color = sampl_replicate), inherit.aes = F)+
  scale_color_manual(values = c("red", "blue", "cyan"))+
  geom_text(aes(y = param_best, label = round(param_best, 1)), vjust = -1)+
  geom_text(aes(y = obs, label = round(obs, 1)), vjust = -1)+
  guides(color = guide_legend(title = "Sampling\nreplicate"))
shed_plot

decay_best <- as_tibble(param_best) %>% dplyr::filter(rate == "decay")

decay_plot <- ggplot(decay_best, aes(x = species, ymin = param_min, ymax = param_max, lower = param_min, upper = param_max, middle = param_best))+
  geom_boxplot(stat = "identity")+
  theme_bw()+
  geom_text(aes(y = param_best, label = round(param_best,4)), vjust = -1)+
  scale_x_discrete(labels = c("Pink salmon", "Atlantic salmon", "Brown trout", "Arctic char"))+
  ylab(expression(paste("eDNA removal rate (", min.^-1, ")")))+
  xlab("")+
  geom_segment(aes(x = 0.55, xend = 1.45, y = param_best[1], yend = param_best[1]), size = 2, col = "blue")+
  geom_segment(aes(x = 1.55, xend = 2.45, y = param_best[2], yend = param_best[2]), size = 2, col = "blue")+
  geom_segment(aes(x = 2.55, xend = 3.45, y = param_best[3], yend = param_best[3]), size = 2, col = "blue")+
  geom_segment(aes(x = 3.55, xend = 4.45, y = param_best[4], yend = param_best[4]), size = 2, col = "blue")

  
decay_plot

param_plot <- (shed_plot /decay_plot) + plot_annotation(tag_levels = "A")
param_plot


ragg::agg_png("figures/param_bestmod_plot2.png", width = 12, height = 12, units = "in", res = 600, scaling = 2)
param_plot
dev.off()
