require(tidyverse)
require(here)
require(ggpubr)
require(ragg)
require(readxl)
library(patchwork)


specieslist <- c("oncgor", "salsal", "saltru", "salalp")
eDNA <- read_xlsx(here("data", "eDNAconc_salmonids_GrenseJRiver_finalw_oncgorcounts.xlsx"))

#### Biomass, linear ####
biom_linear <- read.csv(here("Data_for_figures/Linear_dist_best", "Biomass_data_Linear_kg_20m.csv"))
biom_linear2 <- biom_linear %>% mutate(metres = 20* as.numeric(row.names(biom_linear))+10000) %>% 
  mutate(km = metres/1000) %>% 
  mutate(model = "linear")

biom_linear_og <- biom_linear2 %>% select(km, metres, model, starts_with("oncgor")) %>% 
  mutate(species = specieslist[1])
names(biom_linear_og) <- gsub(paste0(specieslist[1], "_"), "", names(biom_linear_og))

biom_linear_ss <- biom_linear2 %>% select(km, metres, model, starts_with(specieslist[2]))%>% 
  mutate(species = specieslist[2])
names(biom_linear_ss) <- names(biom_linear_og)

biom_linear_st <- biom_linear2 %>% select(km, metres, model, starts_with(specieslist[3]))%>% 
  mutate(species = specieslist[3])
names(biom_linear_st) <- names(biom_linear_og)

biom_linear_sa <- biom_linear2 %>% select(km, metres, model, starts_with(specieslist[4]))%>% 
  mutate(species = specieslist[4])
names(biom_linear_sa) <- names(biom_linear_og)

biom_linear3 <- rbind.data.frame(biom_linear_og, biom_linear_ss, biom_linear_st, biom_linear_sa)


biom_linear_plot <- ggplot(biom_linear3, aes(x = rev(km)))+
  geom_line(aes(y = mean), colour = "blue")+
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2)+
  facet_grid(rows = vars(species), scales = "free")+
  theme_bw()+
  ylab("Biomass (kg) per 20m segment")+
  xlab("Distance from ocean (km)")+
  scale_x_continuous(breaks = unique(oncgor_med$dist_ocean))+
  scale_y_log10()

biom_linear_plot


#### eDNA, linear ####
edna_linear <- read.csv(here("Data_for_figures/Linear_dist_best", "eDNA_data_Linear_ng_L.csv"))
edna_linear2 <- edna_linear %>% mutate(metres = 20* as.numeric(row.names(edna_linear))+10000) %>% 
  mutate(km = metres/1000)%>% 
  mutate(model = "linear")

edna_linear_og <- edna_linear2 %>% select(km, metres, model, starts_with("oncgor")) %>% 
  mutate(species = specieslist[1])
names(edna_linear_og) <- gsub(paste0(specieslist[1], "_"), "", names(edna_linear_og))

edna_linear_ss <- edna_linear2 %>% select(km, metres, model, starts_with(specieslist[2]))%>% 
  mutate(species = specieslist[2])
names(edna_linear_ss) <- names(edna_linear_og)

edna_linear_st <- edna_linear2 %>% select(km, metres, model, starts_with(specieslist[3]))%>% 
  mutate(species = specieslist[3])
names(edna_linear_st) <- names(edna_linear_og)

edna_linear_sa <- edna_linear2 %>% select(km, metres, model, starts_with(specieslist[4]))%>% 
  mutate(species = specieslist[4])
names(edna_linear_sa) <- names(edna_linear_og)

edna_linear3 <- rbind.data.frame(edna_linear_og, edna_linear_ss, edna_linear_st, edna_linear_sa)


edna_linear_plot <- ggplot(edna_linear3, aes(x = rev(km)))+
  geom_line(aes(y = mean), colour = "blue")+
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2)+
  geom_point(data = eDNA, aes(x = dist_ocean, y = 1000*spec_edna_ngml))+ 
  facet_grid(rows = vars(species), scales = "free")+
  theme_bw()+
  ylab("eDNA")+
  xlab("Distance from ocean (km)")+
  scale_x_continuous(breaks = unique(oncgor_med$dist_ocean))+
  scale_y_log10()



#### Biomass, exponential ####

biom_expon <- read.csv(here("Data_for_figures/Exponential_dist_best", "Biomass_data_Exponential_kg_20m.csv"))
biom_expo2 <- biom_expon %>% mutate(metres = 20* as.numeric(row.names(biom_expon))+10000) %>% 
  mutate(km = metres/1000) %>% 
  mutate(model = "expo")

biom_expo_og <- biom_expo2 %>% select(km, metres, model, starts_with("oncgor")) %>% 
  mutate(species = specieslist[1])
names(biom_expo_og) <- gsub(paste0(specieslist[1], "_"), "", names(biom_expo_og))

biom_expo_ss <- biom_expo2 %>% select(km, metres, model, starts_with(specieslist[2]))%>% 
  mutate(species = specieslist[2])
names(biom_expo_ss) <- names(biom_expo_og)

biom_expo_st <- biom_expo2 %>% select(km, metres, model, starts_with(specieslist[3]))%>% 
  mutate(species = specieslist[3])
names(biom_expo_st) <- names(biom_expo_og)

biom_expo_sa <- biom_expo2 %>% select(km, metres, model, starts_with(specieslist[4]))%>% 
  mutate(species = specieslist[4])
names(biom_expo_sa) <- names(biom_expo_og)

biom_expo3 <- rbind.data.frame(biom_expo_og, biom_expo_ss, biom_expo_st, biom_expo_sa)


biom_expo_plot <- ggplot(biom_expo3, aes(x = rev(km)))+
  geom_line(aes(y = mean), colour = "blue")+
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2)+
  facet_grid(rows = vars(species), scales = "free")+
  theme_bw()+
  ylab("Biomass (kg) per 20m segment")+
  xlab("Distance from ocean (km)")+
  scale_x_continuous(breaks = unique(oncgor_med$dist_ocean))+
  scale_y_log10()

biom_expo_plot  
#### eDNA, exponential ####
edna_expo <- read.csv(here("Data_for_figures/Exponential_dist_best", "eDNA_data_Exponential_ng_L.csv"))
edna_expo2 <- edna_expo %>% mutate(metres = 20* as.numeric(row.names(edna_expo))+10000) %>% 
  mutate(km = metres/1000) %>% 
  mutate(model = "expo")

edna_expo_og <- edna_expo2 %>% select(km, metres, model, starts_with("oncgor")) %>% 
  mutate(species = specieslist[1])
names(edna_expo_og) <- gsub(paste0(specieslist[1], "_"), "", names(edna_expo_og))

edna_expo_ss <- edna_expo2 %>% select(km, metres, model, starts_with(specieslist[2]))%>% 
  mutate(species = specieslist[2])
names(edna_expo_ss) <- names(edna_expo_og)

edna_expo_st <- edna_expo2 %>% select(km, metres, model, starts_with(specieslist[3]))%>% 
  mutate(species = specieslist[3])
names(edna_expo_st) <- names(edna_expo_og)

edna_expo_sa <- edna_expo2 %>% select(km, metres, model, starts_with(specieslist[4]))%>% 
  mutate(species = specieslist[4])
names(edna_expo_sa) <- names(edna_expo_og)

edna_expo3 <- rbind.data.frame(edna_expo_og, edna_expo_ss, edna_expo_st, edna_expo_sa)


edna_expo_plot <- ggplot(edna_expo3, aes(x = rev(km)))+
  geom_line(aes(y = mean), colour = "blue")+
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2)+
  geom_point(data = eDNA, aes(x = dist_ocean, y = 1000*spec_edna_ngml))+ 
  facet_grid(rows = vars(species), scales = "free")+
  scale_y_log10()+
  theme_bw()+
  ylab("eDNA concentration (ng/L)")+
  xlab("Distance from ocean (km)")+
  scale_x_continuous(breaks = unique(oncgor_med$dist_ocean))+
  scale_y_log10()





#### Biomass, unimodal ####
biom_unimod <- read.csv(here("Data_for_figures/Unimodal_dist_best", "Biomass_data_Unimodal_kg_20m.csv"))
biom_unimod2 <- biom_unimod %>% mutate(metres = 20* as.numeric(row.names(biom_unimod))+10000) %>% 
  mutate(km = metres/1000) %>% 
  mutate(model = "unimod")

biom_unimod_og <- biom_unimod2 %>% select(km, metres, model, starts_with("oncgor")) %>% 
  mutate(species = specieslist[1])
names(biom_unimod_og) <- gsub(paste0(specieslist[1], "_"), "", names(biom_unimod_og))

biom_unimod_ss <- biom_unimod2 %>% select(km, metres, model, starts_with(specieslist[2]))%>% 
  mutate(species = specieslist[2])
names(biom_unimod_ss) <- names(biom_unimod_og)

biom_unimod_st <- biom_unimod2 %>% select(km, metres, model, starts_with(specieslist[3]))%>% 
  mutate(species = specieslist[3])
names(biom_unimod_st) <- names(biom_unimod_og)

biom_unimod_sa <- biom_unimod2 %>% select(km, metres, model, starts_with(specieslist[4]))%>% 
  mutate(species = specieslist[4])
names(biom_unimod_sa) <- names(biom_unimod_og)

biom_unimod3 <- rbind.data.frame(biom_unimod_og, biom_unimod_ss, biom_unimod_st, biom_unimod_sa)


biom_unimod_plot <- ggplot(biom_unimod3, aes(x = rev(km)))+
  geom_line(aes(y = mean), colour = "blue")+
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2)+
  facet_grid(rows = vars(species), scales = "free")+
  theme_bw()+
  ylab("Biomass (kg) per 20m segment")+
  xlab("Distance from ocean (km)")+
  scale_x_continuous(breaks = unique(oncgor_med$dist_ocean))+
  scale_y_log10()


#### eDNA, unimodal ####
edna_unimod <- read.csv(here("Data_for_figures/Unimodal_dist_best", "eDNA_data_Unimodal_ng_L.csv"))
edna_unimod2 <- edna_unimod %>% mutate(metres = 20* as.numeric(row.names(edna_unimod))+10000) %>% 
  mutate(km = metres/1000) %>% 
  mutate(model = "unimod")

edna_unimod_og <- edna_unimod2 %>% select(km, metres, model, starts_with("oncgor")) %>% 
  mutate(species = specieslist[1])
names(edna_unimod_og) <- gsub(paste0(specieslist[1], "_"), "", names(edna_unimod_og))

edna_unimod_ss <- edna_unimod2 %>% select(km, metres, model, starts_with(specieslist[2]))%>% 
  mutate(species = specieslist[2])
names(edna_unimod_ss) <- names(edna_unimod_og)

edna_unimod_st <- edna_unimod2 %>% select(km, metres, model, starts_with(specieslist[3]))%>% 
  mutate(species = specieslist[3])
names(edna_unimod_st) <- names(edna_unimod_og)

edna_unimod_sa <- edna_unimod2 %>% select(km, metres, model, starts_with(specieslist[4]))%>% 
  mutate(species = specieslist[4])
names(edna_unimod_sa) <- names(edna_unimod_og)

edna_unimod3 <- rbind.data.frame(edna_unimod_og, edna_unimod_ss, edna_unimod_st, edna_unimod_sa)


edna_unimod_plot <- ggplot(edna_unimod3, aes(x = rev(km)))+
  geom_line(aes(y = mean), colour = "blue")+
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2)+
  geom_point(data = eDNA, aes(x = dist_ocean, y = 1000*spec_edna_ngml))+ 
  facet_grid(rows = vars(species), scales = "free")+
  theme_bw()+
  ylab("eDNA")+
  xlab("Distance from ocean (km)")+
  scale_x_continuous(breaks = unique(oncgor_med$dist_ocean))+
  scale_y_log10()



#### Plot all models for all species in the same plot ####
ednalab <- ggplot(data.frame(l = 'Species eDNA concentration in river water (ng/L)', x = 1, y = 1)) +
  geom_text(aes(x, y, label = l), angle = 90) + 
  theme_void() +
  coord_cartesian(clip = "off")

biomasslab <- ggplot(data.frame(l = 'Biomass (kg) per 20 m segment', x = 1, y = 1)) +
  geom_text(aes(x, y, label = l), angle = 90) + 
  theme_void() +
  coord_cartesian(clip = "off")


# eDNA models together in one plot

fullpatchedna <- (ednalab | ((edna_linear_plot+theme(axis.title.y = element_blank(),
                                  axis.title.x.bottom = element_blank()))/
             (edna_expo_plot+ theme(axis.title = element_blank()))/
             (edna_unimod_plot + theme(axis.title.y = element_blank())))) + 
  plot_layout(widths = c(1.5, 25)) 

fullpatchedna[[2]] <- fullpatchedna[[2]] + plot_layout(tag_level = "new")

fullpatchedna2 <- fullpatchedna + plot_annotation(tag_levels = list(c("A", ""), "I"))


# Biomass models together in one plot

fullpatchbiomass <- (biomasslab | ((biom_linear_plot + theme(axis.title = element_blank()))/
                       (biom_expo_plot + theme(axis.title = element_blank()))/
                       (biom_unimod_plot + theme(axis.title.y = element_blank())))) + 
                       plot_layout(widths = c(1.5, 25))

fullpatchbiomass[[2]] <- fullpatchbiomass[[2]] + plot_layout(tag_level = "new")

fullpatchbiomass2 <- fullpatchbiomass + plot_annotation(tag_levels = list(c("B", ""), "I"))



(fullpatchedna2 - fullpatchbiomass2) + plot_layout(ncol = 2, widths = c(26,26)) + plot_annotation(tag_levels = list(c("A", "", "B", ""), "I"))

ragg::agg_png("figures/modeling_edna_biom_all.png", width = 10, height = 12, units = "in", res = 300, scaling = 1.1)
(fullpatchedna2 - fullpatchbiomass2) + plot_layout(ncol = 2, widths = c(26,26)) + plot_annotation(tag_levels = list(c("A", "", "B", ""), "I"))
dev.off()


#### Plot with best model for each species ####
#### Best model eDNA ####

edna_bestmod <- rbind.data.frame(edna_unimod_og, edna_expo_ss, edna_expo_st, edna_unimod_sa)
writexl::write_xlsx(edna_bestmod, here("data", "modeled_edna_bestmod.xlsx"))

edna_bestmod_plot <- ggplot(edna_bestmod, aes(x = rev(km)))+
  geom_line(aes(y = mean), colour = "blue")+
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2)+
  geom_jitter(data = eDNA, aes(x = dist_ocean, y = 1000*spec_edna_ngml, color = sampl_replicate))+ 
  facet_grid(rows = vars(species), scales = "free")+
  theme_bw()+
  ylab("eDNA")+
  xlab("Distance from ocean (km)")+
  scale_x_continuous(breaks = unique(oncgor_med$dist_ocean))+
  scale_y_log10()

biomass_bestmod <- rbind.data.frame(biom_unimod_og, biom_expo_ss, biom_expo_st, biom_unimod_sa)

biomass_bestmod_plot <- ggplot(biomass_bestmod, aes(x = rev(km)))+
  geom_line(aes(y = mean), colour = "blue")+
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2)+
  facet_grid(rows = vars(species), scales = "free")+
  theme_bw()+
  ylab("Biomass")+
  xlab("Distance from ocean (km)")+
  scale_x_continuous(breaks = unique(oncgor_med$dist_ocean))+
  scale_y_log10()


# eDNA best models are plotted in "anadromous_grense_jakobs_river.Rmd"

# Plot of best biomass models for each species

biomass_og_plot <- ggplot(biom_unimod_og, aes(x = rev(km)))+
  geom_line(aes(y = mean), colour = "blue")+
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2)+
  #facet_grid(rows = vars(species), scales = "free")+
  theme_bw()+
  ylab("Biomass (kg fish per 20 m)")+
  xlab("Distance from ocean (km)")+
  theme(axis.title = element_text(size = 10),
        legend.title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 12))+
  scale_x_continuous(breaks = unique(oncgor_med$dist_ocean))+
  scale_y_log10()+
  scale_x_continuous(breaks = unique(oncgor_med$dist_ocean), sec.axis = dup_axis(name = "Station", labels = paste0("St.", seq(1:6))))
biomass_og_plot

biomass_ss_plot <- ggplot(biom_expo_ss, aes(x = rev(km)))+
  geom_line(aes(y = mean), colour = "blue")+
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2)+
  #facet_grid(rows = vars(species), scales = "free")+
  theme_bw()+
  ylab("Biomass (kg fish per 20 m)")+
  xlab("Distance from ocean (km)")+
  theme(axis.title = element_text(size = 10),
        legend.title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 12))+
  scale_x_continuous(breaks = unique(oncgor_med$dist_ocean))+
  scale_y_log10()
biomass_ss_plot

biomass_st_plot <- ggplot(biom_expo_st, aes(x = rev(km)))+
  geom_line(aes(y = mean), colour = "blue")+
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2)+
  #facet_grid(rows = vars(species), scales = "free")+
  theme_bw()+
  ylab("Biomass (kg fish per 20 m)")+
  xlab("Distance from ocean (km)")+
  theme(axis.title = element_text(size = 10),
        legend.title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 12))+
  scale_x_continuous(breaks = unique(oncgor_med$dist_ocean))+
  scale_y_log10()
biomass_st_plot


biomass_sa_plot <- ggplot(biom_unimod_sa, aes(x = rev(km)))+
  geom_line(aes(y = mean), colour = "blue")+
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2)+
  #facet_grid(rows = vars(species), scales = "free")+
  theme_bw()+
  ylab("Biomass (kg fish per 20 m)")+
  xlab("Distance from ocean (km)")+
  theme(axis.title = element_text(size = 10),
        legend.title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 12))+
  scale_x_continuous(breaks = unique(oncgor_med$dist_ocean))+
  scale_y_log10()
biomass_sa_plot



# Put plots together with patchwork

biomlab <- ggplot(data.frame(l = 'Estimated biomass (kg fish per 20 m)', x = 1, y = 1)) +
  geom_text(aes(x, y, label = l), angle = 90) + 
  theme_void() +
  coord_cartesian(clip = "off")


bestmodpatchbiomass <- (biomlab | ((biomass_og_plot+theme(axis.title.y = element_blank(),
                                                       axis.title.x.bottom = element_blank())) /
                                   (biomass_ss_plot + theme(axis.title = element_blank()))/
                                   (biomass_st_plot + theme(axis.title = element_blank()))/
                                    (biomass_sa_plot + theme(axis.title.y = element_blank())))) + 
  plot_layout(widths = c(1.5, 25))

bestmodpatchbiomass[[2]] <- bestmodpatchbiomass[[2]] + plot_layout(tag_level = "new")

bestmodpatchbiomass2 <- bestmodpatchbiomass + plot_annotation(tag_levels = list(c("B", ""), "I"))


# 'bestmodpatchedna2 is made in "anadromous_grense_jakobs_river.Rmd"
# put them together:
(bestmodpatchedna2 - bestmodpatchbiomass2) + plot_layout(ncol = 2, widths = c(26,26)) + plot_annotation(tag_levels = list(c("A", "", "B", ""), "I"))

ragg::agg_png("figures/edna_biomass_withmodel2.png", width = 12, height = 10, units = "in", res = 600, scaling = 1.1)
(bestmodpatchedna2 - bestmodpatchbiomass2) + plot_layout(ncol = 2, widths = c(26,26)) + plot_annotation(tag_levels = list(c("A", "", "B", ""), "I"))
dev.off()
