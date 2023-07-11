require(tidyverse)
require(here)
require(ggpubr)
require(ragg)
#5603
obs0 <- tibble(species = c("oncgor", "salsal", "saltru", "salalp"), nfish = c(3987, 18, 3, 1), 
              bfish  = c(3987*1.5, 18*3, 3*1.5, 1*2.5), totfish = sum(3987, 18, 3, 0)) 
obs

obs <- tibble(species = c("oncgor", "salsal", "saltru", "salalp"), nfish = c(5603, 18, 3, 1), 
              bfish  = c(5603*1.5, 18*3, 3*1.5, 1*2.5), totfish = sum(5603, 18, 3, 0)) 

samples_meta <- readxl::read_xlsx(here("data", "eDNAsamples_GJE_2021_Rform.xlsx")) %>%
  dplyr::select(filter, sampl_replicate)

edna_10km <- readxl::read_xlsx(here("data", "eDNAconc_salmonids_GrenseJRiver_finalw_oncgorcounts.xlsx")) %>% 
  dplyr::filter(dist_ocean == 10)



edna_obs <- left_join(edna_10km, obs, by = "species") %>% 
  mutate(species = factor(species, ordered = T, levels = c("oncgor", "salsal", "saltru", "salalp"))) 

head(edna_obs)
obs

edna_obs_ratios0 <- edna_obs %>% 
  dplyr::filter(!is.na(spec_edna_ngml)) %>% 
  mutate(log_cop = log(spec_edna_ngml*1e8/nfish)) %>% 
  mutate(log_ng_count = log10(spec_edna_ngml/nfish)) %>% 
  mutate(log_ng_biomass = log10(spec_edna_ngml/bfish)) %>% 
  mutate(nolog = spec_edna_ngml/nfish)

edna_obs_ratios <- edna_obs_ratios0 %>% dplyr::filter(species != "salalp") %>% 
  left_join(., samples_meta, by = c("filter" = "filter"))%>% 
  select(-sampl_replicate.y)

median_rat <- edna_obs_ratios %>% group_by(species) %>% 
  summarise_if(is.numeric, median) %>% 
  select(species, log_ng_count, log_ng_biomass) 

ps_med_count <- median_rat %>% filter(species == "oncgor") %>% pull(log_ng_count)
as_med_count <- median_rat %>% filter(species == "salsal") %>% pull(log_ng_count)
st_med_count <- median_rat %>% filter(species == "saltru") %>% pull(log_ng_count)

ps_med_biomass <- median_rat %>% filter(species == "oncgor") %>% pull(log_ng_biomass)
as_med_biomass <- median_rat %>% filter(species == "salsal") %>% pull(log_ng_biomass)
st_med_biomass <- median_rat %>% filter(species == "saltru") %>% pull(log_ng_biomass)

ps_as_count <- 10^ps_med_count/10^as_med_count
ps_as_count

ps_st_count <- 10^ps_med_count/10^st_med_count
ps_st_count
1/ps_st_biomass
1/ps_st_count

ps_as_biomass <- 10^ps_med_biomass/10^as_med_biomass
ps_as_biomass

ps_st_biomass <- 10^ps_med_biomass/10^st_med_biomass
ps_st_biomass


st_as_count <- 10^st_med_count/10^as_med_count
st_as_count

st_as_biomass <- 10^st_med_biomass/10^as_med_biomass
st_as_biomass


#Tukey, ratios
aov_ratios_count <- aov(log_ng_count ~ species, data = edna_obs_ratios)
hsd_ratios_count <- HSD.test(aov_ratios_count, "species", group = T, unbalanced = T)
hsd_ratios_count

hsd_ratios_count2 <- HSD.test(aov_ratios_count, "species", group = F, unbalanced = T)
hsd_ratios_count2

aov_ratios_biom <- aov(log_ng_biomass ~ species, data = edna_obs_ratios)
hsd_ratios_biom <- HSD.test(aov_ratios_biom, "species", group = T, unbalanced = T)
hsd_ratios_biom

ratio_groups_count <- hsd_ratios_count$groups %>% 
  rownames_to_column(., var = "species") %>% tibble() 

ratio_groups_biom <- hsd_ratios_biom$groups %>% 
  rownames_to_column(., var = "species") %>% tibble() 

ratio.summarised <- edna_obs_ratios %>% 
  group_by(species) %>% 
  summarize(maxratiocount = max(log_ng_count, na.rm = T), maxratiobiomass = max(log_ng_biomass, na.rm = T))


ratio_count_labels <- left_join(ratio_groups_count, ratio.summarised, by = "species") %>% 
  mutate(species = factor(species, ordered = T, levels = unique(edna_obs_ratios$species)))

ratio_biom_labels <- left_join(ratio_groups_biom, ratio.summarised, by = "species") %>% 
  mutate(species = factor(species, ordered = T, levels = unique(edna_obs_ratios$species)))


#Barplot, ratios

DNA_numberplot <- ggplot(edna_obs_ratios, aes(x = species, y = log_ng_count, group = species))+
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(width = 0.05, aes(color = sampl_replicate.x))+
  scale_color_manual(values = c("red", "blue", "cyan"))+
  theme_bw()+
  labs(color = "Sampling\nreplicate")+
  scale_x_discrete(labels = c("Pink Salmon", "Atlantic Salmon", "Trout", "Arctic salalpr*"))+
  ylab(expression(paste(log[10],over(paste("ng eDNA m", L^-1),"fish count"))))+
  ylim(c(-6,-4.6))+
  theme(axis.title.x = element_blank())+
  geom_text(data = ratio_count_labels, aes(x = species, y = maxratiocount+0.2, label = groups))

DNA_numberplot

DNA_biomassplot <- ggplot(edna_obs_ratios, aes(x = species, y = log_ng_biomass, group = species))+
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(width = 0.05, aes(color = sampl_replicate.x))+
  scale_color_manual(values = c("red", "blue", "cyan"))+
  theme_bw()+
  labs(color = "Sampling\nreplicate")+
  scale_x_discrete(labels = c("Pink Salmon", "Atlantic Salmon", "Trout"))+
  ylab(expression(paste(log[10],over(paste("ng eDNA m", L^-1),"estimated biomass"))))+
  ylim(c(-6.5,-4.6))+
  theme(axis.title.x = element_blank())+
  geom_text(data = ratio_biom_labels, aes(x = species, y = maxratiobiomass+0.2, label = groups))


DNA_biomassplot

dna_fish <- ggarrange(DNA_numberplot, DNA_biomassplot, nrow = 2, common.legend = T, legend = "right", labels = "AUTO")

ragg::agg_png("figures/ratio_dna_fish2.png", width = 6, height = 5, units = "in", res = 300, scaling = 1)
dna_fish
dev.off()

ednaaov <- aov(log_ng_count ~ species, data = edna_obs_ratios)

hm <- HSD.test(ednaaov, "species", group = T)

summary(aov(log_ng_count ~ species, data = edna_obs_ratios))

with(edna_obs_ratios, t.test(log_ng_count[species == "oncgor"], log_ng_count[species == "salsal"]))

with(edna_obs_ratios, t.test(log_ng_count[species == "saltru"], log_ng_count[species == "salsal"]))

with(edna_obs_ratios, t.test(log_ng_count[species == "oncgor"], log_ng_count[species == "saltru"]))

with(edna_obs_ratios, t.test(log_ng_biomass[species == "oncgor"], log_ng_biomass[species == "saltru"]))

with(edna_obs_ratios, t.test(log_ng_biomass[species == "saltru"], log_ng_biomass[species == "salsal"]))

fishcols <- ggplot_build(allfish_bp)$data[[1]] %>% select(fill) %>% unique()


edna_obs_ratios01 <- edna_obs_ratios0 %>% group_by(filter, species, nfish, bfish) %>% summarise(medianconc = median(spec_edna_ngml, na.rm = T))

loglog_nfish <- ggplot(edna_obs_ratios01, aes(x = nfish, y = (medianconc), group = species, fill = species))+
  geom_point()+
  geom_boxplot( lwd =0.2, outlier.size = 1)+
  scale_fill_manual(values = fishcols$fill, labels = c("Pink salmon", "Atlantic salmon", "saltru", "salalpr"))+
  labs(fill = "Species", y = "eDNA concentration\n (ng/mL)", x = "Number of fish")+
  scale_x_continuous(trans = "log10")+
  scale_y_continuous(trans = "log10")

loglog_nfish

loglog_biomass <-  ggplot(edna_obs_ratios01, aes(x = (bfish), y = (medianconc), group = species, fill = species))+
    geom_point()+
    geom_boxplot( lwd =0.2, outlier.size = 1)+
  scale_fill_manual(values = fishcols$fill, labels = c("Pink salmon", "Atlantic salmon", "saltru", "salalpr"))+
  labs(fill = "Species", y = "eDNA concentration\n (ng/mL)", x = "Biomass of fish (kg)")+
  scale_x_continuous(trans = "log10")+
  scale_y_continuous(trans = "log10")


loglog_biomass

loglog_fish <- ggarrange(loglog_nfish, loglog_biomass, nrow = 2, common.legend = T, legend = "right", labels = "AUTO")

loglog_fish

ragg::agg_png("figures/loglog_fish.png", width = 6, height = 5, units = "in", res = 300, scaling = 1)
loglog_fish
dev.off()
  
with(edna_obs_ratios0, cor.test(log(spec_edna_ngml), log(nfish), method = "kendall"))
with(edna_obs_ratios0, cor.test(log(spec_edna_ngml), log(bfish), method = "kendall"))

with(edna_obs_ratios0, summary(lm(log(spec_edna_ngml) ~ log(nfish))))
with(edna_obs_ratios0, summary(lm(log(spec_edna_ngml) ~ log(bfish))))

with(edna_obs_ratios01, cor.test(log(medianconc), log(nfish), method = "kendall"))
with(edna_obs_ratios01, cor.test(log(medianconc), log(bfish), method = "kendall"))

with(edna_obs_ratios01, summary(lm(log(medianconc) ~ log(nfish))))
with(edna_obs_ratios01, summary(lm(log(medianconc) ~ log(bfish))))

#### shedding rate ####
#10^-5.5 ngDNA/mL river water/kg * 4m3/s
10^-5.5 * 1000 * 4000 #12.65 ng/kg/s # Transform to pg/g/h
12.65 * 3600

10^-6.2 * 1000 * 4000  * 3600 #12.65 ng/kg/s # Transform to pg/g/h

(10^-1.6 * 10^3 * 4000) #ng/mL * 1000mL/L * 4000L/s = 100475 ng/s



# Pink salmon eDNA / fish ratio

ednaps <- readxl::read_xlsx(here("data", "eDNAconc_salmonids_GrenseJRiver_finalw_PinkSalcounts.xlsx")) %>% 
  dplyr::filter(species == "oncgor") %>%
  mutate(pink_sal_obs = as.numeric(pink_sal_obs)) %>% 
  rowwise() %>% 
  mutate(ratioo = spec_edna_ngml/pink_sal_obs) %>% 
  dplyr::filter(!is.na(ratioo))

ggplot(ednaps, aes(x = dist_ocean, y = log10(ratioo), group = station))+ 
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(width = 0.05, aes(color = sampl_replicate))+
  scale_color_manual(values = c("red", "blue", "cyan"))

pinksal_ratio_aov <- aov(log10(ratioo) ~ dist_ocean, data = ednaps)
summary(pinksal_ratio_aov)

psalhsd_ratio <- agricolae::HSD.test(pinksal_ratio_aov, "dist_ocean", group = T)
psalhsd_ratio

pinksal_groups <- psalhsd$groups %>% 
  rownames_to_column(., var = "dist_hav_fact") %>% tibble() %>% 
  mutate(dist_hav = as.double(dist_hav_fact)) %>% 
  arrange(dist_hav)


pinksal.summarised <- oncgor_med %>% group_by(dist_ocean, station) %>% summarize(max.edna = max(log10(spec_edna_ngml), na.rm = T))
