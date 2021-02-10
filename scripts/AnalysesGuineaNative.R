## Script to assess differences in tgermination and growth of Guinea grass
## and the native seed bank as a function of soil treatment (individual soil 
## sampling, ISS, and mixed soil sampling, MSS) and invasion (invaded and 
## non-invaded).
## Written by Dr. Elizabeth Bowman on Nov. 11, 2020

# install.packages('tidyverse')
# install.packages('nlme')
# install.packages('vegan')
library(tidyverse)
library(nlme)
library(vegan)
library(stats)


plant.data <- read.csv('data_output/data.Guinea.csv', as.is = T)
time.data <- read.csv('data_output/data.GuineaHeightOverTime.csv', as.is = T)
native.data <- read.csv('data_output/data.NativeCommunity.csv', as.is = T)

# factor variables
plant.data$soil.treatment <- as.factor(plant.data$soil.treatment)
plant.data$invasion <- factor(plant.data$invasion, 
                              levels = c('Invaded','Uninvaded'),
                              labels = c('Invaded','Non-invaded'))
plant.data$pasture <- as.factor(plant.data$pasture)

time.data$soil.treatment <- as.factor(time.data$soil.treatment)
time.data$invasion <- factor(time.data$invasion, 
                              levels = c('Invaded','Uninvaded'),
                              labels = c('Invaded','Non-invaded'))
time.data$pasture <- as.factor(time.data$pasture)

native.data$soil.treatment <- as.factor(native.data$soil.treatment)
native.data$invasion <- factor(native.data$invasion, 
                              levels = c('Invaded','Uninvaded'),
                              labels = c('Invaded','Non-invaded'))
native.data$pasture <- as.factor(native.data$pasture)

# --------------------------------------------------------------------------#
#--<< Guinea grass: GLM function of soil treatment and invasion >>----
# --------------------------------------------------------------------------#

#--Seedlings----
#Remove -inf
seedling.data <- filter(plant.data, !is.na(log.guinea.grass.seedling.total))
## Overall
glm.seedling <- lme(log.guinea.grass.seedling.total ~ invasion * soil.treatment,
                    random = ~ 1 | pasture,
                    method = 'ML',
                    data = seedling.data)
glm.summary <- anova(glm.seedling) 
# soil treatment is sig

write.csv(as.data.frame(glm.summary), 'results/GuineaSeedlingGLM.csv',
          row.names = T)

# Tukey's HSD
TukeyHSD(aov(log.guinea.grass.seedling.total ~ invasion * soil.treatment,
             data = seedling.data))

## First week (i.e. do seeds in soil from invaded sites germinate faster than 
## seeds grown in soil from uninvaded sites?)
mutate(plant.data,
       guinea.grass.seedling.week.1.rev = guinea.grass.seedling.week.1 + 1,
       log.guinea.seedling.week.1 = log(guinea.grass.seedling.week.1.rev)) -> plant.data
glm.seedling <- lme(guinea.grass.seedling.week.1 ~ invasion * soil.treatment,
                    random = ~ 1 | pasture,
                    method = 'ML',
                    data = plant.data)
glm.summary <- anova(glm.seedling) 
# invasion sig with seeds in invaded soil germinating slightly faster

write.csv(as.data.frame(glm.summary), 'results/GuineaSeedlingWeek1GLM.csv',
          row.names = T)

#--Height----
# Isolate final height (week 6) 
time.data.week6 <- filter(time.data, week == 'week6')

glm.height <- lme(log.height ~ invasion * soil.treatment,
                  random = ~ 1 | pasture,
                  method = 'ML',
                  data = time.data.week6)
glm.height <- anova(glm.height) 
# invasion sig.

write.csv(as.data.frame(glm.height), 'results/GuineaHeight.csv',
          row.names = T)

#--Growth rate-----
# Remove NA
growth.rate.data <- filter(plant.data, !is.na(growth.rate.overall))

glm.growthrate <- lme(log.growth.rate.overall ~ invasion * soil.treatment,
                  random = ~ 1 | pasture,
                  method = 'ML',
                  data = growth.rate.data)
glm.growth <- anova(glm.growthrate)
# invasion sig.

write.csv(as.data.frame(glm.growth), 'results/GuineaGrowthRate.csv',
          row.names = T)

#--Root length----
# Remove NA
root.data <- filter(plant.data, !is.na(guinea.grass.root.length.cm))
glm.rootlength <- lme(guinea.grass.root.length.cm ~ invasion * soil.treatment,
                      random = ~ 1 | pasture,
                      method = 'ML',
                      data = root.data)
glm.rootlength <- anova(glm.rootlength)
# invasion sig.

write.csv(as.data.frame(glm.rootlength), 'results/GuineaRootLength.csv',
          row.names = T)

#--Biomass----
# Remove NA and combine shoot and root biomass
plant.data %>%
  mutate(biomass.g = shoot.weight.g + root.weight.g) %>%
  filter(!is.na(biomass.g)) -> biomass.data
glm.biomass <- lme(biomass.g ~ invasion * soil.treatment,
                      random = ~ 1 | pasture,
                      method = 'ML',
                      data = biomass.data)
glm.biomass <- anova(glm.biomass)
# invasion sig.

write.csv(as.data.frame(glm.biomass), 'results/GuineaBiomass.csv',
          row.names = T)

# --------------------------------------------------------------------------#
#--<< Guinea grass: Plot as function of soil treatment and invasion >>----
# --------------------------------------------------------------------------#

#--Seedlings----
# Overall: invasion + soil treatment
seedling.invasion <- ggplot(seedling.data, aes(x = soil.treatment,
                          y = guinea.grass.seedling.total)) +
  geom_boxplot() +
  facet_grid( . ~ invasion) +
  ylab('Guinea grass\nseedling count') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/GuineaSeedling_plot.tiff', device = 'tiff',
       plot = seedling.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

# Overall: invasion only
seedling.invasion <- ggplot(seedling.data, aes(x = invasion,
                          y = guinea.grass.seedling.total)) +
  geom_boxplot() +
  #facet_grid( . ~ invasion) +
  ylab('Seedling count') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/GuineaSeedling_invasion_plot.tiff', device = 'tiff',
       plot = seedling.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

# Week 1
seedling.invasion <- ggplot(plant.data, aes(x = invasion,
                          y = guinea.grass.seedling.week.1)) +
  geom_boxplot() +
  ylab('Seedling count') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/GuineaSeedlingWeek1_plot.tiff', device = 'tiff',
       plot = seedling.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

#--Height----
# Invasion only
height.invasion <- ggplot(time.data.week6, aes(x = invasion,
                          y = height)) +
  geom_boxplot() +
  ylab('Height (cm)') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/GuineaHeight_Invasion_plot.tiff', device = 'tiff',
       plot = height.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

# Invasion and soil treatment
height.invasion <- ggplot(time.data.week6, aes(x = soil.treatment,
                          y = height)) +
  geom_boxplot() +
  facet_grid( . ~ invasion) +
  ylab('Height (cm)') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/GuineaHeight_plot.tiff', device = 'tiff',
       plot = height.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

#--Growth Rate----
# Invasion only
growth.invasion <- ggplot(growth.rate.data, aes(x = invasion,
                          y = growth.rate.overall)) +
  geom_boxplot() +
  ylab('Growth rate per day (cm)') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/GuineaGrowthRate_Invasion_plot.tiff', device = 'tiff',
       plot = growth.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

# Invasion and soil treamtent
growth.invasion <- ggplot(growth.rate.data, aes(x = soil.treatment,
                          y = growth.rate.overall)) +
  geom_boxplot() +
  facet_grid(. ~ invasion) +
  ylab('Growth rate per day (cm)') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/GuineaGrowthRate_plot.tiff', device = 'tiff',
       plot = growth.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

#--Root length----
# Invasion
root.invasion <- ggplot(root.data, aes(x = invasion,
                          y = guinea.grass.root.length.cm)) +
  geom_boxplot() +
  ylab('Root length (cm)') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/GuineaRootlength_Invasion_plot.tiff', device = 'tiff',
       plot = root.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

# Invasion and soil treatment
root.invasion <- ggplot(root.data, aes(x = soil.treatment,
                          y = guinea.grass.root.length.cm)) +
  geom_boxplot() +
  facet_grid(. ~ invasion) +
  ylab('Root length (cm)') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/GuineaRootlength_plot.tiff', device = 'tiff',
       plot = root.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

#--Biomass----
# Invasion
root.invasion <- ggplot(biomass.data, aes(x = invasion,
                          y = biomass.g)) +
  geom_boxplot() +
  ylab('Biomass (g)') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/GuineaBiomass_Invasion_plot.tiff', device = 'tiff',
       plot = root.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

# Invasion and soil treatment
root.invasion <- ggplot(biomass.data, aes(x = soil.treatment,
                          y = biomass.g)) +
  geom_boxplot() +
  facet_grid(. ~ invasion) +
  ylab('Biomass (g)') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/GuineaBiomass_plot.tiff', device = 'tiff',
       plot = root.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

# --------------------------------------------------------------------------#
#--<< Guinea grass: Summary stats >>----
# --------------------------------------------------------------------------#

# Seedling germination
plant.data %>%
  filter(!is.na(guinea.grass.seedling.total)) %>%
  group_by(soil.treatment) %>%
  summarize(mean = mean(guinea.grass.seedling.total),
            sd = sd(guinea.grass.seedling.total)) -> guinea.summary
write.csv(guinea.summary, 'results/Guinea_seedling_soil.csv',
          row.names = F)

# --------------------------------------------------------------------------#
#--<< Native community: GLM function of soil treatment and invasion >>----
# --------------------------------------------------------------------------#

#--Seedling----
## Overall
glm.seedling <- lme(log.seedling.total ~ invasion * soil.treatment,
                    random = ~ 1 | pasture,
                    method = 'ML',
                    data = native.data)
glm.summary <- anova(glm.seedling) 
# invasion is sig

write.csv(as.data.frame(glm.summary), 'results/NativeSeedlingGLM.csv',
          row.names = T)

#--Biomass----
# Transform and remove outlier
native.data %>%
  mutate(log.biomass.p.1 = log(dry.weight.g + 1)) %>%
  filter(sample != 'ESI.MS1') -> biomass.native
glm.seedling <- lme(log.biomass.p.1 ~ invasion * soil.treatment,
                    random = ~ 1 | pasture,
                    method = 'ML',
                    data = biomass.native)
glm.summary <- anova(glm.seedling) 
# invasion and soil treatment is sig

write.csv(as.data.frame(glm.summary), 'results/NativeBiomassGLM.csv',
          row.names = T)

# Tukey's HSD
TukeyHSD(aov(log.biomass.p.1 ~ invasion * soil.treatment,
             data = biomass.native))

#--Abundance----
# Overall
# Combine dicot and monocot abundance; log transform overall
native.data %>%
  mutate(abundance.overall = monocot.abundance + dicot.abundance,
         log.abund.overall = log(abundance.overall)) -> native.abund

glm.abund.overall <- lme(log.abund.overall ~ invasion * soil.treatment,
                    random = ~ 1 | pasture,
                    method = 'ML',
                    data = native.abund)
glm.abund.overall <- anova(glm.abund.overall) 
# invasion and soil treatment is sig

write.csv(as.data.frame(glm.abund.overall),
          'results/NativeAbundanceOverallGLM.csv',
          row.names = T)

# Tukey's HSD
TukeyHSD(aov(log.abund.overall ~ invasion * soil.treatment,
             data = native.abund))

# Monocot dicot
native.abund %>%
  select(sample, soil.treatment, invasion, pasture,
         monocot.abundance, dicot.abundance) %>%
  gather(key = 'functional.group', value = 'abundance',
         -sample, -soil.treatment, -invasion, -pasture) %>%
  mutate(functional.group = replace(functional.group,
                                    functional.group == 'dicot.abundance',
                        'Dicot'),
         functional.group = replace(functional.group,
                                    functional.group == 'monocot.abundance',
                        'Monocot')) -> native.abund.functional

glm.abund.func <- lme(abundance ~ invasion * functional.group,
                    random = ~ 1 | pasture,
                    method = 'ML',
                    data = native.abund.functional)
glm.abund.func <- anova(glm.abund.func) 

kruskal.test(monocot.abundance ~ invasion, 
             data = native.data)

kruskal.test(dicot.abundance ~ invasion, 
             data = native.data)

#--Species richness----
# Overall
# Combine dicot and monocot richness
native.data %>%
  mutate(richness.overall = monocot.richness + dicot.richness,
         log.richness.p.1 = log(richness.overall + 1)) -> native.richness

glm.richness.overall <- lme(log.richness.p.1 ~ invasion * soil.treatment,
                    random = ~ 1 | pasture,
                    method = 'ML',
                    data = native.richness)
glm.richness.overall <- anova(glm.richness.overall) 
# invasion is sig

write.csv(as.data.frame(glm.richness.overall),
          'results/NativeRichnessOverallGLM.csv',
          row.names = T)

# Monocot and dicot
native.richness %>%
  select(sample, soil.treatment, invasion, pasture,
         monocot.richness, dicot.richness) %>%
  gather(key = 'functional.group', value = 'richness',
         -sample, -soil.treatment, -invasion, -pasture) %>%
  mutate(functional.group = replace(functional.group,
                                    functional.group == 'dicot.richness',
                        'Dicot'),
         functional.group = replace(functional.group,
                                    functional.group == 'monocot.richness',
                        'Monocot')) -> native.rich.functional

glm.richness.func <- lme(richness ~ invasion * functional.group,
                    random = ~ 1 | pasture,
                    method = 'ML',
                    data = native.rich.functional)
glm.richness.func <- anova(glm.richness.func) 

kruskal.test(monocot.richness ~ invasion, 
             data = native.data)
kruskal.test(dicot.richness ~ invasion, 
             data = native.data)

# --------------------------------------------------------------------------#
#--<< Native community: Plot as function of soil treatment and invasion >>----
# --------------------------------------------------------------------------#

#--Seedlings----
# Overall
seedling.invasion <- ggplot(native.data, aes(x = soil.treatment,
                          y = seedling.total)) +
  geom_boxplot() +
  facet_grid( . ~ invasion) +
  ylab('Seedling count') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/NativeSeedling_plot.tiff', device = 'tiff',
       plot = seedling.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

# Invasion only
seedling.invasion <- ggplot(native.data, aes(x = invasion,
                          y = seedling.total)) +
  geom_boxplot() +
  ylab('Seedling count') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/NativeSeedling_Invasion_plot.tiff', device = 'tiff',
       plot = seedling.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

#--Biomass----
# invasion and soil treatment
native.biomass <- ggplot(biomass.native, aes(x = soil.treatment,
                          y = dry.weight.g)) +
  geom_boxplot() +
  facet_grid( . ~ invasion) +
  ylab('Native\ncommunity biomass (g)') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/NativeBiomass_plot.tiff', device = 'tiff',
       plot = native.biomass, width = 10, height = 10, units = 'cm', dpi = 300)

# invasion 
native.biomass <- ggplot(biomass.native, aes(x = invasion,
                          y = dry.weight.g)) +
  geom_boxplot() +
  ylab('Biomass (g)') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/NativeBiomass_Invasion_plot.tiff', device = 'tiff',
       plot = native.biomass, width = 10, height = 10, units = 'cm', dpi = 300)

#--Abundance----
# Overall
native.abund.plot <- ggplot(native.abund, aes(x = soil.treatment,
                          y = abundance.overall)) +
  geom_boxplot() +
  facet_grid( . ~ invasion) +
  ylab('Native\ncommunity abundance') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/NativeAbundance_plot.tiff', device = 'tiff',
       plot = native.abund.plot, width = 10, height = 10, units = 'cm', dpi = 300)

# Invasion
native.abund.plot <- ggplot(native.abund, aes(x = invasion,
                          y = abundance.overall)) +
  geom_boxplot() +
  ylab('Abundance') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/NativeAbundance_Invasion_plot.tiff', device = 'tiff',
       plot = native.abund.plot, width = 10, height = 10, units = 'cm', dpi = 300)

# Dicot and Monocot
native.abund %>%
  select(sample, soil.treatment, invasion, pasture,
         monocot.abundance, dicot.abundance) %>%
  gather(key = 'functional.group', value = 'abundance',
         -sample, -soil.treatment, -invasion, -pasture) %>%
  mutate(functional.group = replace(functional.group,
                                    functional.group == 'dicot.abundance',
                        'Dicot'),
         functional.group = replace(functional.group,
                                    functional.group == 'monocot.abundance',
                        'Monocot')) -> native.abund.functional

native.abund.func <- ggplot(native.abund.functional, aes(x = invasion,
                          y = abundance)) +
  geom_boxplot() +
  facet_grid( . ~ functional.group) +
  ylab('Abundace') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/NativeAbundanceFunctionalGroup_plot.tiff', device = 'tiff',
       plot = native.abund.func, width = 12, height = 10, units = 'cm', dpi = 300)

#--Richness----
# Invasion and soil treatment
native.rich.plot <- ggplot(native.richness, aes(x = soil.treatment,
                          y = richness.overall)) +
  geom_boxplot() +
  facet_grid( . ~ invasion) +
  ylab('Species richness') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/NativeRichness_plot.tiff', device = 'tiff',
       plot = native.rich.plot, width = 10, height = 10, units = 'cm', dpi = 300)

# Invasion only
native.rich.plot <- ggplot(native.richness, aes(x = invasion,
                          y = richness.overall)) +
  geom_boxplot() +
  #facet_grid( . ~ invasion) +
  ylab('Species richness') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/NativeRichness_Invasion_plot.tiff', device = 'tiff',
       plot = native.rich.plot, width = 10, height = 10, units = 'cm', dpi = 300)

# Dicot and Monocot
native.richness %>%
  select(sample, soil.treatment, invasion, pasture,
         monocot.richness, dicot.richness) %>%
  gather(key = 'functional.group', value = 'richness',
         -sample, -soil.treatment, -invasion, -pasture) %>%
  mutate(functional.group = replace(functional.group,
                                    functional.group == 'dicot.richness',
                        'Dicot'),
         functional.group = replace(functional.group,
                                    functional.group == 'monocot.richness',
                        'Monocot')) -> native.rich.functional

native.rich.func <- ggplot(native.rich.functional, aes(x = invasion,
                          y = richness)) +
  geom_boxplot() +
  facet_grid( . ~ functional.group) +
  ylab('Species richness') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/NativeRichnessFunctionalGroup_plot.tiff', device = 'tiff',
       plot = native.rich.func, width = 12, height = 10, units = 'cm', dpi = 300)

# --------------------------------------------------------------------------#
#--<< Native community: Summary stats >>----
# --------------------------------------------------------------------------#

# Biomass by invasion status
biomass.native %>%
  group_by(invasion) %>%
  summarize(mean.overall = mean(dry.weight.g),
            sd.overall = sd(dry.weight.g)) -> native.biomass

write.csv(native.biomass, 'results/native_biomass_invasion.csv',
          row.names = F)

# Biomass by soil treatment
biomass.native %>%
  group_by(soil.treatment) %>%
  summarize(mean.overall = mean(dry.weight.g),
            sd.overall = sd(dry.weight.g)) -> native.biomass

write.csv(native.biomass, 'results/native_biomass_soil.csv',
          row.names = F)

# Abundance by invasion status
native.abund %>%
  group_by(invasion) %>%
  summarise(mean.overall = mean(abundance.overall),
            sd.overall = sd(abundance.overall),
            mean.monocot = mean(monocot.abundance),
            sd.monocot = sd(monocot.abundance),
            mean.dicot = mean(dicot.abundance),
            sd.dicot = sd(dicot.abundance)) -> native.summary
write.csv(native.summary, 'results/native_abundance_invasion.csv',
          row.names = F)

# Abundance by soil treatment
native.abund %>%
  group_by(soil.treatment) %>%
  summarise(mean = mean(abundance.overall),
            sd = sd(abundance.overall)) -> native.summary
write.csv(native.summary, 'results/native_abundance_soil.csv',
          row.names = F)
  
# Richness by invasion status
native.richness %>%
  group_by(invasion) %>%
  summarise(mean.overall = mean(richness.overall),
            sd.overall = sd(richness.overall),
            mean.monocot = mean(monocot.richness),
            sd.monocot = sd(monocot.richness),
            mean.dicot = mean(dicot.richness),
            sd.dicot = sd(dicot.richness)) -> native.summary
write.csv(native.summary, 'results/native_richness_invasion.csv',
          row.names = F)


