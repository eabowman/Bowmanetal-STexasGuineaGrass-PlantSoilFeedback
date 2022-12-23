## Script to assess differences in germination and growth of the native seed 
## bank as a function of soil treatment (individual soil sampling, ISS, and 
## mixed soil sampling, MSS) and invasion (invaded and non-invaded).
## Written by Dr. Elizabeth Bowman on Nov. 11, 2020

## Analysis shown in Table II, Fig. 2, and Supplementary fig S2b, S3b, and S3c

# Read in data files
native.data <- read.csv('data/data.NativeCommunity.csv', as.is = T)

# factor variables soil treatment and pasture
native.data$soil.treatment <- as.factor(native.data$soil.treatment)
native.data$pasture <- as.factor(native.data$pasture)

# --------------------------------------------------------------------------#
# Native community: GLM function of soil treatment and invasion ----
# --------------------------------------------------------------------------#

## Seedling----
### Total seedling count ----
glm.seedling <- lme(log.seedling.total ~ invasion * soil.treatment,
                    random = ~ 1 | pasture,
                    method = 'ML',
                    data = native.data)
glm.summary <- anova(glm.seedling) 
# invasion and invasion * soil treatment is sig

write.csv(as.data.frame(glm.summary), 'results/NativeSeedling_GLM.csv',
          row.names = T)

TukeyHSD(aov(lm(log.seedling.total ~ invasion * soil.treatment, data = native.data)))

### Seedling emergence in the first week ----
native.data <- mutate(native.data, log.seedlings.week1 = log(seedlings.week1+1))
glm.seedling <- lme(log.seedlings.week1 ~ invasion * soil.treatment,
                    random = ~ 1 | pasture,
                    method = 'ML',
                    data = native.data)
glm.summary <- anova(glm.seedling) 
# invasion is sig

write.csv(as.data.frame(glm.summary), 'results/NativeSeedling_Week1_GLM.csv',
          row.names = T)

## Plant abundance----
### Total count ----
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
          'results/NativeAbundanceOverall_GLM.csv',
          row.names = T)

# Tukey's HSD
TukeyHSD(aov(log.abund.overall ~ invasion * soil.treatment,
             data = native.abund))

### Grouped by plant functional group: dicot and monocot ----
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

write.csv(as.data.frame(glm.abund.func),
          'results/NativeAbundanceFunctionalGroup_GLM.csv',
          row.names = T)

## Biomass----
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

write.csv(as.data.frame(glm.summary), 'results/NativeBiomass_GLM.csv',
          row.names = T)

# Tukey's HSD
TukeyHSD(aov(log.biomass.p.1 ~ invasion * soil.treatment,
             data = biomass.native))

## Species richness: Morphological species richness----
### Total final morpho species richness ----
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
          'results/NativeRichnessOverall_GLM.csv',
          row.names = T)

### Monocot and dicot ----
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

write.csv(as.data.frame(glm.richness.func),
          'results/NativeRichnessFunctionalGroup_GLM.csv',
          row.names = T)

# --------------------------------------------------------------------------#
# Native community: Plot as function of soil treatment and invasion ----
# --------------------------------------------------------------------------#

## Seedlings----
# Invasion only: total seedlings
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

ggsave('figures/Fig2a.tiff', device = 'tiff',
       plot = seedling.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

# Invasion only: seedlings first week
seedling.invasion <- ggplot(native.data, aes(x = invasion,
                          y = seedlings.week1)) +
  geom_boxplot() +
  ylab('Seedling count: week 1') +
  xlab('') +
  ggtitle('Seed bank') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"),
        plot.title = element_text(color = "black",
                                  size = 16))

ggsave('figures/SupplementaryFigS2b.tiff', device = 'tiff',
       plot = seedling.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

## Biomass----
### invasion and soil treatment ----
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

ggsave('figures/SupplementaryFigS3c.tiff', device = 'tiff',
       plot = native.biomass, width = 10, height = 10, units = 'cm', dpi = 300)

### invasion ----
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

ggsave('figures/Fig2c.tiff', device = 'tiff',
       plot = native.biomass, width = 10, height = 10, units = 'cm', dpi = 300)

## Abundance----
### Overall ----
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

ggsave('figures/SupplementaryFigS3b.tiff', device = 'tiff',
       plot = native.abund.plot, width = 10, height = 10, units = 'cm', dpi = 300)

### Invasion ---
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

ggsave('figures/Fig2b.tiff', device = 'tiff',
       plot = native.abund.plot, width = 10, height = 10, units = 'cm', dpi = 300)

### Functional group ----
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

ggsave('figures/Fig3a.tiff', device = 'tiff',
       plot = native.abund.func, width = 12, height = 10, units = 'cm', dpi = 300)

## Richness----
### Invasion only ----
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

ggsave('figures/Fig2d.tiff', device = 'tiff',
       plot = native.rich.plot, width = 10, height = 10, units = 'cm', dpi = 300)

### Functional groups----
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

ggsave('figures/Figure3b.tiff', device = 'tiff',
       plot = native.rich.func, width = 12, height = 10, units = 'cm', dpi = 300)

# --------------------------------------------------------------------------#
# Native community: Summary stats ----
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

