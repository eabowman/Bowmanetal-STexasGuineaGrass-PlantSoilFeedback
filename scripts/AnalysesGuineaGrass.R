## Script to assess differences in germination and growth of Guinea grass
## as a function of soil treatment (individual soil sampling, ISS, and 
## mixed soil sampling, MSS) and invasion (invaded and non-invaded).
## Written by Dr. Elizabeth Bowman on Nov. 11, 2020

## Analysis shown in Table II and Fig. I and IIIa

plant.data <- read.csv('data/data.Guinea.csv', as.is = T)
time.data <- read.csv('data/data.GuineaHeightOverTime.csv', as.is = T)

# factor variables
plant.data$soil.treatment <- as.factor(plant.data$soil.treatment)
plant.data$pasture <- as.factor(plant.data$pasture)

time.data$soil.treatment <- as.factor(time.data$soil.treatment)
time.data$pasture <- as.factor(time.data$pasture)

# --------------------------------------------------------------------------#
# Guinea grass: GLM function of soil treatment and invasion ----
# --------------------------------------------------------------------------#

## Seedlings----
#Remove -inf
seedling.data <- filter(plant.data, !is.na(log.guinea.grass.seedling.total))
### Overall ----
glm.seedling <- lme(log.guinea.grass.seedling.total ~ invasion * soil.treatment,
                    random = ~ 1 | pasture,
                    method = 'ML',
                    data = seedling.data)
glm.summary <- anova(glm.seedling) 
# soil treatment is sig

write.csv(as.data.frame(glm.summary), 'results/GuineaSeedling_OverallGLM.csv',
          row.names = T)

# Tukey's HSD
TukeyHSD(aov(log.guinea.grass.seedling.total ~ invasion * soil.treatment,
             data = seedling.data))

### First week (i.e. do seeds in soil from invaded sites germinate faster than seeds grown in soil from noninvaded sites?) ----
## add in two columns (transform Guinea grass seedling data from the 1st week)
mutate(plant.data,
       guinea.grass.seedling.week.1.rev = guinea.grass.seedling.week.1 + 1,
       log.guinea.seedling.week.1 = log(guinea.grass.seedling.week.1.rev)) -> plant.data

glm.seedling <- lme(guinea.grass.seedling.week.1 ~ invasion * soil.treatment,
                    random = ~ 1 | pasture,
                    method = 'ML',
                    data = plant.data)
glm.summary <- anova(glm.seedling) 
# invasion sig with seeds in invaded soil germinating slightly faster

write.csv(as.data.frame(glm.summary), 'results/GuineaSeedling_Week1GLM.csv',
          row.names = T)

## Height----
# Isolate final height (week 6) 
time.data.week6 <- filter(time.data, week == 'week6')

glm.height <- lme(log.height ~ invasion * soil.treatment,
                  random = ~ 1 | pasture,
                  method = 'ML',
                  data = time.data.week6)
glm.height <- anova(glm.height) 
# invasion sig.

write.csv(as.data.frame(glm.height), 'results/GuineaHeight_Week6GLM.csv',
          row.names = T)

## Growth rate-----
# Remove NA
growth.rate.data <- filter(plant.data, !is.na(growth.rate.overall))

glm.growthrate <- lme(log.growth.rate.overall ~ invasion * soil.treatment,
                  random = ~ 1 | pasture,
                  method = 'ML',
                  data = growth.rate.data)
glm.growth <- anova(glm.growthrate)
# invasion sig.

write.csv(as.data.frame(glm.growth), 'results/GuineaGrowthRate_GLM.csv',
          row.names = T)

## Root length----
# Remove NA
root.data <- filter(plant.data, !is.na(guinea.grass.root.length.cm))

glm.rootlength <- lme(guinea.grass.root.length.cm ~ invasion * soil.treatment,
                      random = ~ 1 | pasture,
                      method = 'ML',
                      data = root.data)
glm.rootlength <- anova(glm.rootlength)
# invasion sig.

write.csv(as.data.frame(glm.rootlength), 'results/GuineaRootLength_GLM.csv',
          row.names = T)

## Biomass----
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

write.csv(as.data.frame(glm.biomass), 'results/GuineaBiomass_GLM.csv',
          row.names = T)

# --------------------------------------------------------------------------#
# Guinea grass: Plot as function of soil treatment and invasion ----
# --------------------------------------------------------------------------#
## Height----
# Isolate final height (week 6) 
time.data.week6 <- filter(time.data, week == 'week6')
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

ggsave('figures/Fig1A.tiff', device = 'tiff',
       plot = height.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

# Invasion and soil treatment
# not used in manuscript
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

## Root length----
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

ggsave('figures/Fig1B.tiff', device = 'tiff',
       plot = root.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

# Invasion and soil treatment
# not used in manuscript
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

## Biomass----
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

ggsave('figures/Fig1C.tiff', device = 'tiff',
       plot = root.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

# Invasion and soil treatment
# not used in manuscript
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

## Seedlings----
#Remove -inf
seedling.data <- filter(plant.data, !is.na(log.guinea.grass.seedling.total))
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

ggsave('figures/Fig3A.tiff', device = 'tiff',
       plot = seedling.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

# Week 1
seedling.invasion <- ggplot(plant.data, aes(x = invasion,
                          y = guinea.grass.seedling.week.1)) +
  geom_boxplot() +
  ylab('Seedling count: week 1') +
  xlab('') +
  ggtitle("Guinea grass") +
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

ggsave('figures/SupplementaryFigS2a.tiff', device = 'tiff',
       plot = seedling.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

# --------------------------------------------------------------------------#
# Guinea grass: Summary stats ----
# --------------------------------------------------------------------------#

# Seedling germination
plant.data %>%
  filter(!is.na(guinea.grass.seedling.total)) %>%
  group_by(soil.treatment) %>%
  summarize(mean = mean(guinea.grass.seedling.total),
            sd = sd(guinea.grass.seedling.total)) -> guinea.summary

write.csv(guinea.summary, 'results/Guinea_Seedling_SoilTreatment.csv',
          row.names = F)

