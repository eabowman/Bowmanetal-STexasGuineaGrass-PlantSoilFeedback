## Script to assess differences in soil fungi as a function of 
## soil treatment (individual soil samples, ISS, and multiple soil samples, MSS)
## and invasion (Invaded and Non-invaded).
## Written by Dr. Elizabeth Bowman on Nov. 11, 2020

# install.packages('tidyverse')
# install.packages('nlme')
# install.packages('vegan')
library(tidyverse)
library(nlme)
library(vegan)
library(stats)

time.data <- read.csv('data_output/data.microbial.csv', as.is = T)

# factor variables
time.data$soil.treatment <- as.factor(time.data$soil.treatment)
time.data$invasion <- factor(time.data$invasion, 
                              levels = c('Invaded','Uninvaded'),
                              labels = c('Invaded','Non-invaded'))
time.data$pasture <- as.factor(time.data$pasture)
time.data$time.point <- as.factor(as.character(time.data$time.point))

# --------------------------------------------------------------------------#
#--<< Fungal abundance and richness as a function of soil treatment and invasion >>----
# Final time point only (week 14)
# --------------------------------------------------------------------------#

# isolate time point 3 data
time.data %>%
  filter(time.point == '3') %>%
  mutate(log.abund = log(abundance.fungal),
         log.richness = log(spec.richness.fungal)) -> fun.data

# assess normality
hist(fun.data$log.abund)
shapiro.test(fun.data$log.abund)
hist(fun.data$log.richness)
shapiro.test(fun.data$log.richness)

#--Fungal abundance----
## Overall
glm.abund <- lme(log.abund ~ invasion * soil.treatment,
                    random = ~ 1 | pasture,
                    method = 'ML',
                    data = fun.data)
glm.summary <- anova(glm.abund) 
# no factor is sig

write.csv(as.data.frame(glm.summary), 'results/FungalAbundanceGLM.csv',
          row.names = T)

#--Fungal species richness----
## Overall
glm.rich <- lme(log.richness ~ invasion * soil.treatment,
                    random = ~ 1 | pasture,
                    method = 'ML',
                    data = fun.data)
glm.summary <- anova(glm.rich) 
# invasion is trending toward significane

write.csv(as.data.frame(glm.summary), 'results/FungalRichnessGLM.csv',
          row.names = T)

# --------------------------------------------------------------------------#
#--<< Time point 3: Plot species richness as a function of invasion >>----
# --------------------------------------------------------------------------#

#--Abundance----
# Invasion
abundance.invasion <- ggplot(fun.data, aes(x = invasion,
                          y = log.abund)) +
  geom_boxplot() +
  ylab('Fungal abundance') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/FungalRichnessInvasion_plot.tiff', device = 'tiff',
       plot = richness.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

#--Species richness----
# Invasion
richness.invasion <- ggplot(fun.data, aes(x = invasion,
                          y = spec.richness.fungal)) +
  geom_boxplot() +
  ylab('Fungal species richness') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/FungalRichnessInvasion_plot.tiff', device = 'tiff',
       plot = richness.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

# --------------------------------------------------------------------------#
#--<< Fungal abundance and richness as a function of time >>----
# --------------------------------------------------------------------------#

#--Abundance---
# Time
abundance.Time <- ggplot(time.data, aes(x = time.point,
                          y = CFU.fungal)) +
  geom_boxplot() +
  ylab('Fungal CFUs') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/FungalAbundanceTime_plot.tiff', device = 'tiff',
       plot = abundance.Time, width = 10, height = 10, units = 'cm', dpi = 300)

#--Species richness----
# Time
richness.Time <- ggplot(time.data, aes(x = time.point,
                          y = spec.richness.fungal)) +
  geom_boxplot() +
  ylab('Fungal species richness') +
  xlab('') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text = element_text(color = "black"))

ggsave('figures/FungalRichnessTime_plot.tiff', device = 'tiff',
       plot = richness.Time, width = 10, height = 10, units = 'cm', dpi = 300)

# --------------------------------------------------------------------------#
#--<< Fungal abundance and richness Summary >>----
# --------------------------------------------------------------------------#
fun.data %>%
  group_by(soil.treatment) %>%
  summarize(mean.abund = mean(CFU.fungal),
            sd.abund = sd(CFU.fungal),
            med.abund = median(CFU.fungal))

fun.data %>%
  group_by(invasion) %>%
  summarize(mean.rich = mean(spec.richness.fungal),
            sd.rich = sd(spec.richness.fungal))
