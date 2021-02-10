## Script to assess differences in the seedlings emerging from the native
## seed bank bases on soil treatment (individual soil samples, ISS, and 
## multiple soil samples, MSS), invasion, and pasture (geographic location).
## Written by Dr. Elizabeth Bowman on Sept. 29, 2020

library(tidyverse)
library(nlme)

seedling.data <- read.csv('data/data.MSS.ISS.csv', as.is = T)
native.data <- read.csv('data/data.NativeCommunity.csv', as.is =T)

# --------------------------------------------------------------------------#
#--Data manipulation and evaluation of normality----
# --------------------------------------------------------------------------#

#--Native seedlings-----
# create new column merging the 9.15 and 9.14 seedling data
seedling.data$seedlings.9.14.20 <- replace_na(seedling.data$seedlings.9.14.20, 0)
seedling.data$seedlings.9.15.20 <- replace_na(seedling.data$seedlings.9.15.20, 0)

seedling.data %>%
  mutate(seedling.total = seedlings.9.14.20 + seedlings.9.15.20) -> seedling.data

# Mean and sd
seedling.data %>%
  group_by(invasion) %>%
  summarise(mean = mean(seedling.total), sd = sd(seedling.total))

# check distribution
hist(seedling.data$seedling.total)
seedling.data$log.seedling.total <- log(seedling.data$seedling.total)
hist(seedling.data$log.seedling.total)

#--Native community----
# add total abundance and richness (monocot + dicot) and reconfigure data
native.data %>%
  mutate(total.abundance = monocot.abundance + dicot.abundance,
         total.richness = monocot.richness + dicot.richness) -> native.data
native.data %>%
  gather(key = functional.group, value = abundance,
         -sample, -soil.treatment, -invasion, -dry.weight.g,
         -monocot.richness,-dicot.richness, -total.abundance, -total.richness) %>%
  gather(key = functional.group2, value = richness,
         -sample, -soil.treatment, -invasion, -dry.weight.g,
         -abundance,-functional.group, -total.abundance, -total.richness) -> native.data.re
native.data.re <- native.data.re[!names(native.data.re) == 'functional.group2']
native.data.re[native.data.re$functional.group == 'monocot.abundance',
               'functional.group'] <- 'Monocot'
native.data.re[native.data.re$functional.group == 'dicot.abundance',
               'functional.group'] <- 'Dicot'

# check distribution
hist(native.data.re$abundance)
hist(log(native.data.re$abundance))
native.data.ed <- native.data.re[!native.data.re$abundance == 0,]
shapiro.test(log(native.data.ed$abundance))
# Abundance is non-normal; use nonparametric methods

hist(native.data.re$richness)
hist(log(native.data.re$richness))
native.data.ed <- native.data.re[!native.data.re$richness == 0,]
shapiro.test(log(native.data.ed$richness))

# Abundance is non-normal; use nonparametric methods

hist(log(native.data$total.abundance))
shapiro.test(log(native.data$total.abundance)) # normal
hist(log(native.data$total.richness))
shapiro.test(log(native.data$total.richness)) # nonnormal

# --------------------------------------------------------------------------#
#--ANOVA function of soil treatment and invasion----
# --------------------------------------------------------------------------#

#--Native seed bank germination
# ANOVA
lm.seedling <- lm(log.seedling.total ~ invasion * soil.treatment,
                  data = seedling.data)
anova.seedling <- anova(lm.seedling)
anova.seedling

anova.seedling <- as.matrix(anova.seedling)

write.csv(anova.seedling, 'results/NativeSeedlingResults.csv',
          row.names = F)

#--Native community: Total
## Abundance
lm.abund <- lm(total.abundance ~ invasion * soil.treatment,
               data = native.data)
anova.abund <- anova(lm.abund)
anova.abund

## Richness
lm.richness <- lm(total.richness ~ invasion * soil.treatment,
               data = native.data)
anova.richness <- anova(lm.richness)
anova.richness

#--Native community: monocot
native.data.monocot <- native.data.re[native.data.re$functional.group == 'Monocot',]

# abundance
# Kruskal Wallis: abundance as a function of invasion
kruskal.mono.inv.ab <- kruskal.test(abundance ~ invasion,
                                 data = native.data.monocot) # Significant difference
# Kruskal Wallis: abundance as a function of soil treatment
kruskal.mono.soil.ab <- kruskal.test(abundance ~ soil.treatment,
                                      data = native.data.monocot)
# richness
# Kruskal Wallis: richness as a function of invasion
kruskal.mono.inv.ri <- kruskal.test(richness ~ invasion,
                                 data = native.data.monocot) # Significant difference
# Kruskal Wallis: richness as a function of soil treatment
kruskal.mono.soil.ri <- kruskal.test(richness ~ soil.treatment,
                                      data = native.data.monocot)

#--Native community: dicot
native.data.dicot <- native.data.re[native.data.re$functional.group == 'Dicot',]

# abundance
# Kruskal Wallis: abundance as a function of invasion
kruskal.di.inv.ab <- kruskal.test(abundance ~ invasion,
                                 data = native.data.dicot) # Significant difference
# Kruskal Wallis: abundance as a function of soil treatment
kruskal.di.soil.ab <- kruskal.test(abundance ~ soil.treatment,
                                      data = native.data.dicot)
# richness
# Kruskal Wallis: richness as a function of invasion
kruskal.di.inv.ri <- kruskal.test(richness ~ invasion,
                                 data = native.data.dicot) # Significant difference
# Kruskal Wallis: richness as a function of soil treatment
kruskal.di.soil.ri <- kruskal.test(richness ~ soil.treatment,
                                      data = native.data.dicot)

# --------------------------------------------------------------------------#
#--Plot as function of soil treatment and invasion----
# --------------------------------------------------------------------------#
#--Seedbank----
# bank.treatment <- ggplot(seedling.data, aes(x = soil.treatment,
#                           y = seedling.total)) +
#   geom_boxplot() +
#   ylab('Seedling count') +
#   xlab('') +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black")) +
#   theme(axis.text = element_text(color = "black"))
#   
# ggsave('figures/SeedlingGermination_SoilTreatment.pdf', device = 'tiff',
#        plot = bank.treatment, width = 6, height = 6, units = 'cm', dpi = 300)

bank.inv <- ggplot(seedling.data, aes(x = invasion,
                          y = seedling.total)) +
  geom_boxplot() +
  ylab('Seedling count') +
  xlab('') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black")) +
  scale_x_discrete(breaks=c('invaded', 'uninvaded'),
                      labels=c('Invaded','Uninvaded'))
  
ggsave('figures/SeedlingGermination_Invasion.tiff', device = 'tiff',
       plot = bank.inv, width = 10, height = 10, units = 'cm', dpi = 300)

bank.invsoil <- ggplot(seedling.data, aes(x = invasion,
                          y = seedling.total)) +
  geom_boxplot() +
  facet_grid(. ~ soil.treatment) +
  ylab('Seedling count') +
  xlab('') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black")) +
  scale_x_discrete(breaks=c('invaded', 'uninvaded'),
                      labels=c('Invaded','Uninvaded'))
  
ggsave('figures/SeedlingGermination_InvasionSoilTreatment.tiff', device = 'tiff',
       plot = bank.invsoil, width = 10, height = 10, units = 'cm', dpi = 300)
#--Native community: Invasion and Functional group----
# Abundance as function of invasion and functional group
nat.invgroup <- ggplot(native.data.re, aes(x = invasion,
                          y = abundance)) +
  geom_boxplot() +
  facet_grid(. ~ functional.group) +
  ylab('Abundance') +
  xlab('') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black")) +
  scale_x_discrete(breaks=c('invaded', 'uninvaded'),
                      labels=c('Invaded','Uninvaded'))
  
ggsave('figures/NativeComm_AbInvasionFunctGroup.tiff', device = 'tiff',
       plot = nat.invgroup, width = 10, height = 10, units = 'cm', dpi = 300)

# Richness as function of invasion and functional group
nat.invgroup <- ggplot(native.data.re, aes(x = invasion,
                          y = richness)) +
  geom_boxplot() +
  facet_grid(. ~ functional.group) +
  ylab('Species richness') +
  xlab('') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black")) +
  scale_x_discrete(breaks=c('invaded', 'uninvaded'),
                      labels=c('Invaded','Uninvaded'))
  
ggsave('figures/NativeComm_RichInvasionFunctGroup.tiff', device = 'tiff',
       plot = nat.invgroup, width = 10, height = 10, units = 'cm', dpi = 300)

# Overall as function of invasion
nat.invtotal <- ggplot(native.data, aes(x = invasion,
                          y = total.abundance)) +
  geom_boxplot() +
  ylab('Total abundance') +
  xlab('') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black")) +
  scale_x_discrete(breaks=c('invaded', 'uninvaded'),
                      labels=c('Invaded','Uninvaded'))
  
ggsave('figures/NativeComm_TotalAbInvasionFunctGroup.tiff', device = 'tiff',
       plot = nat.invtotal, width = 10, height = 10, units = 'cm', dpi = 300)

nat.invtotal <- ggplot(native.data, aes(x = invasion,
                          y = total.richness)) +
  geom_boxplot() +
  ylab('Total species richness') +
  xlab('') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black")) +
  scale_x_discrete(breaks=c('invaded', 'uninvaded'),
                      labels=c('Invaded','Uninvaded'))
  
ggsave('figures/NativeComm_TotalRichInvasionFunctGroup.tiff', device = 'tiff',
       plot = nat.invtotal, width = 10, height = 10, units = 'cm', dpi = 300)  

#--Native community: Soil Treatment and Invasion
nat.soiltotal <- ggplot(native.data, aes(x = invasion,
                          y = total.abundance)) +
  geom_boxplot() +
  facet_grid( . ~ soil.treatment) +
  ylab('Total abundance') +
  xlab('') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black")) +
  scale_x_discrete(breaks=c('invaded', 'uninvaded'),
                      labels=c('Invaded','Uninvaded'))
  
ggsave('figures/NativeComm_TotalAbInvasionSoil.tiff', device = 'tiff',
       plot = nat.soiltotal, width = 10, height = 10, units = 'cm', dpi = 300)  

nat.soiltotal <- ggplot(native.data, aes(x = invasion,
                          y = total.richness)) +
  geom_boxplot() +
  facet_grid( . ~ soil.treatment) +
  ylab('Total species richness') +
  xlab('') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black")) +
  scale_x_discrete(breaks=c('invaded', 'uninvaded'),
                      labels=c('Invaded','Uninvaded'))
  
ggsave('figures/NativeComm_TotalRichInvasionSoil.tiff', device = 'tiff',
       plot = nat.soiltotal, width = 10, height = 10, units = 'cm', dpi = 300)  
