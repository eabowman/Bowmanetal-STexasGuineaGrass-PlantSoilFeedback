## Script to assess differences in the seedlings emerging from the
## seed bank bases on soil treatment (individual soil samples, ISS, and 
## multiple soil samples, MSS), invasion, and pasture (geographic location).
## Written by Dr. Elizabeth Bowman on Sept. 29, 2020

library(tidyverse)
library(nlme)

seedling.data <- read.csv('data/data.MSS.ISS.csv', as.is = T)

# --------------------------------------------------------------------------#
#--Data manipulation and evaluation of normality----
# --------------------------------------------------------------------------#

#--Native seed bank-----
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

# --------------------------------------------------------------------------#
#--Native seed bank germination: ANOVA function of soil treatment and invasion----
# --------------------------------------------------------------------------#

#--ANOVA
lm.seedling <- lm(log.seedling.total ~ invasion * soil.treatment,
                  data = seedling.data)
anova.seedling <- anova(lm.seedling)
anova.seedling

anova.seedling <- as.matrix(anova.seedling)

write.csv(anova.seedling, 'results/NativeSeedlingResults.csv',
          row.names = F)

# --------------------------------------------------------------------------#
#--Native seed bank germination: Plot as function of soil treatment and invasion----
# --------------------------------------------------------------------------#

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
       plot = bank.inv, width = 6, height = 6, units = 'cm', dpi = 300)

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
       plot = bank.invsoil, width = 6, height = 6, units = 'cm', dpi = 300)

  