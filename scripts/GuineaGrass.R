## Script to assess differences in the emergence of Guinea grass eon soil treatment (individual soil samples, ISS, and 
## multiple soil samples, MSS), invasion, and pasture (geographic location).
## Written by Dr. Elizabeth Bowman on Nov. 11, 2020

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
  mutate(seedling.total = seedlings.9.14.20 + seedlings.9.15.20,
         log.seedling.total = log(seedling.total)) -> seedling.data
# log evaluation based on assessment indicating data non-normal
# to see these analyses look at the R file 'NativeSeedBank.R'

#--Guinea grass seed germination----
seedling.data %>%
  mutate(guinea.grass.seedling.total = guinea.grass.seedling.9.28.20 +
    guinea.grass.seedling.10.12.20 + guinea.grass.seedling.10.23.20,
    log.guinea.grass.seedling.total = log(guinea.grass.seedling.total)) %>%
  filter(guinea.grass.seedling.total != 0) -> seedling.data

#--Check normality of data
hist(seedling.data$guinea.grass.seedling.total)
hist(seedling.data$log.guinea.grass.seedling.total)
shapiro.test(seedling.data$log.guinea.grass.seedling.total)

# not normal, use non-parametric tests

#--Mean and sd
seedling.data %>%
  group_by(invasion, soil.treatment) %>%
  summarise(mean.nativegerm = mean(seedling.total),
            sd.nativegerm = sd(seedling.total),
            mean.guineagerm = mean(guinea.grass.seedling.total),
            sd.guineagerm = sd(guinea.grass.seedling.total)) -> germ.summary

# write.csv(germ.summary, 'results/GerminationSummaryData.csv',
#           row.names = F)

#--Guinea grass height data----
#--Transform to make the height columns into one keeping invaion
# and soil treatment columns
seedling.data %>%
  select(invasion, soil.treatment,
         guinea.grass.height.10.29.20, guinea.grass.height.11.6.20) %>%
  gather(key = week, value = height, -invasion, -soil.treatment) %>%
  mutate(week = replace(week, week == 'guinea.grass.height.10.29.20',
                        'week1'),
         week = replace(week, week == 'guinea.grass.height.11.6.20',
                        'week2'),
         log.height = log(height)) %>%
  filter(height != 0) -> time.data

#--Check normality of data
hist(time.data$height)
shapiro.test(time.data$height) # not normal
hist(time.data$log.height)
shapiro.test(time.data$log.height)

# Data non-normal even after transformation, use non-parametric methods

# --------------------------------------------------------------------------#
#--Guinea grass seedlings and height: GLS function of soil treatment and invasion----
# --------------------------------------------------------------------------#

#--Seedlings
gls.seedling <- gls(guinea.grass.seedling.total ~ invasion * soil.treatment,
                  data = seedling.data)
gls.summary <- summary(gls.seedling) 
# invasion and invasion * soil treatment almost sig

# write.csv(anova.seedling, 'results/NativeSeedlingResults.csv',
#           row.names = F)

#--Height
gls.heigh <- gls(height ~ invasion * soil.treatment,
                  data = time.data)
gls.height <- summary(gls.seedling) # invasion sig.

# write.csv(anova.seedling, 'results/NativeSeedlingResults.csv',
#           row.names = F)

# --------------------------------------------------------------------------#
#--Guinea grass seedlings and height: Plot as function of soil treatment and invasion----
# --------------------------------------------------------------------------#

#--Seedlings
# Invasion
seedling.invasion <- ggplot(seedling.data, aes(x = invasion,
                          y = guinea.grass.seedling.total)) +
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

ggsave('figures/GuineaSeedling_invasion.tiff', device = 'tiff',
       plot = seedling.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

# soil treatment
seedling.invsoil <- ggplot(seedling.data, aes(x = invasion,
                          y = guinea.grass.seedling.total)) +
  geom_boxplot() +
  facet_grid(. ~ soil.treatment) +
  ylab('Seedling count') +
  xlab('') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black"))  +
  scale_x_discrete(breaks=c('invaded', 'uninvaded'),
                      labels=c('Invaded','Uninvaded'))
  
ggsave('figures/GuineaSeedling_InvasionSoilTreatment.tiff', device = 'tiff',
       plot = seedling.invsoil, width = 10, height = 10, units = 'cm', dpi = 300)

#--Height
# Invasion
height.invasion <- ggplot(time.data, aes(x = invasion,
                          y = height)) +
  geom_boxplot() +
  ylab('Height (cm)') +
  xlab('') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black")) +
  scale_x_discrete(breaks=c('invaded', 'uninvaded'),
                      labels=c('Invaded','Uninvaded'))

ggsave('figures/GuineaHeight_invasion.tiff', device = 'tiff',
       plot = height.invasion, width = 10, height = 10, units = 'cm', dpi = 300)

# soil treatment
height.soil <- ggplot(time.data, aes(x = soil.treatment,
                          y = height)) +
  geom_boxplot() +
  ylab('Seedling count') +
  xlab('') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black"))
  
ggsave('figures/GuineaHeight_invasion.tiff', device = 'tiff',
       plot = height.soil, width = 10, height = 10, units = 'cm', dpi = 300)
