## Script to analyze soil nutrients and texture to test for differences
## based on invasion, pasture (geography), and autoclave time
## Written by Dr. Elizabeth Bowman Oct. 4, 2020

# install.packages('devtools')
library(devtools)
#install_github('vqv/ggbiplot')
library(tidyverse)
library(ggbiplot)

# change number of decimal points format to 4
options(digits = 4)

soil.data <- read.csv('data/data.autoclave.effect.csv', as.is = T)

# log transform non-normal data
soil.data %>%
  mutate(log.EC = log(EC),
         log.P = log(P),
         log.S = log(S)) -> soil.data

# --------------------------------------------------------------------------#
#--Soil characteristics as a function of autoclave time: One Way ANOVA ----
# --------------------------------------------------------------------------#

# Make results table
autoclave.results <- data.frame(soil.characteristics = 
                                  c('pH','log.EC','Nitrate','log.P','K','Mg',
                                    'log.S','Na','Ca'),
                                df.1 = NA,
                                df.2 = NA,
                                F.stat = NA,
                                p = NA)

# pH
lm.pH <- lm(pH ~ autoclave.time, data = soil.data)
anova.pH <- anova(lm.pH)
autoclave.results[autoclave.results$soil.characteristics == 'pH',
                    'df.1'] <- anova.pH$Df[1]
autoclave.results[autoclave.results$soil.characteristics == 'pH',
                    'df.2'] <- anova.pH$Df[2]
autoclave.results[autoclave.results$soil.characteristics == 'pH',
                    'F.stat'] <- anova.pH$`F value`[1]
autoclave.results[autoclave.results$soil.characteristics == 'pH',
                    'p'] <- anova.pH$`Pr(>F)`[1]

# log.EC
lm.EC <- lm(log.EC ~ autoclave.time, data = soil.data)
anova.EC <- anova(lm.EC)
autoclave.results[autoclave.results$soil.characteristics == 'log.EC',
                    'df.1'] <- anova.EC$Df[1]
autoclave.results[autoclave.results$soil.characteristics == 'log.EC',
                    'df.2'] <- anova.EC$Df[2]
autoclave.results[autoclave.results$soil.characteristics == 'log.EC',
                    'F.stat'] <- anova.EC$`F value`[1]
autoclave.results[autoclave.results$soil.characteristics == 'log.EC',
                    'p'] <- anova.EC$`Pr(>F)`[1]

# Nitrate
lm.N <- lm(Nitrate ~ autoclave.time, data = soil.data)
anova.N <- anova(lm.N)
autoclave.results[autoclave.results$soil.characteristics == 'Nitrate',
                    'df.1'] <- anova.N$Df[1]
autoclave.results[autoclave.results$soil.characteristics == 'Nitrate',
                    'df.2'] <- anova.N$Df[2]
autoclave.results[autoclave.results$soil.characteristics == 'Nitrate',
                    'F.stat'] <- anova.N$`F value`[1]
autoclave.results[autoclave.results$soil.characteristics == 'Nitrate',
                    'p'] <- anova.N$`Pr(>F)`[1]

# log.P
lm.P <- lm(log.P ~ autoclave.time, data = soil.data)
anova.P <- anova(lm.P)
autoclave.results[autoclave.results$soil.characteristics == 'log.P',
                    'df.1'] <- anova.P$Df[1]
autoclave.results[autoclave.results$soil.characteristics == 'log.P',
                    'df.2'] <- anova.P$Df[2]
autoclave.results[autoclave.results$soil.characteristics == 'log.P',
                    'F.stat'] <- anova.P$`F value`[1]
autoclave.results[autoclave.results$soil.characteristics == 'log.P',
                    'p'] <- anova.P$`Pr(>F)`[1]

# K
lm.K <- lm(K ~ autoclave.time, data = soil.data)
anova.K <- anova(lm.K)
autoclave.results[autoclave.results$soil.characteristics == 'K',
                    'df.1'] <- anova.K$Df[1]
autoclave.results[autoclave.results$soil.characteristics == 'K',
                    'df.2'] <- anova.K$Df[2]
autoclave.results[autoclave.results$soil.characteristics == 'K',
                    'F.stat'] <- anova.K$`F value`[1]
autoclave.results[autoclave.results$soil.characteristics == 'K',
                    'p'] <- anova.K$`Pr(>F)`[1]

# Mg
lm.Mg <- lm(Mg ~ autoclave.time, data = soil.data)
anova.Mg <- anova(lm.Mg)
autoclave.results[autoclave.results$soil.characteristics == 'Mg',
                    'df.1'] <- anova.Mg$Df[1]
autoclave.results[autoclave.results$soil.characteristics == 'Mg',
                    'df.2'] <- anova.Mg$Df[2]
autoclave.results[autoclave.results$soil.characteristics == 'Mg',
                    'F.stat'] <- anova.Mg$`F value`[1]
autoclave.results[autoclave.results$soil.characteristics == 'Mg',
                    'p'] <- anova.Mg$`Pr(>F)`[1]

# log.S
lm.S <- lm(log.S ~ autoclave.time, data = soil.data)
anova.S <- anova(lm.S)
autoclave.results[autoclave.results$soil.characteristics == 'log.S',
                    'df.1'] <- anova.S$Df[1]
autoclave.results[autoclave.results$soil.characteristics == 'log.S',
                    'df.2'] <- anova.S$Df[2]
autoclave.results[autoclave.results$soil.characteristics == 'log.S',
                    'F.stat'] <- anova.S$`F value`[1]
autoclave.results[autoclave.results$soil.characteristics == 'log.S',
                    'p'] <- anova.S$`Pr(>F)`[1]

# Na
lm.Na <- lm(Na ~ autoclave.time, data = soil.data)
anova.Na <- anova(lm.Na)
autoclave.results[autoclave.results$soil.characteristics == 'Na',
                    'df.1'] <- anova.Na$Df[1]
autoclave.results[autoclave.results$soil.characteristics == 'Na',
                    'df.2'] <- anova.Na$Df[2]
autoclave.results[autoclave.results$soil.characteristics == 'Na',
                    'F.stat'] <- anova.Na$`F value`[1]
autoclave.results[autoclave.results$soil.characteristics == 'Na',
                    'p'] <- anova.Na$`Pr(>F)`[1]

# Ca
lm.Ca <- lm(Ca ~ autoclave.time, data = soil.data)
anova.Ca <- anova(lm.Ca)
autoclave.results[autoclave.results$soil.characteristics == 'Ca',
                    'df.1'] <- anova.Ca$Df[1]
autoclave.results[autoclave.results$soil.characteristics == 'Ca',
                    'df.2'] <- anova.Ca$Df[2]
autoclave.results[autoclave.results$soil.characteristics == 'Ca',
                    'F.stat'] <- anova.Ca$`F value`[1]
autoclave.results[autoclave.results$soil.characteristics == 'Ca',
                    'p'] <- anova.Ca$`Pr(>F)`[1]

write.csv(autoclave.results, 'results/Autoclave_results.csv', row.names = F)


# --------------------------------------------------------------------------#
#--Soil characteristics as a function of autoclave time: Graphs---
#-- Log.EC, Log.P, Log.S, Na
# --------------------------------------------------------------------------#
soil.data$autoclave.time <- factor(soil.data$autoclave.time)

##****Check outliers

# Log EC
graph.logEC <- ggplot(soil.data, aes(x = autoclave.time,
                                     y = log.EC)) +
  geom_boxplot() +
  ylab('Log EC (umhos/cm)') +
  xlab('Autoclave length (min)') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black"))
  
ggsave('figures/logEC_AutoclaveTime.tiff', device = 'tiff', plot = graph.logEC,
       width = 6, height = 6, units = 'cm', dpi = 300)

# Log P
graph.logP <- ggplot(soil.data, aes(x = autoclave.time,
                                     y = log.P)) +
  geom_boxplot() +
  ylab('Log phosphorus (ppm)') +
  xlab('Autoclave length (min)') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black"))
  
ggsave('figures/logP_AutoclaveTime.tiff', device = 'tiff', plot = graph.logP,
       width = 6, height = 6, units = 'cm', dpi = 300)

# Log S
graph.logS <- ggplot(soil.data, aes(x = autoclave.time,
                                     y = log.S)) +
  geom_boxplot() +
  ylab('Log sulfur (ppm)') +
  xlab('Autoclave length (min)') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black"))
  
ggsave('figures/logS_AutoclaveTime.tiff', device = 'tiff', plot = graph.logS,
       width = 6, height = 6, units = 'cm', dpi = 300)

# Na
graph.Na <- ggplot(soil.data, aes(x = autoclave.time,
                                     y = Na)) +
  geom_boxplot() +
  ylab('Sodium (ppm)') +
  xlab('Autoclave length (min)') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black"))
  
ggsave('figures/Na_AutoclaveTime.tiff', device = 'tiff', plot = graph.Na,
       width = 6, height = 6, units = 'cm', dpi = 300)

