## Script to analyze soil nutrients and texture to test for differences
## based on invasion and pasture (geography)
## Written by Dr. Elizabeth Bowman Dec. 8, 2020

# install.packages('devtools')
library(devtools)
#install_github('vqv/ggbiplot')
library(tidyverse)
library(ggbiplot)
#install.packages('ggpubr')
library(ggpubr)

# change number of decimal points format to 4
options(digits = 4)

soil.data <- read.csv('data/data.autoclave.effect.csv', as.is = T)

# log transform non-normal data
soil.data %>%
  mutate(log.EC = log(EC),
         log.P = log(P),
         log.S = log(S)) -> soil.data

# --------------------------------------------------------------------------#
#--Data manipulation and analysis----
# --------------------------------------------------------------------------#
# isolate unautoclaved soil data
soil.data %>%
  filter(autoclave.time == 0)  -> soil.data

# Make results table
soil.results <- data.frame(soil.characteristics = 
                                  c('pH','log.EC','Nitrate','log.P','K','Mg',
                                    'log.S','Na','Ca'),
                                df.1 = NA,
                                df.2 = NA,
                                F.stat = NA,
                                p = NA)


# pH
lm.pH <- lm(pH ~ invasion, data = soil.data)
anova.pH <- anova(lm.pH)
soil.results[soil.results$soil.characteristics == 'pH',
                    'df.1'] <- anova.pH$Df[1]
soil.results[soil.results$soil.characteristics == 'pH',
                    'df.2'] <- anova.pH$Df[2]
soil.results[soil.results$soil.characteristics == 'pH',
                    'F.stat'] <- anova.pH$`F value`[1]
soil.results[soil.results$soil.characteristics == 'pH',
                    'p'] <- anova.pH$`Pr(>F)`[1]

# log.EC
lm.EC <- lm(log.EC ~ invasion, data = soil.data)
anova.EC <- anova(lm.EC)
soil.results[soil.results$soil.characteristics == 'log.EC',
                    'df.1'] <- anova.EC$Df[1]
soil.results[soil.results$soil.characteristics == 'log.EC',
                    'df.2'] <- anova.EC$Df[2]
soil.results[soil.results$soil.characteristics == 'log.EC',
                    'F.stat'] <- anova.EC$`F value`[1]
soil.results[soil.results$soil.characteristics == 'log.EC',
                    'p'] <- anova.EC$`Pr(>F)`[1]

# Nitrate
lm.N <- lm(Nitrate ~ invasion, data = soil.data)
anova.N <- anova(lm.N)
soil.results[soil.results$soil.characteristics == 'Nitrate',
                    'df.1'] <- anova.N$Df[1]
soil.results[soil.results$soil.characteristics == 'Nitrate',
                    'df.2'] <- anova.N$Df[2]
soil.results[soil.results$soil.characteristics == 'Nitrate',
                    'F.stat'] <- anova.N$`F value`[1]
soil.results[soil.results$soil.characteristics == 'Nitrate',
                    'p'] <- anova.N$`Pr(>F)`[1]

# log.P
lm.P <- lm(log.P ~ invasion, data = soil.data)
anova.P <- anova(lm.P)
soil.results[soil.results$soil.characteristics == 'log.P',
                    'df.1'] <- anova.P$Df[1]
soil.results[soil.results$soil.characteristics == 'log.P',
                    'df.2'] <- anova.P$Df[2]
soil.results[soil.results$soil.characteristics == 'log.P',
                    'F.stat'] <- anova.P$`F value`[1]
soil.results[soil.results$soil.characteristics == 'log.P',
                    'p'] <- anova.P$`Pr(>F)`[1]

# K
lm.K <- lm(K ~ invasion, data = soil.data)
anova.K <- anova(lm.K)
soil.results[soil.results$soil.characteristics == 'K',
                    'df.1'] <- anova.K$Df[1]
soil.results[soil.results$soil.characteristics == 'K',
                    'df.2'] <- anova.K$Df[2]
soil.results[soil.results$soil.characteristics == 'K',
                    'F.stat'] <- anova.K$`F value`[1]
soil.results[soil.results$soil.characteristics == 'K',
                    'p'] <- anova.K$`Pr(>F)`[1]

# Mg
lm.Mg <- lm(Mg ~ invasion, data = soil.data)
anova.Mg <- anova(lm.Mg)
soil.results[soil.results$soil.characteristics == 'Mg',
                    'df.1'] <- anova.Mg$Df[1]
soil.results[soil.results$soil.characteristics == 'Mg',
                    'df.2'] <- anova.Mg$Df[2]
soil.results[soil.results$soil.characteristics == 'Mg',
                    'F.stat'] <- anova.Mg$`F value`[1]
soil.results[soil.results$soil.characteristics == 'Mg',
                    'p'] <- anova.Mg$`Pr(>F)`[1]

# log.S
lm.S <- lm(log.S ~ invasion, data = soil.data)
anova.S <- anova(lm.S)
soil.results[soil.results$soil.characteristics == 'log.S',
                    'df.1'] <- anova.S$Df[1]
soil.results[soil.results$soil.characteristics == 'log.S',
                    'df.2'] <- anova.S$Df[2]
soil.results[soil.results$soil.characteristics == 'log.S',
                    'F.stat'] <- anova.S$`F value`[1]
soil.results[soil.results$soil.characteristics == 'log.S',
                    'p'] <- anova.S$`Pr(>F)`[1]

# Na
lm.Na <- lm(Na ~ invasion, data = soil.data)
anova.Na <- anova(lm.Na)
soil.results[soil.results$soil.characteristics == 'Na',
                    'df.1'] <- anova.Na$Df[1]
soil.results[soil.results$soil.characteristics == 'Na',
                    'df.2'] <- anova.Na$Df[2]
soil.results[soil.results$soil.characteristics == 'Na',
                    'F.stat'] <- anova.Na$`F value`[1]
soil.results[soil.results$soil.characteristics == 'Na',
                    'p'] <- anova.Na$`Pr(>F)`[1]

# Ca
lm.Ca <- lm(Ca ~ invasion, data = soil.data)
anova.Ca <- anova(lm.Ca)
soil.results[soil.results$soil.characteristics == 'Ca',
                    'df.1'] <- anova.Ca$Df[1]
soil.results[soil.results$soil.characteristics == 'Ca',
                    'df.2'] <- anova.Ca$Df[2]
soil.results[soil.results$soil.characteristics == 'Ca',
                    'F.stat'] <- anova.Ca$`F value`[1]
soil.results[soil.results$soil.characteristics == 'Ca',
                    'p'] <- anova.Ca$`Pr(>F)`[1]

write.csv(soil.results, 'results/Soil_results.csv', row.names = F)

#<< Plot of soil characteristics >> -------
pH_plot <- ggplot(soil.data, aes(x = invasion,
                      y = pH)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

EC_plot <- ggplot(soil.data, aes(x = invasion,
                      y = EC)) +
  geom_boxplot() +
  theme_classic() +
  xlab('EC (umhos/cm)') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

Nitrate_plot <- ggplot(soil.data, aes(x = invasion,
                      y = Nitrate)) +
  geom_boxplot() +
  theme_classic() +
  xlab('Nitrate (ppm)') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

P_plot <- ggplot(soil.data, aes(x = invasion,
                      y = P)) +
  geom_boxplot() +
  theme_classic() +
  xlab('P (ppm)') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

K_plot <- ggplot(soil.data, aes(x = invasion,
                      y = K)) +
  geom_boxplot() +
  theme_classic() +
  xlab('K (ppm)') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

Mg_plot <- ggplot(soil.data, aes(x = invasion,
                      y = Mg)) +
  geom_boxplot() +
  theme_classic() +
  xlab('Mg (ppm)') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

S_plot <- ggplot(soil.data, aes(x = invasion,
                      y = S)) +
  geom_boxplot() +
  theme_classic() +
  xlab('S (ppm)') +
  theme(axis.title.x=element_blank())

Na_plot <- ggplot(soil.data, aes(x = invasion,
                      y = Na)) +
  geom_boxplot() +
  theme_classic() +
  xlab('Na (ppm)') +
  theme(axis.title.x=element_blank())

Ca_plot <- ggplot(soil.data, aes(x = invasion,
                      y = Ca)) +
  geom_boxplot() +
  theme_classic() +
  xlab('Ca (ppm)') +
  theme(axis.title.x=element_blank())

soil_plots <- ggarrange(pH_plot, EC_plot, Nitrate_plot, P_plot, K_plot, Mg_plot,
                        S_plot, Na_plot, Ca_plot,
                        labels = c('A','B','C','D','E','F','G','H','I'),
                        ncol = 3, nrow = 3)
ggsave('figures/SupplementaryFig_soil.tiff', device = 'tiff',
       plot = soil_plots, width = 15, height = 10, units = 'cm', dpi = 300)

soil.data %>%
  group_by(invasion) %>%
  select(pH, EC, Nitrate, P, K, Mg, S, Na, Ca) %>%
  summarize_all(mean) -> soil.summary
write.csv(soil.summary, 'results/soil_summary.csv', row.names = T)

# --------------------------------------------------------------------------#
#--Soil characteristics as a function of invasion: Graphs---
#-- Log.EC, Log.P, Log.S, Na
# --------------------------------------------------------------------------#
soil.data$autoclave.time <- factor(soil.data$autoclave.time)

##****Check outliers

# Log EC
graph.logEC <- ggplot(soil.data, aes(x = invasion,
                                     y = log.EC)) +
  geom_boxplot() +
  ylab('Log EC (umhos/cm)') +
  xlab('') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black"))
  
ggsave('figures/logEC_invasion.tiff', device = 'tiff', plot = graph.logEC,
       width = 6, height = 6, units = 'cm', dpi = 300)

# Log P
graph.logP <- ggplot(soil.data, aes(x = invasion,
                                     y = log.P)) +
  geom_boxplot() +
  ylab('Log phosphorus (ppm)') +
  xlab('') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black"))
  
ggsave('figures/logP_invasion.tiff', device = 'tiff', plot = graph.logP,
       width = 6, height = 6, units = 'cm', dpi = 300)

# Log S
graph.logS <- ggplot(soil.data, aes(x = invasion,
                                     y = log.S)) +
  geom_boxplot() +
  ylab('Log sulfur (ppm)') +
  xlab('') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black"))
  
ggsave('figures/logS_invasion.tiff', device = 'tiff', plot = graph.logS,
       width = 6, height = 6, units = 'cm', dpi = 300)

# Na
graph.Na <- ggplot(soil.data, aes(x = invasion,
                                     y = Na)) +
  geom_boxplot() +
  ylab('Sodium (ppm)') +
  xlab('') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black"))
  
ggsave('figures/Na_invasion.tiff', device = 'tiff', plot = graph.Na,
       width = 6, height = 6, units = 'cm', dpi = 300)

# Nitrate
graph.N <- ggplot(soil.data, aes(x = invasion,
                                     y = Nitrate)) +
  geom_boxplot() +
  ylab('Nitrate (ppm)') +
  xlab('') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black"))
  
ggsave('figures/Nitrate_invasion.tiff', device = 'tiff', plot = graph.N,
       width = 6, height = 6, units = 'cm', dpi = 300)

# Magnesium
graph.Mg <- ggplot(soil.data, aes(x = invasion,
                                     y = Mg)) +
  geom_boxplot() +
  ylab('Magnesium (ppm)') +
  xlab('') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black"))
  
ggsave('figures/Mg_invasion.tiff', device = 'tiff', plot = graph.Mg,
       width = 6, height = 6, units = 'cm', dpi = 300)

# pH
graph.ph <- ggplot(soil.data, aes(x = invasion,
                                     y = pH)) +
  geom_boxplot() +
  ylab('pH (ppm)') +
  xlab('') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(color = "black"))
  
ggsave('figures/ph_invasion.tiff', device = 'tiff', plot = graph.ph,
       width = 6, height = 6, units = 'cm', dpi = 300)
