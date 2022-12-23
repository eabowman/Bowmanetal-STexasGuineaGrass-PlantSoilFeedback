## Script to analyze soil nutrients and texture to test for differences
## based on invasion 
## Written by Dr. Elizabeth Bowman Dec. 8, 2020

## Anova results are not shown in manuscript as none were significant
## Plots of soild ata are shown in Supplementary fig. S2

# install.packages('devtools')
library(devtools)
# install_github('vqv/ggbiplot')
library(tidyverse)
library(ggbiplot)
# install.packages('ggpubr')
library(ggpubr)

soil.data <- read.csv('data/data.soil.csv', as.is = T)

# log transform non-normal data
soil.data %>%
  mutate(log.EC = log(EC),
         log.P = log(P),
         log.S = log(S)) -> soil.data

# --------------------------------------------------------------------------#
# Aanalysis of variance of soil characteristics as a function of invasion----
# --------------------------------------------------------------------------#
# isolate unautoclaved soil data
soil.data %>%
  filter(autoclave.time == 0)  -> soil.data

# Make results table
soil.results <- data.frame(soil.characteristics = 
                                  c('pH','log.EC','Nitrate','log.P','K','Mg',
                                    'log.S','Na','Ca'),
                                t = NA,
                                df = NA,
                                p = NA)


## pH ----
ttest.pH <- t.test(pH ~ invasion, data = soil.data)

soil.results[soil.results$soil.characteristics == 'pH',
                    't'] <- ttest.pH$statistic[1]
soil.results[soil.results$soil.characteristics == 'pH',
                    'df'] <- ttest.pH$parameter[1]
soil.results[soil.results$soil.characteristics == 'pH',
                    'p'] <- ttest.pH$p.value[1]

## log.EC ----
ttest.EC <- t.test(log.EC ~ invasion, data = soil.data)

soil.results[soil.results$soil.characteristics == 'log.EC',
                    't'] <- ttest.EC$statistic[1]
soil.results[soil.results$soil.characteristics == 'log.EC',
                    'df'] <- ttest.EC$parameter[1]
soil.results[soil.results$soil.characteristics == 'log.EC',
                    'p'] <- ttest.EC$p.value[1]

## Nitrate ----
ttest.N <- t.test(Nitrate ~ invasion, data = soil.data)

soil.results[soil.results$soil.characteristics == 'Nitrate',
                    't'] <- ttest.N$statistic[1]
soil.results[soil.results$soil.characteristics == 'Nitrate',
                    'df'] <- ttest.N$parameter[1]
soil.results[soil.results$soil.characteristics == 'Nitrate',
                    'p'] <- ttest.N$p.value[1]

## log.P ----
ttest.P <- t.test(log.P ~ invasion, data = soil.data)

soil.results[soil.results$soil.characteristics == 'log.P',
                    't'] <- ttest.P$statistic[1]
soil.results[soil.results$soil.characteristics == 'log.P',
                    'df'] <- ttest.P$parameter[1]
soil.results[soil.results$soil.characteristics == 'log.P',
                    'p'] <- ttest.P$p.value[1]

## K ----
ttest.K <- t.test(K ~ invasion, data = soil.data)

soil.results[soil.results$soil.characteristics == 'K',
                    't'] <- ttest.K$statistic[1]
soil.results[soil.results$soil.characteristics == 'K',
                    'df'] <- ttest.K$parameter[1]
soil.results[soil.results$soil.characteristics == 'K',
                    'p'] <- ttest.K$p.value[1]

## Mg ----
ttest.Mg <- t.test(Mg ~ invasion, data = soil.data)

soil.results[soil.results$soil.characteristics == 'Mg',
                    't'] <- ttest.Mg$statistic[1]
soil.results[soil.results$soil.characteristics == 'Mg',
                    'df'] <- ttest.Mg$parameter[1]
soil.results[soil.results$soil.characteristics == 'Mg',
                    'p'] <- ttest.Mg$p.value[1]

## log.S ----
ttest.S <- t.test(log.S ~ invasion, data = soil.data)

soil.results[soil.results$soil.characteristics == 'log.S',
                    't'] <- ttest.S$statistic[1]
soil.results[soil.results$soil.characteristics == 'log.S',
                    'df'] <- ttest.S$parameter[1]
soil.results[soil.results$soil.characteristics == 'log.S',
                    'p'] <- ttest.S$p.value[1]

## Na ----
ttest.Na <- t.test(Na ~ invasion, data = soil.data)

soil.results[soil.results$soil.characteristics == 'Na',
                    't'] <- ttest.Na$statistic[1]
soil.results[soil.results$soil.characteristics == 'Na',
                    'df'] <- ttest.Na$parameter[1]
soil.results[soil.results$soil.characteristics == 'Na',
                    'p'] <- ttest.Na$p.value[1]


## Ca ----
ttest.Ca <- t.test(Ca ~ invasion, data = soil.data)

soil.results[soil.results$soil.characteristics == 'Ca',
                    't'] <- ttest.Ca$statistic[1]
soil.results[soil.results$soil.characteristics == 'Ca',
                    'df'] <- ttest.Ca$parameter[1]
soil.results[soil.results$soil.characteristics == 'Ca',
                    'p'] <- ttest.Ca$p.value[1]

write.csv(soil.results, 'results/Soil_results.csv', row.names = F)

soil.data %>% 
  group_by(invasion) %>%
  summarise(across(c(pH, EC, Nitrate, P, K, Mg, S, Na, Ca),
                   c(mean,sd))) -> soil.summary

# --------------------------------------------------------------------#
# Plots ----
# --------------------------------------------------------------------#

pH_plot <- ggplot(soil.data, aes(x = invasion,
                      y = pH)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size = 16, color = 'black'),
        axis.title.y = element_text(size = 20, color = 'black'))

EC_plot <- ggplot(soil.data, aes(x = invasion,
                      y = EC)) +
  geom_boxplot() +
  theme_classic() +
  xlab('EC (umhos/cm)') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size = 16, color = 'black'),
        axis.title.y = element_text(size = 20, color = 'black'))

Nitrate_plot <- ggplot(soil.data, aes(x = invasion,
                      y = Nitrate)) +
  geom_boxplot() +
  theme_classic() +
  xlab('Nitrate (ppm)') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size = 16, color = 'black'),
        axis.title.y = element_text(size = 20, color = 'black'))

P_plot <- ggplot(soil.data, aes(x = invasion,
                      y = P)) +
  geom_boxplot() +
  theme_classic() +
  xlab('P (ppm)') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size = 16, color = 'black'),
        axis.title.y = element_text(size = 20, color = 'black'))

K_plot <- ggplot(soil.data, aes(x = invasion,
                      y = K)) +
  geom_boxplot() +
  theme_classic() +
  xlab('K (ppm)') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size = 16, color = 'black'),
        axis.title.y = element_text(size = 20, color = 'black'))

Mg_plot <- ggplot(soil.data, aes(x = invasion,
                      y = Mg)) +
  geom_boxplot() +
  theme_classic() +
  xlab('Mg (ppm)') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size = 16, color = 'black'),
        axis.title.y = element_text(size = 20, color = 'black'))

S_plot <- ggplot(soil.data, aes(x = invasion,
                      y = S)) +
  geom_boxplot() +
  theme_classic() +
  xlab('S (ppm)') +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20, color = 'black'),
        axis.text.y = element_text(size = 16, color = 'black'),
        axis.title.y = element_text(size = 20, color = 'black'))

Na_plot <- ggplot(soil.data, aes(x = invasion,
                      y = Na)) +
  geom_boxplot() +
  theme_classic() +
  xlab('Na (ppm)') +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20, color = 'black'),
        axis.text.y = element_text(size = 16, color = 'black'),
        axis.title.y = element_text(size = 20, color = 'black'))

Ca_plot <- ggplot(soil.data, aes(x = invasion,
                      y = Ca)) +
  geom_boxplot() +
  theme_classic() +
  xlab('Ca (ppm)') +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20, color = 'black'),
        axis.text.y = element_text(size = 16, color = 'black'),
        axis.title.y = element_text(size = 20, color = 'black'))

soil_plots <- ggarrange(pH_plot, EC_plot, Nitrate_plot, P_plot, K_plot, Mg_plot,
                        S_plot, Na_plot, Ca_plot,
                        labels = c('a','b','c','d','e','f','g','h','i'),
                        ncol = 3, nrow = 3)
ggsave('figures/SupplementaryFigS2.tiff', device = 'tiff',
       plot = soil_plots,
       width = 30, height = 20, units = 'cm',
       dpi = 300)
# --------------------------------------------------------------------#
# Summary data ----
# --------------------------------------------------------------------#

soil.data %>%
  group_by(invasion) %>%
  select(pH, EC, Nitrate, P, K, Mg, S, Na, Ca) %>%
  summarize_all(mean) -> soil.summary

write.csv(soil.summary, 'results/soil_summary.csv', row.names = T)
