## Script to analyze soil nutrients and texture to test for differences
## based on invasion, pasture (geography), and autoclave time
## Written by Dr. Elizabeth Bowman Oct. 4, 2020

# install.packages('devtools')
library(devtools)
#install_github('vqv/ggbiplot')
library(tidyverse)
library(ggbiplot)

soil.data <- read.csv('data/data.autoclave.effect.csv', as.is = T)

# log transform non-normal data
soil.data %>%
  mutate(log.EC = log(Cond..umhos.cm.),
         log.P = log(P..ppm.),
         log.S = log(S..ppm.)) -> soil.data

#--Check for differences based on invasion
# Isolate unautoclaved data
soil.data[soil.data$autoclave.time == 0,] -> soil.invasion.data

# T-test
t.ph <- t.test(pH ~ invasion, data = soil.invasion.data) # p = 0.1
t.EC <- t.test(log.EC ~ invasion, data = soil.invasion.data) # p = 0.09
t.no3 <- t.test(NO3N..ppm. ~ invasion, data = soil.invasion.data) 
# p = 0.0002***** all data, just non-autoclaved = 0.16

t.p <- t.test(log.P ~ invasion, data = soil.invasion.data) # p = 0.51
t.K <- t.test(K..ppm. ~ invasion, data = soil.invasion.data) 
# p = 0.05****** all data, just non-autoclaved = 0.45

t.Ca <- t.test(Ca..ppm. ~ invasion, data = soil.invasion.data) # p = 0.09
t.Mg <- t.test(Mg..ppm. ~ invasion, data = soil.invasion.data) 
# p = 0.01****** all data, just non-autoclaved = 0.19

t.S <- t.test(log.S ~ invasion, data = soil.invasion.data) # p = 0.63
t.Na <- t.test(Na..ppm. ~ invasion, data = soil.invasion.data) # p = 0.96

# PCA
soil.pca <- prcomp(soil.invasion.data[c('pH','log.EC','NO3N..ppm.','log.P',
                               'K..ppm.','Ca..ppm.','Mg..ppm.',
                               'log.S','Na..ppm.')],
                   center = T, scale. = T)
summary(soil.pca)
ggbiplot(soil.pca, groups = soil.invasion.data$invasion) +
  theme_bw()
