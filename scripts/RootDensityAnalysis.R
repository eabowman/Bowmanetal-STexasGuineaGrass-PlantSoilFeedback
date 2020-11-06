## Script to assess differences in root density between invaded and uninvaded
## grasslands. 100 grams of soil + roots was weighed out for each samples.
## Roots were sieved from the soil using a 1.7 mm sieve and dried at 60°C for
## 48 hours before weighing.
## Written by Dr. Elizabeth A. Bowman, elizabeth.bowman@austin.utexas.edu
## September 5, 2020

library(tidyverse)
library(nlme)

root.data <- read.csv('data/data.root.comparison.csv', as.is = T)

#-- Averages by invasion
root.data %>%
  group_by(invasion, pasture) %>%
  summarise(mean(weight), sd(weight))

#-- Check normality of root data -----
histogram(root.data$weight)
shapiro.test(root.data$weight)

# Log transform
root.data$log.weight <- log(root.data$weight)
histogram(root.data$log.weight)
shapiro.test(root.data$log.weight)

# remove outlier in invaded data (0.031g)
root.data.out <- root.data[!root.data$weight == '0.031',]

#-- ANOVA to assess diferences based on location and invasion--------
lm.roots <- lm(log.weight ~ invasion * pasture, data = root.data.out)
summary(lm.roots)
anova (lm.roots)

# Set pasture as a random variable
lme.roots <- lme(log.weight ~ invasion, 
                 data = root.data.out,
                 random = ~ 1 | pasture,
                 method = 'REML')
summary(lme.roots)

#-- plot -----------
ggplot(root.data.out, aes(x = invasion,
                      y = weight)) +
  geom_boxplot() +
  facet_grid(. ~ pasture) +
  theme_classic() +
  ylab('Weight (g)') +
  xlab('')

