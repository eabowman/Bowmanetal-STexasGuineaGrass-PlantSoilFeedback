## Script to assess differences in the seedlings emerging from the
## seed bank bases on soil treatment (individual soil samples, ISS, and 
## multiple soil samples, MSS), invasion, and pasture (geographic location).
## Written by Dr. Elizabeth Bowman on Sept. 29, 2020

library(tidyverse)
library(nlme)

seedling.data <- read.csv('data/data.MSS.ISS.csv', as.is = T)

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

# ANOVA
lm.seedling <- lm(log.seedling.total ~ invasion * pasture,
                  data = seedling.data)
anova.seedling <- anova(lm.seedling)
anova.seedling

# Plot
ggplot(seedling.data, aes(x = soil.treatment,
                          y = log.seedling.total)) +
  geom_boxplot() +
  facet_grid( . ~ invasion) +
  theme_classic()

ggplot(seedling.data, aes(x = invasion,
                          y = log.seedling.total)) +
  geom_boxplot() +
  facet_grid(. ~ pasture + soil.treatment) +
  theme_classic()

ggsave('figures/SeedlingGermination_Overall.pdf', device = 'pdf')

ggplot(seedling.data, aes(x = invasion,
                          y = log.seedling.total)) +
  geom_boxplot() +
  #facet_grid(. ~ pasture + soil.treatment) +
  theme_classic()

ggsave('figures/SeedlingGermination_InvasionOnly.pdf', device = 'pdf')

#--Guinea grass germination-----
#--Time point 1----
hist(seedling.data$guinea.grass.seedling.9.28.20)
seedling.data$log.guinea.grass.tp1 <- log(seedling.data$guinea.grass.seedling.9.28.20)
hist(seedling.data$log.guinea.grass.tp1)

# Remove 0s 
seedling.data.out <- seedling.data[!seedling.data$guinea.grass.seedling.9.28.20 == 0,]

# Mean and sd
seedling.data.out %>%
  group_by(invasion) %>%
  summarise(mean = mean(guinea.grass.seedling.9.28.20),
            sd = sd(guinea.grass.seedling.9.28.20))

# ANOVA
lm.guinea <- lm(log.guinea.grass.tp1 ~ invasion * soil.treatment,
                  data = seedling.data.out)
anova.guinea.tp1 <- anova(lm.guinea)
anova.guinea.tp1

# Plot
ggplot(seedling.data, aes(x = soil.treatment,
                          y = log.guinea.grass.tp1)) +
  geom_boxplot() +
  facet_grid( . ~ invasion) +
  theme_classic()

ggplot(seedling.data, aes(x = soil.treatment,
                          y = log.guinea.grass.tp1)) +
  geom_boxplot() +
  facet_grid(. ~  invasion) +
  theme_classic()

#ggsave('figures/GuineaGerminationTP1_Overall.pdf', device = 'pdf')

ggplot(seedling.data.out, aes(x = invasion,
                          y = log.guinea.grass.tp1)) +
  geom_boxplot() +
  #facet_grid(. ~ pasture + soil.treatment) +
  theme_classic()

#ggsave('figures/GuineaGerminationTP1_InvasionOnly.pdf', device = 'pdf')

#--Time point 2----
seedling.data %>%
  mutate(guinea.grass.seedling.total =
  guinea.grass.seedling.9.28.20 + guinea.grass.seedling.10.12.20) -> seedling.data

# germinated new
hist(seedling.data$guinea.grass.seedling.10.12.20)
seedling.data.out <- seedling.data[!seedling.data$guinea.grass.seedling.10.12.20 == 0,]
seedling.data.out$log.guinea.grass.tp2 <- log(seedling.data.out$guinea.grass.seedling.10.12.20)
hist(seedling.data.out$log.guinea.grass.tp2)

# total
hist(seedling.data$guinea.grass.seedling.total)
seedling.data.out.total <- 
  seedling.data[!seedling.data$guinea.grass.seedling.total == 0,]
seedling.data.out.total$log.guinea.grass.total <- 
  log(seedling.data.out.total$guinea.grass.seedling.total)
hist(seedling.data.out.total$log.guinea.grass.total)

# ANOVA
lm.guinea <- lm(log.guinea.grass.total ~ invasion * soil.treatment,
                  data = seedling.data.out.total)
anova.guinea.total <- anova(lm.guinea)
anova.guinea.total

# Plot
ggplot(seedling.data.out.total,
       aes(x = soil.treatment,
           y = log.guinea.grass.total)) +
  geom_boxplot() +
  facet_grid( . ~ invasion) +
  theme_classic()

ggplot(seedling.data.out.total,
       aes(x = soil.treatment,
           y = log.guinea.grass.total)) +
  geom_boxplot() +
  facet_grid(. ~ invasion) +
  theme_classic()

ggsave('figures/GuineaGerminationTotal_Overall.pdf', device = 'pdf')

ggplot(seedling.data.out.total,
       aes(x = invasion,
           y = log.guinea.grass.total)) +
  geom_boxplot() +
  #facet_grid(. ~ pasture + soil.treatment) +
  theme_classic()
ggsave('figures/GuineaGerminationTotal_InvasionOnly.pdf', device = 'pdf')

#--Correlation of native to Guinea-----

lm.guinea <- lm(log.seedling.total ~ log.guinea.grass.total,
                  data = seedling.data.out.total)
summary(lm.guinea)

ggplot(seedling.data.out.total,
       aes(x = log.guinea.grass.total,
           y = log.seedling.total)) +
  geom_point() +
  geom_smooth(method = lm,
              se = F) +
  theme_classic()
  