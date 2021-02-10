## Script to assess differences in the emergence of Guinea grass eon soil treatment (individual soil samples, ISS, and 
## multiple soil samples, MSS), invasion, and pasture (geographic location).
## Written by Dr. Elizabeth Bowman on Nov. 11, 2020

library(tidyverse)
library(nlme)
library(vegan)

plant.data <- read.csv('data/data.MSS.ISS.csv', as.is = T)

# --------------------------------------------------------------------------#
#--<< Data manipulation and evaluation of normality >> ----
# --------------------------------------------------------------------------#

#--Native seed bank-----
# create new column merging the 9.15 and 9.14 seedling data
plant.data$seedlings.9.14.20 <- replace_na(plant.data$seedlings.9.14.20, 0)
plant.data$seedlings.9.15.20 <- replace_na(plant.data$seedlings.9.15.20, 0)

plant.data %>%
  mutate(seedling.total = seedlings.9.14.20 + seedlings.9.15.20,
         log.seedling.total = log(seedling.total)) %>%
    select(-seedlings.9.8.20,-seedlings.9.10.20, # remove extra columns
           -seedlings.9.14.20,-seedlings.9.15.20) -> plant.data
# log evaluation based on assessment indicating data non-normal
# to see these analyses look at the R file 'NativeSeedBank.R'

#--Guinea grass seed germination----
plant.data %>%
  mutate(guinea.grass.seedling.total = guinea.grass.seedling.9.28.20 +
    guinea.grass.seedling.10.12.20 + guinea.grass.seedling.10.23.20,
    log.guinea.grass.seedling.total = log(guinea.grass.seedling.total)) %>%
  #filter(guinea.grass.seedling.total != 0) %>%
  select(-guinea.grass.seedling.9.28.20,-guinea.grass.seedling.10.12.20,
  -guinea.grass.seedling.10.23.20) -> plant.data.rev

# seedling germination as a function of time
plant.data.time <- plant.data[c('sample','pasture','invasion',
                                      'soil.treatment',
                                      'guinea.grass.seedling.9.28.20',
                                      'guinea.grass.seedling.10.12.20',
                                      'guinea.grass.seedling.10.23.20')]
plant.data.time %>%
  gather(key = time.pt, value = seedling.count,
         -sample, -pasture, -invasion, -soil.treatment) -> plant.data.time

#--Check normality of data
hist(plant.data.rev$guinea.grass.seedling.total)
hist(plant.data.rev$log.guinea.grass.seedling.total)
shapiro.test(plant.data.rev$log.guinea.grass.seedling.total)

# not normal, use non-parametric tests

#--Mean and sd
plant.data %>%
  filter(!is.na(guinea.grass.seedling.total)) %>%
  group_by(invasion) %>%
  summarise(mean.guineagerm = mean(guinea.grass.seedling.total),
            sd.guineagerm = sd(guinea.grass.seedling.total),
            mean.week1 = mean(guinea.grass.seedling.week.1),
            sd.week1 = sd(guinea.grass.seedling.week.1)) -> germ.summary

# write.csv(germ.summary, 'results/GerminationSummaryData.csv',
#           row.names = F)

#--Guinea grass height data and growth rate----
#--Transform to make the height columns into one keeping invasion
# and soil treatment columns
plant.data %>%
  select(sample,replicate,pasture,invasion, soil.treatment,
         guinea.grass.height.10.29.20.cm,
         guinea.grass.height.11.6.20.cm,
         guinea.grass.height.11.13.20.cm,
         guinea.grass.height.11.20.20.cm,
         guinea.grass.height.11.27.20.cm,
         guinea.grass.height.12.4.20.cm) %>%
  gather(key = week, value = height, -invasion, -soil.treatment, 
         -sample, -replicate, -pasture) %>%
  mutate(week = replace(week, week == 'guinea.grass.height.10.29.20.cm',
                        'week1'),
         week = replace(week, week == 'guinea.grass.height.11.6.20.cm',
                        'week2'),
         week = replace(week, week == 'guinea.grass.height.11.13.20.cm',
                        'week3'),
         week = replace(week, week == 'guinea.grass.height.11.20.20.cm',
                        'week4'),
         week = replace(week, week == 'guinea.grass.height.11.27.20.cm',
                        'week5'),
         week = replace(week, week == 'guinea.grass.height.12.4.20.cm',
                        'week6'),
         log.height = log(height)) %>%
  filter(height != 0, !is.na(height)) -> time.data

# Calculate growth rate in the seedling.data 
plant.data[is.na(plant.data)] <- 0
plant.data <- mutate(plant.data,
       growth.rate.overall = guinea.grass.height.12.4.20.cm/97,
       log.growth.rate.overall = log(growth.rate.overall),
       log.height.cm = log(guinea.grass.height.12.4.20.cm))

#--Check normality of data
hist(plant.data$guinea.grass.height.12.4.20.cm)
shapiro.test(plant.data$guinea.grass.height.12.4.20.cm) # not normal
hist(plant.data$log.height.cm)
shapiro.test(plant.data$log.height.cm)

# Data non-normal even after transformation, use non-parametric methods

hist(seedling.data$growth.rate.overall)
shapiro.test(seedling.data$growth.rate.overall)

# Data non-normal even after transformation, use non-parametric methods

# Summary of data
group_by(seedling.data, invasion) %>% 
  summarise(mean.growth.rate = mean(growth.rate.overall), 
            sd.growth.rate = sd(growth.rate.overall), 
            mean.height = mean(guinea.grass.height.12.4.20.cm), 
            sd.height = sd(guinea.grass.height.12.4.20.cm),
            week1.gr.mean = mean(guinea.grass.height.10.29.2.cm0),
            week1.gr.sd = sd(guinea.grass.height.10.29.20.cm),
            week2.gr.mean = mean(guinea.grass.height.11.6.20.cm),
            week2.gr.sd = sd(guinea.grass.height.11.6.20.cm),
            week3.gr.mean = mean(guinea.grass.height.11.13.20.cm),
            week3.gr.sd = sd(guinea.grass.height.11.13.20.cm),
            week4.gr.mean = mean(guinea.grass.height.11.20.20.cm),
            week4.gr.sd = sd(guinea.grass.height.11.20.20.cm),
            week5.gr.mean = mean(guinea.grass.height.11.27.20.cm),
            week5.gr.sd = sd(guinea.grass.height.11.27.20.cm)) -> height.summary

#--Guinea grass root length----
hist(seedling.data$guinea.grass.root.length)
shapiro.test(seedling.data$guinea.grass.root.length)

# Data normal
