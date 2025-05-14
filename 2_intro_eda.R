# Introductory EDA (Data Inspection and Quality Check) ----

## load pkgs ----
library(tidyverse)
library(skimr)

## read in data ----
hitting_data <- read_rds(file = 'data/clean_data/clean_hit.rds') %>% 
  # using ungroup to fix a structural era
  ungroup()

pitching_data <- read_rds(file = 'data/clean_data/clean_pitch.rds') %>% 
  # using ungroup to fix a structural era
  ungroup()

## quality check ----
# changing variable type rank to an ordered factor and season to a factor
hitting_data <- hitting_data %>% 
  mutate(rank = factor(unlist(rank), ordered = TRUE))

pitching_data <- pitching_data %>% 
  mutate(rank = factor(unlist(rank), ordered = TRUE))

# checking for missingness
skim_without_charts(hitting_data)

skim_without_charts(pitching_data)

# no issues with missingness or extreme values

### distribution check ----
# examining the distribution of variables at the focus of thesis 

# offensive distributions
ggplot(hitting_data, aes(x = as.factor(season), y = avg)) +
  geom_boxplot()

ggplot(hitting_data, aes(x = as.factor(season), y = rbi)) +
  geom_boxplot() 

ggplot(hitting_data, aes(x = as.factor(season), y = ops)) +
  geom_boxplot() 

ggplot(pitching_data, aes(x = as.factor(season), y = era)) +
  geom_boxplot()

ggplot(pitching_data, aes(x = as.factor(season), y = whip)) +
  geom_boxplot()

ggplot(pitching_data, aes(x = as.factor(season), y = avg)) +
  geom_boxplot()

# distribution seems okay












