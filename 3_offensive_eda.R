# Offensive EDA ----

## no random processes in script

### load pkgs ----
library(tidyverse)
library(skimr)

### load data ----
hitting_data <- read_rds(file = 'data/clean_data/clean_hit.rds') %>% 
  # using ungroup to fix a structural era
  ungroup()

### data subsetting ----
# retaining information for players who appeared both pre- and post-legalization
player_totals <- hitting_data %>% 
  # sports betting legalized in after 2018 
  filter(season >= '2017') %>% 
  group_by(player) %>% 
  summarise(count = n()) %>%  
  # subsetting for players who played at least 3 seasons post-legalization
  filter(count >= 3)

player_in_range <- left_join(player_totals, hitting_data) %>% 
  filter(season >= 2017)

### summary stats ----
sum_stats <- hitting_data %>% 
  filter(season >= 2017) %>% 
  # will be looking at these metrics on a larger scale and team-by-team
  group_by(team, season) %>% 
  summarise(mean_rbi = mean(rbi, na.rm = TRUE),
            lower_rbi = quantile(rbi, 0.025, na.rm = TRUE),
            upper_rbi = quantile(rbi, 0.975, na.rm = TRUE),
            mean_avg = mean(avg, na.rm = TRUE),
            lower_avg = quantile(avg, 0.025, na.rm = TRUE),
            upper_avg = quantile(avg, 0.975, na.rm = TRUE),
            mean_ops = mean(ops, na.rm = TRUE),
            lower_ops = quantile(ops, 0.025, na.rm = TRUE),
            upper_ops = quantile(ops, 0.975, na.rm = TRUE))

### initial figures ----
ggplot(sum_stats, aes(x = season, y = mean_rbi)) +
  geom_errorbar(aes(ymin = lower_rbi, ymax = upper_rbi), width = 0.2) +
  labs(title = "Average RBI by Season with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean RBI")


ggplot(sum_stats, aes(x = season, y = mean_avg)) +
  geom_errorbar(aes(ymin = lower_avg, ymax = upper_avg), width = 0.2) +
  labs(title = "Average AVG by Season with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean AVG")


ggplot(sum_stats, aes(x = season, y = mean_ops)) +
  geom_errorbar(aes(ymin = lower_ops, ymax = upper_ops), width = 0.2) +
  labs(title = "Average OPS by Season with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean OPS")

### team specific trends ----
ggplot(sum_stats, aes(x = season, y = mean_ops)) +
  geom_errorbar(aes(ymin = lower_ops, ymax = upper_ops), width = 0.2) +
  facet_wrap(~team) +
  labs(title = "OPS with 97.5% and 2.5% Error Bars By Team", x = "Season", y = "Mean OPS") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(sum_stats, aes(x = season, y = mean_avg)) +
  geom_errorbar(aes(ymin = lower_avg, ymax = upper_avg), width = 0.2) +
  facet_wrap(~team) +
  labs(title = "AVG with 97.5% and 2.5% Error Bars By Team", x = "Season", y = "Mean AVG") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(sum_stats, aes(x = season, y = mean_rbi)) +
  geom_errorbar(aes(ymin = lower_rbi, ymax = upper_rbi), width = 0.2) +
  facet_wrap(~team) +
  labs(title = "RBI with 97.5% and 2.5% Error Bars By Team", x = "Season", y = "Mean RBI") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

### players with experience pre- and post-legalization ----
sum_stats_range <- player_in_range %>% 
  # will be looking at these metrics on a larger scale and team-by-team
  group_by(team, season) %>% 
  summarise(mean_rbi = mean(rbi, na.rm = TRUE),
            lower_rbi = quantile(rbi, 0.025, na.rm = TRUE),
            upper_rbi = quantile(rbi, 0.975, na.rm = TRUE),
            mean_avg = mean(avg, na.rm = TRUE),
            lower_avg = quantile(avg, 0.025, na.rm = TRUE),
            upper_avg = quantile(avg, 0.975, na.rm = TRUE),
            mean_ops = mean(ops, na.rm = TRUE),
            lower_ops = quantile(ops, 0.025, na.rm = TRUE),
            upper_ops = quantile(ops, 0.975, na.rm = TRUE))

ggplot(sum_stats_range, aes(x = season, y = mean_rbi)) +
  geom_errorbar(aes(ymin = lower_rbi, ymax = upper_rbi), width = 0.2) +
  labs(title = "Average RBI by Season with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean RBI")


ggplot(sum_stats_range, aes(x = season, y = mean_avg)) +
  geom_errorbar(aes(ymin = lower_avg, ymax = upper_avg), width = 0.2) +
  labs(title = "Average AVG by Season with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean AVG")


ggplot(sum_stats_range, aes(x = season, y = mean_ops)) +
  geom_errorbar(aes(ymin = lower_ops, ymax = upper_ops), width = 0.2) +
  labs(title = "Average OPS by Season with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean OPS")

### comparing pre and post legalization avg performance ----
pre_legal_off_avgs <- hitting_data %>% 
  ungroup() %>% 
  filter(season < 2018) %>% 
  select(avg, rbi, ops) %>% 
  summarize(
    mean_rbi = mean(rbi, na.rm = TRUE),
    mean_avg = mean(avg, na.rm = TRUE),
    mean_ops = mean(ops, na.rm = TRUE)
  )

post_legal_off_avgs <- hitting_data %>% 
  ungroup() %>% 
  filter(season >= 2018) %>% 
  select(avg, rbi, ops) %>% 
  summarize(
    mean_rbi = mean(rbi, na.rm = TRUE),
    mean_avg = mean(avg, na.rm = TRUE),
    mean_ops = mean(ops, na.rm = TRUE)
  )
