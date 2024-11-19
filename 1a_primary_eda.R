# Primary EDA ----

# load pkgs ----
library(tidyverse)

# read in data ----
hitting_data <- read_rds(file = 'data/clean_data/clean_hit.rds')

### some initial work
player_totals <- hitting_data %>% 
  # sports betting legalized in after 2018 
  filter(season >= 2017) %>% 
  group_by(player) %>% 
  summarise(count = n()) %>% 
  filter(count >= 3)

player_in_range <- left_join(player_totals, hitting_data) %>% 
  filter(season >= 2017)

# some visualization
plot_1_dat <- player_in_range %>% 
  group_by(season) %>% 
  summarise(avg_per_season = mean(avg)) %>% 
  ungroup()

plot_2 <- hitting_data %>% 
  filter(season >= 2008)

ggplot(plot_2, aes(x = season, y = rbi)) +
  geom_boxplot() 

ggplot(player_in_range, aes(y = season, x = rbi, color = season)) +
  geom_point()

plot_3 <- hitting_data %>%
  group_by(season) %>% 
  summarise(mean_rbi = mean(rbi), mean_avg = mean(avg)) %>% 
  filter(season >= 2008)

ggplot(plot_3, aes(x = season, y = mean_rbi/mean_avg)) +
  geom_point()

# normalize with the number of games 
# do a two sample t-test
t.test(hitting_data$rbi, hitting_data$avg)
# check the metrics for home runs
# research other metrics to help support performance
# for every plot we could do summary to get the summary stats
  # summary for rbi and avg, then do a line plot and use error bars to 97.5% & 2.5%
sum_stats <- hitting_data %>% 
  filter(season >= 2017) %>% 
  group_by(season) %>% 
  summarise(mean_rbi = mean(rbi, na.rm = TRUE),
            lower_rbi = quantile(rbi, 0.025, na.rm = TRUE),
            upper_rbi = quantile(rbi, 0.975, na.rm = TRUE),
            mean_avg = mean(avg, na.rm = TRUE),
            lower_avg = quantile(avg, 0.025, na.rm = TRUE),
            upper_avg = quantile(avg, 0.975, na.rm = TRUE))
ggplot(sum_stats, aes(x = season, y = mean_rbi)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower_rbi, ymax = upper_rbi), width = 0.2) +
  labs(title = "RBI with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean RBI")

ggplot(sum_stats, aes(x = season, y = mean_avg)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower_avg, ymax = upper_avg), width = 0.2) +
  labs(title = "AVG with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean AVG")

# looking at ops as well
sum_stats_tm <- hitting_data %>% 
  filter(season >= 2017) %>% 
  group_by(team, season) %>% 
  summarise(mean_ops = mean(ops, na.rm = TRUE),
            lower_ops = quantile(ops, 0.025, na.rm = TRUE),
            upper_ops = quantile(ops, 0.975, na.rm = TRUE))
# look for global metrics for an entire team for ex
ggplot(sum_stats_tm, aes(x = season, y = mean_ops)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower_ops, ymax = upper_ops), width = 0.2) +
  facet_wrap(~team) +
  labs(title = "OPS with 97.5% and 2.5% Error Bars By Team", x = "Season", y = "Mean OPS") 

# do t-test by team with rbi and avg and another for ops 
team_t_dat <- hitting_data %>% 
  group_by(team) %>% 
  select(rbi, team, season, avg, player, ops) %>% 
  filter(season >= "2017")

team_t_dat <- team_t_dat %>% 
  filter(team == "ATL")
# greater shows that it actually increased (unidirectional t-test)
t.test(team_t_dat$rbi, team_t_dat$avg, alternative = 'greater')

# take all metrics that i plan to use and send them to Prof. Banerjee (make a codebook)

