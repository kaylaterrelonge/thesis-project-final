# Primary EDA (Pitching) ----

# load pkgs ----
library(tidyverse)

# read in data ----
pitching_data <- read_rds(file = 'data/clean_data/clean_pitch.rds')

### some initial work
player_totals <- pitching_data %>% 
  # sports betting legalized in after 2018 
  filter(season >= 2017) %>% 
  group_by(player) %>% 
  summarise(count = n()) %>% 
  filter(count >= 3)

player_in_range <- left_join(player_totals, pitching_data) %>% 
  filter(season >= 2017)

# correcting a naming error 
player_h <- player_in_range %>% 
  filter(player == " H") %>% 
  mutate(player = "Hyun Jin Ryu")

player_in_range <- player_in_range %>% 
  filter(player != " H")

player_in_range <- full_join(player_in_range, player_h)

# some visualization
plot_1_dat <- player_in_range %>% 
  group_by(season) %>% 
  summarise(runs_per_season = mean(r)) %>% 
  ungroup()

ggplot(plot_1_dat, mapping = aes(y = runs_per_season, x = season)) +
  geom_point()

plot_2 <- pitching_data %>% 
  filter(season >= 2008)  %>% 
  group_by(season) %>% 
  summarise(so_per_season = mean(so)) %>% 
  ungroup()


ggplot(plot_2, aes(x = season, y = so_per_season)) +
  geom_point() 

plot_3 <- pitching_data %>%
  filter(season >= 2008) 

ggplot(plot_3, aes(x = factor(season), y = avg)) +
  geom_boxplot()


ggplot(player_in_range, mapping = aes(y = era, x = factor(season))) +
  geom_boxplot()

ggplot(plot_3, mapping = aes(y = whip, x = factor(season))) +
  geom_boxplot()

plot_5_dat <- pitching_data %>%
  filter(season >= 2016) %>% 
  group_by(season) %>% 
  summarise(avg_wins = mean(w))

ggplot(plot_5_dat, mapping = aes(y = avg_wins, x = season)) +
  geom_point()

sum_stats <- player_in_range %>%  
  group_by(season) %>% 
  summarise(mean_era = mean(era, na.rm = TRUE),
            lower_era = quantile(era, 0.025, na.rm = TRUE),
            upper_era = quantile(era, 0.975, na.rm = TRUE),
            mean_avg = mean(avg, na.rm = TRUE),
            lower_avg = quantile(avg, 0.025, na.rm = TRUE),
            upper_avg = quantile(avg, 0.975, na.rm = TRUE),
            mean_whip = mean(whip, na.rm = TRUE),
            lower_whip = quantile(whip, 0.025, na.rm = TRUE),
            upper_whip = quantile(whip, 0.975, na.rm = TRUE),
            mean_so = mean(so, na.rm = TRUE),
            lower_so = quantile(so, 0.025, na.rm = TRUE),
            upper_so = quantile(so, 0.975, na.rm = TRUE))

ggplot(sum_stats, aes(x = season, y = mean_era)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_era, ymax = upper_era), width = 0.2) +
  labs(title = "ERA with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean ERA")

ggsave(filename = "figs_tables/pitching_mean_era.png")

ggplot(sum_stats, aes(x = season, y = mean_avg)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_avg, ymax = upper_avg), width = 0.2) +
  labs(title = "AVG with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean AVG")

ggsave(filename = "figs_tables/pitching_mean_avg.png")
# look for global metrics for an entire team for ex
sum_stats_tm <- player_in_range %>%  
  group_by(team, season) %>% 
  summarise(mean_era = mean(era, na.rm = TRUE),
            lower_era = quantile(era, 0.025, na.rm = TRUE),
            upper_era = quantile(era, 0.975, na.rm = TRUE),
            mean_avg = mean(avg, na.rm = TRUE),
            lower_avg = quantile(avg, 0.025, na.rm = TRUE),
            upper_avg = quantile(avg, 0.975, na.rm = TRUE),
            mean_whip = mean(whip, na.rm = TRUE),
            lower_whip = quantile(whip, 0.025, na.rm = TRUE),
            upper_whip = quantile(whip, 0.975, na.rm = TRUE),
            mean_so = mean(so, na.rm = TRUE),
            lower_so = quantile(so, 0.025, na.rm = TRUE),
            upper_so = quantile(so, 0.975, na.rm = TRUE))

ggplot(sum_stats_tm, aes(x = season, y = mean_whip)) +
  geom_errorbar(aes(ymin = lower_era, ymax = upper_era), width = 0.2) +
  facet_wrap(~team) +
  labs(title = "Era with 97.5% and 2.5% Error Bars By Team", x = "Season", y = "Mean ERA") 

