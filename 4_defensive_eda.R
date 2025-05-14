# Defensive EDA ----

## no random processes in script

### load pkgs ----
library(tidyverse)
library(skimr)

### load data ----
pitching_data <- read_rds(file = 'data/clean_data/clean_pitch.rds') %>% 
  # using ungroup to fix a structural era
  ungroup()

### data subsetting ----
# retaining information for players who appeared both pre- and post-legalization
player_totals_pitch <- pitching_data %>% 
  # sports betting legalized in after 2018 
  filter(season >= 2017) %>% 
  group_by(player) %>% 
  summarise(count = n()) %>% 
  filter(count >= 3)

player_in_range_pitch <- left_join(player_totals_pitch, pitching_data) %>% 
  filter(season >= 2017)

# correcting a naming error 
player_h <- player_in_range %>% 
  filter(player == " H") %>% 
  mutate(player = "Hyun Jin Ryu")

player_in_range <- player_in_range %>% 
  filter(player != " H")

player_in_range <- full_join(player_in_range, player_h)

player_years_active_pitch <- pitching_data %>% 
  group_by(player) %>% 
  summarise(years_active = n())

player_in_range_pitch_tot <- full_join(player_in_range_pitch, player_years_active_pitch) %>% 
  filter(!is.na(season))

### summary stats ----
sum_stats_pitch <- pitching_data%>%  
  group_by(season) %>% 
  summarise(mean_era = mean(era, na.rm = TRUE),
            lower_era = quantile(era, 0.025, na.rm = TRUE),
            upper_era = quantile(era, 0.975, na.rm = TRUE),
            mean_whip = mean(whip, na.rm = TRUE),
            lower_whip = quantile(whip, 0.025, na.rm = TRUE),
            upper_whip = quantile(whip, 0.975, na.rm = TRUE),
            mean_avg = mean(era, na.rm = TRUE),
            lower_avg = quantile(era, 0.025, na.rm = TRUE),
            upper_avg = quantile(era, 0.975, na.rm = TRUE))

sum_stats_pitch_tm <- pitching_data%>%  
  group_by(team, season) %>% 
  summarise(mean_era = mean(era, na.rm = TRUE),
            lower_era = quantile(era, 0.025, na.rm = TRUE),
            upper_era = quantile(era, 0.975, na.rm = TRUE),
            mean_whip = mean(whip, na.rm = TRUE),
            lower_whip = quantile(whip, 0.025, na.rm = TRUE),
            upper_whip = quantile(whip, 0.975, na.rm = TRUE),
            mean_avg = mean(era, na.rm = TRUE),
            lower_avg = quantile(era, 0.025, na.rm = TRUE),
            upper_avg = quantile(era, 0.975, na.rm = TRUE))

### initial figures ----
ggplot(sum_stats_pitch, aes(x = season, y = mean_era)) +
  geom_errorbar(aes(ymin = lower_era, ymax = upper_era), width = 0.2) +
  labs(title = "Average ERA by Season with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean ERA")


ggplot(sum_stats_pitch, aes(x = season, y = mean_avg)) +
  geom_errorbar(aes(ymin = lower_avg, ymax = upper_avg), width = 0.2) +
  labs(title = "Average AAVG by Season with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean AAVG")


ggplot(sum_stats_pitch, aes(x = season, y = mean_whip)) +
  geom_errorbar(aes(ymin = lower_whip, ymax = upper_whip), width = 0.2) +
  labs(title = "Average WHIP by Season with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean WHIP")

### team specific trends ----
ggplot(sum_stats_pitch_tm, aes(x = season, y = mean_whip)) +
  geom_errorbar(aes(ymin = lower_whip, ymax = upper_whip), width = 0.2) +
  facet_wrap(~team) +
  labs(title = "WHIP with 97.5% and 2.5% Error Bars By Team", x = "Season", y = "Mean WHIP") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(sum_stats_pitch_tm, aes(x = season, y = mean_avg)) +
  geom_errorbar(aes(ymin = lower_avg, ymax = upper_avg), width = 0.2) +
  facet_wrap(~team) +
  labs(title = "AAVG with 97.5% and 2.5% Error Bars By Team", x = "Season", y = "Mean AAVG") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(sum_stats_pitch_tm, aes(x = season, y = mean_era)) +
  geom_errorbar(aes(ymin = lower_era, ymax = upper_era), width = 0.2) +
  facet_wrap(~team) +
  labs(title = "ERA with 97.5% and 2.5% Error Bars By Team", x = "Season", y = "Mean ERA") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

### players with pre- and post-legalization data ----
sum_stats_pitch_in_range <- player_in_range_pitch_tot %>%  
  group_by(season) %>% 
  summarise(mean_era = mean(era, na.rm = TRUE),
            lower_era = quantile(era, 0.025, na.rm = TRUE),
            upper_era = quantile(era, 0.975, na.rm = TRUE),
            mean_whip = mean(whip, na.rm = TRUE),
            lower_whip = quantile(whip, 0.025, na.rm = TRUE),
            upper_whip = quantile(whip, 0.975, na.rm = TRUE),
            mean_avg = mean(era, na.rm = TRUE),
            lower_avg = quantile(era, 0.025, na.rm = TRUE),
            upper_avg = quantile(era, 0.975, na.rm = TRUE))

ggplot(sum_stats_pitch_in_range, aes(x = season, y = mean_era)) +
  geom_errorbar(aes(ymin = lower_era, ymax = upper_era), width = 0.2) +
  labs(title = "Average ERA by Season with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean ERA")


ggplot(sum_stats_pitch_in_range, aes(x = season, y = mean_avg)) +
  geom_errorbar(aes(ymin = lower_avg, ymax = upper_avg), width = 0.2) +
  labs(title = "Average AAVG by Season with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean AAVG")


ggplot(sum_stats_pitch_in_range, aes(x = season, y = mean_whip)) +
  geom_errorbar(aes(ymin = lower_whip, ymax = upper_whip), width = 0.2) +
  labs(title = "Average WHIP by Season with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean WHIP")


### comparing pre and post legalization avg performance ----
pre_legal_def_avg <- pitching_data %>% 
  ungroup() %>% 
  filter(season < 2018) %>% 
  select(era,avg, whip) %>% 
  summarize(
    mean_era = mean(era, na.rm = TRUE),
    mean_agavg = mean(avg, na.rm = TRUE),
    mean_whip = mean(whip, na.rm = TRUE))

post_legal_def_avgs_per_szn <- pitching_data %>% 
  ungroup() %>% 
  mutate(season = as.double(season)) %>% 
  filter(season >= 2017) %>% 
  group_by(season) %>% 
  select(era, whip, avg) %>% 
  summarize(
    mean_era = mean(era, na.rm = TRUE),
    mean_aavg = mean(avg, na.rm = TRUE),
    mean_whip = mean(whip, na.rm = TRUE)
  ) 

