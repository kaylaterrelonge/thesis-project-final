# Final Figures & Tables ----

# no random processes in the script
# uses official mlb branding logo colors for figures

## load-pkgs ----
library(tidyverse)
library(patchwork)
library(tseries)
library(kableExtra)
library(ggrepel)

## read in data ----
hitting_data <- read_rds(file = 'data/clean_data/clean_hit.rds') %>% 
  # using ungroup to fix a structural era
  ungroup()

pitching_data <- read_rds(file = 'data/clean_data/clean_pitch.rds') %>% 
  # using ungroup to fix a structural era
  ungroup()

### offensive figures ----
sum_stats <- hitting_data %>% 
  group_by(season) %>% 
  summarise(mean_rbi = mean(rbi, na.rm = TRUE),
            lower_rbi = quantile(rbi, 0.025, na.rm = TRUE),
            upper_rbi = quantile(rbi, 0.975, na.rm = TRUE),
            mean_avg = mean(avg, na.rm = TRUE),
            lower_avg = quantile(avg, 0.025, na.rm = TRUE),
            upper_avg = quantile(avg, 0.975, na.rm = TRUE),
            mean_ops = mean(ops, na.rm = TRUE),
            lower_ops = quantile(ops, 0.025, na.rm = TRUE),
            upper_ops = quantile(ops, 0.975, na.rm = TRUE))

ggplot(sum_stats, aes(x = as.numeric(season), y = mean_avg)) +
  geom_point(size = 3, color = "#002D72") +
  geom_errorbar(aes(ymin = lower_avg, ymax = upper_avg), width = 0.2) +
  labs(title = "Mean Batting Average by Season with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean Batting Average") +
  geom_rect(
    mapping = aes(xmin = 2018, xmax = Inf, ymin = -Inf, ymax = Inf),
    alpha = 0.01
  ) +
  theme_minimal()

ggsave("report_images/intro_avg_plt.png")

ggplot(sum_stats, aes(x = as.numeric(season), y = mean_rbi)) +
  geom_point(size = 3, color = "#002D72")  +
  geom_errorbar(aes(ymin = lower_rbi, ymax = upper_rbi), width = 0.2) +
  labs(title = "Average Runs Batted In by Season with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean Runs Batted In") +
  geom_rect(
    mapping = aes(xmin = 2018, xmax = Inf, ymin = -Inf, ymax = Inf),
    alpha = 0.01
  ) +
  theme_minimal()

ggsave("report_images/intro_rbi_plt.png")

ggplot(sum_stats, aes(x = as.numeric(season), y = mean_ops)) +
  geom_point(size = 3, color = "#002D72") +
  geom_errorbar(aes(ymin = lower_ops, ymax = upper_ops), width = 0.2) +
  labs(title = "Average On Base Plus Slugging (OPS) In by Season with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean On Base Plus Slugging") +
  geom_rect(
    mapping = aes(xmin = 2018, xmax = Inf, ymin = -Inf, ymax = Inf),
    alpha = 0.01
  ) +
  theme_minimal()

ggsave("report_images/intro_ops_plt.png")

### defensive figures ----
sum_stats_pitch <- pitching_data %>%  
  group_by(season) %>% 
  summarise(mean_era = mean(era, na.rm = TRUE),
            lower_era = quantile(era, 0.025, na.rm = TRUE),
            upper_era = quantile(era, 0.975, na.rm = TRUE),
            mean_avg = mean(avg, na.rm = TRUE),
            lower_avg = quantile(avg, 0.025, na.rm = TRUE),
            upper_avg = quantile(avg, 0.975, na.rm = TRUE),
            mean_whip = mean(whip, na.rm = TRUE),
            lower_whip = quantile(whip, 0.025, na.rm = TRUE),
            upper_whip = quantile(whip, 0.975, na.rm = TRUE))

ggplot(sum_stats_pitch, aes(x = season, y = mean_era)) +
  geom_point(color = "#D50032", size = 3) +
  geom_errorbar(aes(ymin = lower_era, ymax = upper_era), width = 0.2) +
  labs(title = "Average Earned Run Average (ERA) by \nSeason with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean ERA") +
  geom_rect(
    mapping = aes(xmin = 2018, xmax = Inf, ymin = -Inf, ymax = Inf),
    alpha = 0.01
  ) +
  theme_minimal()

ggsave("report_images/intro_era_plt.png")

ggplot(sum_stats_pitch, aes(x = season, y = mean_avg)) +
  geom_point(color = "#D50032", size = 3) +
  geom_errorbar(aes(ymin = lower_avg, ymax = upper_avg), width = 0.2) +
  labs(title = "Mean Average Against Pitcher (AAVG) by \nSeason with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean Average Against Pitcher") +
  geom_rect(
    mapping = aes(xmin = 2018, xmax = Inf, ymin = -Inf, ymax = Inf),
    alpha = 0.01
  ) +
  theme_minimal()

ggsave("report_images/intro_aavg_plt.png")

ggplot(sum_stats_pitch, aes(x = season, y = mean_whip)) +
  geom_point(color = "#D50032", size = 3) +
  geom_errorbar(aes(ymin = lower_whip, ymax = upper_whip), width = 0.2) +
  labs(title = "Average Walks and Hits Per Innings Pitched (WHIP) by \nSeason with 97.5% and 2.5% Error Bars", x = "Season", y = "Mean WHIP") +
  geom_rect(
    mapping = aes(xmin = 2018, xmax = Inf, ymin = -Inf, ymax = Inf),
    alpha = 0.01
  ) +
  theme_minimal()

ggsave("report_images/intro_whip_plt.png")

### comparison plots ----
player_totals <- hitting_data %>% 
  # sports betting legalized in after 2018 
  filter(season >= '2017') %>% 
  group_by(player) %>% 
  summarise(count = n()) %>%  
  # subsetting for players who played at least 3 seasons post-legalization
  filter(count >= 3)

player_in_range <- left_join(player_totals, hitting_data) %>% 
  filter(season >= 2017)

player_years_active <- hitting_data %>% 
  group_by(player) %>% 
  summarise(years_active = n())

player_in_range_tot <- full_join(player_in_range, player_years_active) %>% 
  filter(!is.na(season))


labels <- player_in_range_tot %>% 
  group_by(season) %>% 
  select(avg, season, years_active) %>%
  slice_max(avg, n = 1, with_ties = FALSE) %>%
  select(season, avg, years_active) %>%
  rename(max_value = avg)

ggplot(player_in_range_tot, mapping = aes(x = season, y = avg)) +
  geom_boxplot(na.rm = TRUE) +
  geom_text_repel(data = labels, 
                  aes(y = max_value, label = years_active),
                  color = "black",
                  min.segment.length = 0,
                  seed = 9876,
                  hjust = 1,
                  box.padding = 0.6,
                  vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Batting Average Distribution From 2017 to 2023",
       subtitle = "Labels Represent the Number of Active Playing Years for the Player Who Recieved the Highest Average",
       x = "Season",
       y = "Batting Average") + 
  theme_minimal()

ggsave("report_images/dist_avg_plt.png")

labels_1 <- player_in_range_tot %>% 
  group_by(season) %>% 
  select(rbi, season, years_active) %>%
  slice_max(rbi, n = 1, with_ties = FALSE) %>%
  select(season, rbi, years_active) %>%
  rename(max_value = rbi)

ggplot(player_in_range_tot, mapping = aes(x = season, y = rbi)) +
  geom_boxplot(na.rm = TRUE) +
  geom_text_repel(data = labels_1, 
                  aes(y = max_value, label = years_active),
                  color = "black",
                  min.segment.length = 0,
                  seed = 9876,
                  hjust = 1,
                  box.padding = 0.6,
                  vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Runs Batted In Distribution From 2017 to 2023",
       subtitle = "Labels Represent the Number of Active Playing Years for the Player Who Recieved the Highest Average",
       x = "Season",
       y = "Runs Batted In") + 
  theme_minimal()

ggsave("report_images/dist_rbi_plt.png")

labels_2 <- player_in_range_tot %>% 
  group_by(season) %>% 
  select(ops, season, years_active) %>%
  slice_max(ops, n = 1, with_ties = FALSE) %>%
  select(season, ops, years_active) %>%
  rename(max_value = ops)

ggplot(player_in_range_tot, mapping = aes(x = season, y = ops)) +
  geom_boxplot(na.rm = TRUE) +
  geom_text_repel(data = labels_2, 
                  aes(y = max_value, label = years_active),
                  color = "black",
                  min.segment.length = 0,
                  seed = 9876,
                  hjust = 1,
                  box.padding = 0.6,
                  vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "On Base Plus Slugging Distribution From 2017 to 2023",
       subtitle = "Labels Represent the Number of Active Playing Years for the Player Who Recieved the Highest Average",
       x = "Season",
       y = "On Base Plus Slugging") + 
  theme_minimal()

ggsave("report_images/dist_ops_plt.png")

player_totals_pitch <- pitching_data %>% 
  # sports betting legalized in after 2018 
  filter(season >= 2017) %>% 
  group_by(player) %>% 
  summarise(count = n()) %>% 
  filter(count >= 3)

player_in_range_pitch <- left_join(player_totals_pitch, pitching_data) %>% 
  filter(season >= 2017)

player_years_active_pitch <- pitching_data %>% 
  group_by(player) %>% 
  summarise(years_active = n())

player_in_range_pitch_tot <- full_join(player_in_range_pitch, player_years_active_pitch) %>% 
  filter(!is.na(season))

labels_3 <- player_in_range_pitch_tot %>% 
  group_by(season) %>% 
  select(era, season, years_active) %>%
  slice_max(era, n = 1, with_ties = FALSE) %>%
select(season, era, years_active) %>%
  rename(max_value = era)

ggplot(player_in_range_pitch_tot, mapping = aes(x = factor(season), y = era)) +
  geom_boxplot(na.rm = TRUE) +
  geom_text_repel(data = labels_3, 
                  aes(y = max_value, label = years_active),
                  color = "black",
                  min.segment.length = 0,
                  seed = 9876,
                  hjust = 1,
                  box.padding = 0.6,
                  vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Earned Run Average Distribution From 2017 to 2023",
       subtitle = "Labels Represent the Number of Active Playing Years for the Player Who Recieved the Highest Average",
       x = "Season",
       y = "Earned Run Average") + 
  theme_minimal()

ggsave("report_images/dist_era_plt.png")

labels_4 <- player_in_range_pitch_tot %>% 
  group_by(season) %>% 
  select(avg, season, years_active) %>%
  slice_max(avg, n = 1, with_ties = FALSE) %>%
  select(season, avg, years_active) %>%
  rename(max_value = avg)

ggplot(player_in_range_pitch_tot, mapping = aes(x = factor(season), y = avg)) +
  geom_boxplot(na.rm = TRUE) +
  geom_text_repel(data = labels_4, 
                  aes(y = max_value, label = years_active),
                  color = "black",
                  min.segment.length = 0,
                  seed = 9876,
                  hjust = 1,
                  box.padding = 0.6,
                  vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Average Against Pitcher Distribution From 2017 to 2023",
       subtitle = "Labels Represent the Number of Active Playing Years for the Player Who Recieved the Highest Average",
       x = "Season",
       y = "Average Against Pitcher") + 
  theme_minimal()

ggsave("report_images/dist_aavg_plt.png")

labels_5 <- player_in_range_pitch_tot %>% 
  group_by(season) %>% 
  select(whip, season, years_active) %>%
  slice_max(whip, n = 1, with_ties = FALSE) %>%
  select(season, whip, years_active) %>%
  rename(max_value = whip)

ggplot(player_in_range_pitch_tot, mapping = aes(x = factor(season), y = whip)) +
  geom_boxplot(na.rm = TRUE) +
  geom_text_repel(data = labels_5, 
                  aes(y = max_value, label = years_active),
                  color = "black",
                  min.segment.length = 0,
                  seed = 9876,
                  hjust = 1,
                  box.padding = 0.6,
                  vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Walks And Hits Per Inning Pitched Distribution From 2017 to 2023",
       subtitle = "Labels Represent the Number of Active Playing Years for the Player Who Recieved the Highest Average",
       x = "Season",
       y = "Walks And Hits Per Inning Pitched") + 
  theme_minimal()

ggsave("report_images/dist_whip_plt.png")

# data wrangling to get some information for time specific plots
post_legal_off_avgs_per_szn <- hitting_data %>% 
  ungroup() %>% 
  mutate(season = as.double(season)) %>% 
  filter(season >= 2017) %>% 
  group_by(season) %>% 
  select(avg, rbi, ops) %>% 
  summarize(
    mean_rbi = mean(rbi, na.rm = TRUE),
    mean_avg = mean(avg, na.rm = TRUE),
    mean_ops = mean(ops, na.rm = TRUE)
  )

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

total_combined <- full_join(post_legal_def_avgs_per_szn, post_legal_off_avgs_per_szn)

labs_info  <- total_combined %>% 
  filter(season == 2020)

lab_info_fin <- tibble(
  "stat_type" = c("offensive", "defensive"),
  "avg" = c(labs_info$mean_avg, labs_info$mean_aavg),
  "season" = c("2020", "2020")
)

ggplot(total_combined, mapping = aes(x = season)) +
  geom_line(aes(y = mean_avg, 
                color = "#002D72")) +
  geom_line(aes(y = mean_aavg, 
                color = "#D50032"))+
  scale_color_manual(name = NULL,
                     values = c("#002D72", "#D50032"), 
                     labels = c("Offensive Average", "Defensive Average")) +
  theme_minimal() +
  theme(
    legend.justification = c(1, 1),
    legend.position = c(1, 1),
    legend.direction = "vertical"
  ) +
  labs(title = "Mean Offensive v. Defensive Average From 2017 to 2023",
       x = "Season",
       y = "Average") +
  guides(color = guide_legend(
    override.aes = list(
      color = c("#002D72", "#D50032"))))

ggsave("report_images/comp_avg_plt.png")

plot_a <- ggplot(total_combined, mapping = aes(x = season)) +
  geom_line(aes(y = mean_ops), color = "#002D72") +
  theme_minimal() +
  labs(title = "Mean OPS From 2017 to 2023",
       x = "Season",
       y = "Mean OPS") 

plot_b <- ggplot(total_combined, mapping = aes(x = season)) +
  geom_line(aes(y = mean_whip), color = "#D50032") +
  theme_minimal() +
  labs(title = "Mean WHIP From 2017 to 2023",
       x = "Season",
       y = "Mean WHIP") 

# must assign plots created with patchwork to a variable in order to save!
p1 <- plot_a/plot_b
ggsave("report_images/comp_base_plt.png", p1)

plot_c <- ggplot(total_combined, mapping = aes(x = season)) +
  geom_line(aes(y = mean_rbi), color = "#002D72") +
  theme_minimal() +
  labs(title = "Mean RBI From 2017 to 2023",
       x = "Season",
       y = "Mean RBI") 

plot_d <- ggplot(total_combined, mapping = aes(x = season)) +
  geom_line(aes(y = mean_era), color = "#D50032") +
  theme_minimal() +
  labs(title = "Mean ERA From 2017 to 2023",
       x = "Season",
       y = "Mean ERA") 

p2 <- plot_c/plot_d
ggsave("report_images/comp_run_plt.png", p2)


### tables ----

#### statistical testing ----
team_t_dat <- hitting_data %>% 
  group_by(team) %>% 
  select(rbi, team, season, avg, player, ops) %>% 
  mutate("legalized" = ifelse(season >= 2018, "yes", "no"))



t_test_results_avg <- team_t_dat %>%
  filter(n_distinct(legalized) == 2) %>% 
  group_by(team) %>%
  group_modify(~{
    test_result <- t.test(avg ~ legalized, data = .x)
    tibble(
      t_value = test_result$statistic,
      df = test_result$parameter,
      p_value = test_result$p.value,
      mean_legal = mean(.x$avg[.x$legalized == "yes"], na.rm = TRUE),
      mean_not_legal = mean(.x$avg[.x$legalized == "no"], na.rm = TRUE)
    )
  })%>%
  rename(Team = team,
         "T Value" = t_value,
         "Degrees of Freedom" = df,
         "P Value" = p_value,
         "Mean AVG (Pre-Legalized)" = mean_legal,
         "Mean AVG (Legalized)" = mean_not_legal) %>% 
  kbl(caption = "Welch Two Sample T-Test Results for Mean Batting Average Pre and Post-Legalization",
      centering = TRUE) %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable("report_images/t_test_avg.pdf")

t_test_results_rbi <- team_t_dat %>%
  filter(n_distinct(legalized) == 2) %>% 
  group_by(team) %>%
  group_modify(~{
    test_result <- t.test(rbi ~ legalized, data = .x)
    tibble(
      t_value = test_result$statistic,
      df = test_result$parameter,
      p_value = test_result$p.value,
      mean_legal = mean(.x$rbi[.x$legalized == "yes"], na.rm = TRUE),
      mean_not_legal = mean(.x$rbi[.x$legalized == "no"], na.rm = TRUE)
    )
  })%>%
  rename(Team = team,
         "T Value" = t_value,
         "Degrees of Freedom" = df,
         "P Value" = p_value,
         "Mean RBI (Pre-Legalized)" = mean_legal,
         "Mean RBI (Legalized)" = mean_not_legal) %>% 
  kbl(caption = "Welch Two Sample T-Test Results for Mean Runs Batted In Pre and Post-Legalization",
      centering = TRUE) %>% 
  kable_classic(full_width = F, html_font = "Cambria")%>% 
  save_kable("report_images/t_test_rbi.pdf") 

t_test_results_ops <- team_t_dat %>%
  filter(n_distinct(legalized) == 2) %>% 
  group_by(team) %>%
  group_modify(~{
    test_result <- t.test(ops ~ legalized, data = .x)
    tibble(
      t_value = test_result$statistic,
      df = test_result$parameter,
      p_value = test_result$p.value,
      mean_legal = mean(.x$ops[.x$legalized == "yes"], na.rm = TRUE),
      mean_not_legal = mean(.x$ops[.x$legalized == "no"], na.rm = TRUE)
    )
  })%>%
  rename(Team = team,
         "T Value" = t_value,
         "Degrees of Freedom" = df,
         "P Value" = p_value,
         "Mean OPS (Pre-Legalized)" = mean_legal,
         "Mean OPS (Legalized)" = mean_not_legal) %>% 
  kbl(caption = "Welch Two Sample T-Test Results for Mean OPS Pre and Post-Legalization",
      centering = TRUE) %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable("report_images/t_test_ops.pdf")


team_t_dat_p <- pitching_data %>% 
  group_by(team) %>% 
  select(era, team, season, avg, player, whip) %>% 
  mutate("legalized" = ifelse(season >= 2018, "yes", "no"))

t_test_results_era <- team_t_dat_p %>%
  filter(n_distinct(legalized) == 2) %>% 
  group_by(team) %>%
  group_modify(~{
    test_result <- t.test(era ~ legalized, data = .x)
    tibble(
      t_value = test_result$statistic,
      df = test_result$parameter,
      p_value = test_result$p.value,
      mean_legal = mean(.x$era[.x$legalized == "yes"], na.rm = TRUE),
      mean_not_legal = mean(.x$era[.x$legalized == "no"], na.rm = TRUE)
    )
  })%>%
  rename(Team = team,
         "T Value" = t_value,
         "Degrees of Freedom" = df,
         "P Value" = p_value,
         "Mean ERA (Pre-Legalized)" = mean_legal,
         "Mean ERA (Legalized)" = mean_not_legal) %>% 
  kbl(caption = "Welch Two Sample T-Test Results for Mean Earned Run Average Pre and Post-Legalization",
      centering = TRUE) %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable("report_images/t_test_era.pdf")

t_test_results_avg <- team_t_dat_p %>%
  filter(n_distinct(legalized) == 2) %>% 
  group_by(team) %>%
  group_modify(~{
    test_result <- t.test(avg ~ legalized, data = .x)
    tibble(
      t_value = test_result$statistic,
      df = test_result$parameter,
      p_value = test_result$p.value,
      mean_legal = mean(.x$avg[.x$legalized == "yes"], na.rm = TRUE),
      mean_not_legal = mean(.x$avg[.x$legalized == "no"], na.rm = TRUE)
    )
  })%>%
  rename(Team = team,
         "T Value" = t_value,
         "Degrees of Freedom" = df,
         "P Value" = p_value,
         "Mean AAVG (Pre-Legalized)" = mean_legal,
         "Mean AAVG (Legalized)" = mean_not_legal) %>% 
  kbl(caption = "Welch Two Sample T-Test Results for Mean Average Against Pitcher Pre and Post-Legalization",
      centering = TRUE) %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable("report_images/t_test_aavg.pdf")

t_test_results_whip <- team_t_dat_p %>%
  filter(n_distinct(legalized) == 2) %>% 
  group_by(team) %>%
  group_modify(~{
    test_result <- t.test(whip ~ legalized, data = .x)
    tibble(
      t_value = test_result$statistic,
      df = test_result$parameter,
      p_value = test_result$p.value,
      mean_legal = mean(.x$whip[.x$legalized == "yes"], na.rm = TRUE),
      mean_not_legal = mean(.x$whip[.x$legalized == "no"], na.rm = TRUE)
    )
  })%>%
  rename(Team = team,
         "T Value" = t_value,
         "Degrees of Freedom" = df,
         "P Value" = p_value,
         "Mean WHIP (Pre-Legalized)" = mean_legal,
         "Mean WHIP (Legalized)" = mean_not_legal) %>% 
  kbl(caption = "Welch Two Sample T-Test Results for Mean Walks and Hits Per Innings Pitched Pre and Post-Legalization",
      centering = TRUE) %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable("report_images/t_test_whip.pdf")

# dickey fuller test
offensive_vars <- c("avg", "rbi", "ops")

all_adf_off <- list()

# streamlining the testing process via a for loop
off_adf <- for (i in offensive_vars){
  adf_test <- adf.test(hitting_data[[i]])
  
  all_adf_off[[i]] <- tibble(
    "Metric" = i,
    "ADF Stat" = adf_test$statistic,
    "P Value" = adf_test$p.value,
    "Stationary" = ifelse(adf_test$p.value < 0.05, "TRUE", "FALSE")
  )
}

all_adf_off <- bind_rows(all_adf_off)

defensive_vars <- c("avg", "whip", "era")

all_adf_def <- list()

off_def <- for (i in defensive_vars){
  adf_test <- adf.test(pitching_data[[i]])
  
  all_adf_def[[i]] <- tibble(
    "Metric" = i,
    "ADF Stat" = adf_test$statistic,
    "P Value" = adf_test$p.value,
    "Stationary" = ifelse(adf_test$p.value < 0.05, "TRUE", "FALSE")
  )
}

all_adf_def <- bind_rows(all_adf_def)%>%
  mutate(Metric = ifelse(Metric == "avg", "aavg", Metric))

total_adf <- rbind(all_adf_def, all_adf_off) %>% 
  kbl(caption = "Augmented Dickey Fuller Offensive & Defensive Test Results") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable("report_images/adf.pdf")

#### metric performance ----
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

pre_legal_def_avg <- pitching_data %>% 
  ungroup() %>% 
  filter(season < 2018) %>% 
  select(era,avg, whip) %>% 
  summarize(
    mean_era = mean(era, na.rm = TRUE),
    mean_agavg = mean(avg, na.rm = TRUE),
    mean_whip = mean(whip, na.rm = TRUE))

post_legal_def_avg <- pitching_data %>% 
  ungroup() %>% 
  filter(season >= 2018) %>% 
  select(era,avg, whip) %>% 
  summarize(
    mean_era = mean(era, na.rm = TRUE),
    mean_agavg = mean(avg, na.rm = TRUE),
    mean_whip = mean(whip, na.rm = TRUE))

pre_legal_avg_tot <- cbind(pre_legal_def_avg, pre_legal_off_avgs) 
post_legal_avg_tot <- cbind(post_legal_def_avg, post_legal_off_avgs) 

avg_perf_by_legality <- rbind(pre_legal_avg_tot, post_legal_avg_tot)
avg_perf_by_legality <- avg_perf_by_legality %>% 
  mutate("Legal Betting?" = c(FALSE, TRUE),
         "Mean RBI" = mean_rbi, 
         "Mean AVG" = mean_avg,
         "Mean OPS" = mean_ops,
         "Mean ERA" = mean_era,
         "Mean AVG
         Against Pitcher" = mean_agavg,
         "Mean WHIP" = mean_whip) %>% 
  select(-c(mean_era, mean_rbi, mean_avg, mean_ops, mean_agavg, mean_whip))


avg_perf_by_legality %>%  
  kbl(caption = "Mean Performance Changes Pre and Post Legalization") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable("report_images/perf_trends.pdf")


