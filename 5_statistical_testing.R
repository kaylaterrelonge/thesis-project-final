# Statistical Testing ----

# no random processes in the script

## load-pkgs ----
library(tidyverse)
library(skimr)
library(kableExtra)
library(tseries)

## read in data ----
hitting_data <- read_rds(file = 'data/clean_data/clean_hit.rds') %>% 
  # using ungroup to fix a structural era
  ungroup()

pitching_data <- read_rds(file = 'data/clean_data/clean_pitch.rds') %>% 
  # using ungroup to fix a structural era
  ungroup()

### two sample t-test ----

# doing a t-test by team
team_t_dat <- hitting_data %>% 
  group_by(team) %>% 
  select(rbi, team, season, avg, player, ops) %>% 
  mutate("legalized" = ifelse(season >= 2018, "yes", "no"))

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
  kable_classic(full_width = F, html_font = "Cambria") 

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
  kable_classic(full_width = F, html_font = "Cambria") 

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
  kable_classic(full_width = F, html_font = "Cambria") 


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
  kable_classic(full_width = F, html_font = "Cambria") 

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
  kable_classic(full_width = F, html_font = "Cambria") 

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
  kable_classic(full_width = F, html_font = "Cambria") 

### dickey-fuller test ----
offensive_vars <- c("avg", "rbi", "ops")

all_adf_off <- list()

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
  kable_classic(full_width = F, html_font = "Cambria") 