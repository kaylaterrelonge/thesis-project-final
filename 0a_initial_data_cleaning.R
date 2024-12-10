# Data Cleaning ----

# no random processes within script
# cleaning and aggregating data

## load pkgs ----
library(janitor)
library(tidyverse)

## reading and aggregating data ----

hitting_files <- dir("data/raw_data/", pattern = "\\hitting.rds$", full.names = TRUE)
hitting_files

# reading in files via for loop
hitting_files_imported <-list()

# for season information
seasons <- list()

for(i in seq_along(hitting_files)){
  hitting_files_imported<- read_rds(hitting_files[[i]]) 
  year <- 2000 + (i - 1)
  
  hitting_files_imported <- bind_rows(hitting_files_imported)
  
  seasons[[i]] <- hitting_files_imported %>%
    mutate(season = as.character(year))

}

combined_data <- bind_rows(seasons)


# cleaning up first col
data_cleaning <- function(data){
  data <- janitor::clean_names(data)
  data <- data %>% 
  rowwise() %>% 
  mutate(
         rank = str_extract_all(playerplayer, "^[0-9]{1,3}"),
         position = sub("^[^ ]* ", "", playerplayer),
         y = gsub("(?<=[a-z])(?=[A-Z0-9])", " ", position, perl = TRUE),
         z = sub("^[^ ]* ", "", y),
         a =  sub("^[^ ]* ", "", z),
         b = str_extract_all(a, "^[0-9A-Z]{1,2}"),
         c = word((playerplayer)),
         d = gsub("(?<=[0-9])(?=[A-Z])", " ", c, perl = TRUE),
         e = gsub("^\\S+ ", "", d),
         f = gsub("([A-Z])([^A-Z]*)$", "", e),
         g = sub("^(.*?)\\s.*$", "\\1", z),
         playerplayer = str_c(f, g, sep = " "),
         position = b,
         ops = caret_upcaret_down_op_scaret_upcaret_down_ops) %>% 
  select(-c(y,z,a,b,c,d,e,f,g, caret_upcaret_down_op_scaret_upcaret_down_ops))
# fixing repeated cols
  col_names <- colnames(data)
  col_names_final <- str_replace_all(col_names, "(\\w+)\\1", "\\1")
  data <- data %>% 
  rename_with(~ col_names_final)
  
  return(data)
}

final_data <- data_cleaning(combined_data)

# write out final data
write_rds(final_data, file = "data/clean_data/clean_hit.rds")

# pitching data
pitching_files <- dir("data/raw_data/", pattern = "\\pitching.rds$", full.names = TRUE)
pitching_files

# problem because webscrapped an empty table, must be manually input.
rm_file <- "data/raw_data//16_pitching.rds"
pitching_files <- pitching_files[pitching_files != rm_file]

# reading in files via for loop
pitching_files_imported <-list()

# for season information
seasons <- list()

for(i in seq_along(pitching_files)){
  pitching_files_imported<- read_rds(pitching_files[[i]]) 
  year <- 2000 + (i - 1)
  

  pitching_files_imported <- bind_rows(pitching_files_imported)
  
  
  seasons[[i]] <- pitching_files_imported %>%
    mutate(season = as.character(year))
  
}

# correcting season error
pitching_int <- seasons %>% 
  bind_rows() %>%  
  filter(season >= '2016') %>% 
  mutate(season = as.numeric(season) + 1)

pitching_pre16 <- seasons %>% 
  bind_rows() %>% 
  filter(season < '2016')

# import 2016 data
s16_dat <-  read_rds("data/raw_data/16_pitching.rds") 


season_16_full <- bind_rows(s16_dat[1:3])%>% 
  mutate(season = '2016')


mini_combine <- full_join(pitching_pre16, season_16_full) %>% 
  mutate(season = as.numeric(season))

combined_data_p <- full_join(mini_combine, pitching_int)

# pitching data cleaning function
# cleaning up first col
data_cleaning_p <- function(data){
  data <- janitor::clean_names(data)
  data <- data %>% 
    rowwise() %>% 
    mutate(
      rank = str_extract_all(playerplayer, "^[0-9]{1,3}"),
      position = sub("^[^ ]* ", "", playerplayer),
      y = gsub("(?<=[a-z])(?=[A-Z0-9])", " ", position, perl = TRUE),
      z = sub("^[^ ]* ", "", y),
      a =  sub("^[^ ]* ", "", z),
      b = str_extract_all(a, "^[0-9A-Z]{1,2}"),
      c = word((playerplayer)),
      d = gsub("(?<=[0-9])(?=[A-Z])", " ", c, perl = TRUE),
      e = gsub("^\\S+ ", "", d),
      f = gsub("([A-Z])([^A-Z]*)$", "", e),
      g = sub("^(.*?)\\s.*$", "\\1", z),
      playerplayer = str_c(f, g, sep = " "),
      position = b,
      era = caret_upcaret_down_er_acaret_upcaret_down_era) %>% 
    select(-c(y,z,a,b,c,d,e,f,g, caret_upcaret_down_er_acaret_upcaret_down_era))
  # fixing repeated cols
  col_names <- colnames(data)
  col_names_final <- str_replace_all(col_names, "(\\w+)\\1", "\\1")
  data <- data %>% 
    rename_with(~ col_names_final)
  
  return(data)
}

final_data_p <- data_cleaning_p(combined_data_p)

# write out final cleaned data
write_rds(final_data_p, file = "data/clean_data/clean_pitch.rds")
