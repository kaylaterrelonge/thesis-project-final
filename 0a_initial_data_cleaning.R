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

# reading in files via for loop
pitching_files_imported <-list()

# for season information
seasons <- list()

for(i in seq_along(pitching_files)){
  pitching_files_imported<- read_rds(pitching_files[[i]]) 
  year <- 2000 + (i - 1)
  
  pitching_files_imported <- pitching_files_imported %>%
    select(-where(is.logical))
  
  pitching_files_imported <- bind_rows(pitching_files_imported)
  
  
  seasons[[i]] <- pitching_files_imported %>%
    mutate(season = as.character(year))
  
}

combined_data_p <- bind_rows(seasons)

