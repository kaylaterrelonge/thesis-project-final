# Data Cleaning Function

### load-pkgs ----
library(tidyverse)

### function ----
data_cleaning <- function(data){
  # start by cleaning the names 
  data <- janitor::clean_names(data)
  data <- data %>%
    # ensures all operations are applied row by row
    rowwise() %>% 
    mutate(
      numeric_var = str_extract_all(untidy_variable, "^[0-9]{1,3}"),
      single_letter_var = sub("^[^ ]* ", "", untidy_variable),
      # separating all irrevelant information - tailor patterns to your data
      y = gsub("(?<=[a-z])(?=[A-Z0-9])", " ", single_letter_var, perl = TRUE),
      z = sub("^[^ ]* ", "", y),
      a =  sub("^[^ ]* ", "", z),
      b = str_extract_all(a, "^[0-9A-Z]{1,2}"),
      c = word((repeated_var_name)),
      d = gsub("(?<=[0-9])(?=[A-Z])", " ", c, perl = TRUE),
      e = gsub("^\\S+ ", "", d),
      f = gsub("([A-Z])([^A-Z]*)$", "", e),
      g = sub("^(.*?)\\s.*$", "\\1", z),
      repeated_var_name = str_c(f, g, sep = " "),
      single_letter_var = b) %>% 
    select(-c(y,z,a,b,c,d,e,f,g))
  # pulling column names
  col_names <- colnames(data)
  # removing repeated names
  col_names_final <- str_replace_all(col_names, "(\\w+)\\1", "\\1")
  # overwriting col names
  data <- data %>% 
    rename_with(~ col_names_final)
  return(data)
}