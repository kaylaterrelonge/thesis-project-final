# Data Collection ----

# No random processes in this script

## load-pkgs ----
library(rvest)
library(httr)
library(tidyverse)
library(RSelenium)
library(purrr)
library(xml2)

# for loop for data collection per season

#### 2000-szn ----
# creating an empty list to store scrapped data

# batting stats
season_0_batting <- list()

for (i in 1:7) {
  url <- paste0("https://www.mlb.com/stats/2000?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_0_batting[[i]] <- table
}
write_rds(season_0_batting, file = "data/raw_data/00_hitting.rds")

# pitching stats
season_0_pitching <- list()

for (i in 1:4) {
  url <- paste0("https://www.mlb.com/stats/pitching/2000?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_0_pitching[[i]] <- table
}

write_rds(season_0_pitching, file = "data/raw_data/00_pitching.rds")

#### 2001-szn ----
# creating an empty list to store scrapped data

# batting stats
season_01_batting <- list()

for (i in 1:7) {
  url <- paste0("https://www.mlb.com/stats/2001?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_01_batting[[i]] <- table
}
write_rds(season_01_batting, file = "data/raw_data/01_hitting.rds")

# pitching stats
season_01_pitching <- list()

for (i in 1:4) {
  url <- paste0("https://www.mlb.com/stats/pitching/2001?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_01_pitching[[i]] <- table
}

write_rds(season_01_pitching, file = "data/raw_data/01_pitching.rds")

#### 2002-szn ----
# creating an empty list to store scrapped data

# batting stats
season_02_batting <- list()

for (i in 1:7) {
  url <- paste0("https://www.mlb.com/stats/2002?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_02_batting[[i]] <- table
}
write_rds(season_02_batting, file = "data/raw_data/02_hitting.rds")

# pitching stats
season_02_pitching <- list()

for (i in 1:4) {
  url <- paste0("https://www.mlb.com/stats/pitching/2002?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_02_pitching[[i]] <- table
}

write_rds(season_02_pitching, file = "data/raw_data/02_pitching.rds")

#### 2003-szn ----
# creating an empty list to store scrapped data

# batting stats
season_03_batting <- list()

for (i in 1:7) {
  url <- paste0("https://www.mlb.com/stats/2003?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_03_batting[[i]] <- table
}
write_rds(season_03_batting, file = "data/raw_data/03_hitting.rds")

# pitching stats
season_03_pitching <- list()

for (i in 1:4) {
  url <- paste0("https://www.mlb.com/stats/pitching/2003?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_03_pitching[[i]] <- table
}

write_rds(season_03_pitching, file = "data/raw_data/03_pitching.rds")

#### 2004-szn ----
# creating an empty list to store scrapped data

# batting stats
season_04_batting <- list()

for (i in 1:7) {
  url <- paste0("https://www.mlb.com/stats/2004?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_04_batting[[i]] <- table
}
write_rds(season_04_batting, file = "data/raw_data/04_hitting.rds")

# pitching stats
season_04_pitching <- list()

for (i in 1:4) {
  url <- paste0("https://www.mlb.com/stats/pitching/2004?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_04_pitching[[i]] <- table
}

write_rds(season_04_pitching, file = "data/raw_data/04_pitching.rds")

#### 2005-szn ----
# creating an empty list to store scrapped data

# batting stats
season_05_batting <- list()

for (i in 1:6) {
  url <- paste0("https://www.mlb.com/stats/2005?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_05_batting[[i]] <- table
}
write_rds(season_05_batting, file = "data/raw_data/05_hitting.rds")

# pitching stats
season_05_pitching <- list()

for (i in 1:4) {
  url <- paste0("https://www.mlb.com/stats/pitching/2005?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_05_pitching[[i]] <- table
}

write_rds(season_05_pitching, file = "data/raw_data/05_pitching.rds")


#### 2006-szn ----
# creating an empty list to store scrapped data

# batting stats
season_06_batting <- list()

for (i in 1:7) {
  url <- paste0("https://www.mlb.com/stats/2006?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_06_batting[[i]] <- table
}
write_rds(season_06_batting, file = "data/raw_data/06_hitting.rds")

# pitching stats
season_06_pitching <- list()

for (i in 1:4) {
  url <- paste0("https://www.mlb.com/stats/pitching/2006?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_06_pitching[[i]] <- table
}

write_rds(season_06_pitching, file = "data/raw_data/06_pitching.rds")


#### 2007-szn ----
# creating an empty list to store scrapped data

# batting stats
season_07_batting <- list()

for (i in 1:7) {
  url <- paste0("https://www.mlb.com/stats/2007?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_07_batting[[i]] <- table
}
write_rds(season_07_batting, file = "data/raw_data/07_hitting.rds")

# pitching stats
season_07_pitching <- list()

for (i in 1:4) {
  url <- paste0("https://www.mlb.com/stats/pitching/2007?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_07_pitching[[i]] <- table
}

write_rds(season_07_pitching, file = "data/raw_data/07_pitching.rds")


#### 2008-szn ----
# creating an empty list to store scrapped data

# batting stats
season_08_batting <- list()

for (i in 1:6) {
  url <- paste0("https://www.mlb.com/stats/2008?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_08_batting[[i]] <- table
}
write_rds(season_08_batting, file = "data/raw_data/08_hitting.rds")

# pitching stats
season_08_pitching <- list()

for (i in 1:4) {
  url <- paste0("https://www.mlb.com/stats/pitching/2008?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_08_pitching[[i]] <- table
}

write_rds(season_08_pitching, file = "data/raw_data/08_pitching.rds")


#### 2009-szn ----
# creating an empty list to store scrapped data

# batting stats
season_09_batting <- list()

for (i in 1:7) {
  url <- paste0("https://www.mlb.com/stats/2009?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_09_batting[[i]] <- table
}
write_rds(season_02_batting, file = "data/raw_data/09_hitting.rds")

# pitching stats
season_09_pitching <- list()

for (i in 1:4) {
  url <- paste0("https://www.mlb.com/stats/pitching/2009?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_09_pitching[[i]] <- table
}

write_rds(season_02_pitching, file = "data/raw_data/09_pitching.rds")


#### 2010-szn ----
# creating an empty list to store scrapped data

# batting stats
season_10_batting <- list()

for (i in 1:7) {
  url <- paste0("https://www.mlb.com/stats/2010?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_10_batting[[i]] <- table
}
write_rds(season_10_batting, file = "data/raw_data/10_hitting.rds")

# pitching stats
season_10_pitching <- list()

for (i in 1:4) {
  url <- paste0("https://www.mlb.com/stats/pitching/2010?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_10_pitching[[i]] <- table
}

write_rds(season_10_pitching, file = "data/raw_data/10_pitching.rds")


#### 2011-szn ----
# creating an empty list to store scrapped data

# batting stats
season_11_batting <- list()

for (i in 1:6) {
  url <- paste0("https://www.mlb.com/stats/2011?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_11_batting[[i]] <- table
}
write_rds(season_11_batting, file = "data/raw_data/11_hitting.rds")

# pitching stats
season_11_pitching <- list()

for (i in 1:4) {
  url <- paste0("https://www.mlb.com/stats/pitching/2011?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_11_pitching[[i]] <- table
}

write_rds(season_11_pitching, file = "data/raw_data/11_pitching.rds")

#### 2012-szn ----
# creating an empty list to store scrapped data

# batting stats
season_12_batting <- list()

for (i in 1:6) {
  url <- paste0("https://www.mlb.com/stats/2012?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_12_batting[[i]] <- table
}
write_rds(season_12_batting, file = "data/raw_data/12_hitting.rds")

# pitching stats
season_12_pitching <- list()

for (i in 1:4) {
  url <- paste0("https://www.mlb.com/stats/pitching/2012?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_12_pitching[[i]] <- table
}

write_rds(season_12_pitching, file = "data/raw_data/12_pitching.rds")

#### 2013-szn ----
# creating an empty list to store scrapped data

# batting stats
season_13_batting <- list()

for (i in 1:6) {
  url <- paste0("https://www.mlb.com/stats/2013?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_13_batting[[i]] <- table
}
write_rds(season_13_batting, file = "data/raw_data/13_hitting.rds")

# pitching stats
season_13_pitching <- list()

for (i in 1:4) {
  url <- paste0("https://www.mlb.com/stats/pitching/2013?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_13_pitching[[i]] <- table
}

write_rds(season_13_pitching, file = "data/raw_data/13_pitching.rds")

#### 2014-szn ----
# creating an empty list to store scrapped data

# batting stats
season_14_batting <- list()

for (i in 1:6) {
  url <- paste0("https://www.mlb.com/stats/2014?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_14_batting[[i]] <- table
}
write_rds(season_14_batting, file = "data/raw_data/14_hitting.rds")

# pitching stats
season_14_pitching <- list()

for (i in 1:4) {
  url <- paste0("https://www.mlb.com/stats/pitching/2014?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_14_pitching[[i]] <- table
}

write_rds(season_14_pitching, file = "data/raw_data/14_pitching.rds")

#### 2015-szn ----
# creating an empty list to store scrapped data

# batting stats
season_15_batting <- list()

for (i in 1:6) {
  url <- paste0("https://www.mlb.com/stats/2015?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_15_batting[[i]] <- table
}
write_rds(season_15_batting, file = "data/raw_data/15_hitting.rds")

# pitching stats
season_15_pitching <- list()

for (i in 1:4) {
  url <- paste0("https://www.mlb.com/stats/pitching/2015?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_15_pitching[[i]] <- table
}

write_rds(season_15_pitching, file = "data/raw_data/15_pitching.rds")

#### 2016-szn ----
# creating an empty list to store scrapped data

# batting stats
season_16_batting <- list()

for (i in 1:6) {
  url <- paste0("https://www.mlb.com/stats/2016?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_16_batting[[i]] <- table
}
write_rds(season_05_batting, file = "data/raw_data/16_hitting.rds")

# pitching stats
season_16_pitching <- list()

for (i in 1:4) {
  url <- paste0("https://www.mlb.com/stats/pitching/2016?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_16_pitching[[i]] <- table
}

write_rds(season_16_pitching, file = "data/raw_data/16_pitching.rds")


#### 2017-szn ----
# creating an empty list to store scrapped data

# batting stats
season_17_batting <- list()

for (i in 1:6) {
  url <- paste0("https://www.mlb.com/stats/2017?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_17_batting[[i]] <- table
}
write_rds(season_17_batting, file = "data/raw_data/17_hitting.rds")

# pitching stats
season_17_pitching <- list()

for (i in 1:3) {
  url <- paste0("https://www.mlb.com/stats/pitching/2017?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_17_pitching[[i]] <- table
}

write_rds(season_17_pitching, file = "data/raw_data/17_pitching.rds")

#### 2018-szn ----
# creating an empty list to store scrapped data

# batting stats
season_18_batting <- list()

for (i in 1:6) {
  url <- paste0("https://www.mlb.com/stats/2018?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_18_batting[[i]] <- table
}
write_rds(season_18_batting, file = "data/raw_data/18_hitting.rds")

# pitching stats
season_18_pitching <- list()

for (i in 1:3) {
  url <- paste0("https://www.mlb.com/stats/pitching/2018?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_18_pitching[[i]] <- table
}

write_rds(season_18_pitching, file = "data/raw_data/18_pitching.rds")


#### 2019-szn ----
# creating an empty list to store scrapped data

# batting stats
season_19_batting <- list()

for (i in 1:6) {
  url <- paste0("https://www.mlb.com/stats/2019?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_19_batting[[i]] <- table
}
write_rds(season_19_batting, file = "data/raw_data/19_hitting.rds")

# pitching stats
season_19_pitching <- list()

for (i in 1:3) {
  url <- paste0("https://www.mlb.com/stats/pitching/2019?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_19_pitching[[i]] <- table
}

write_rds(season_19_pitching, file = "data/raw_data/19_pitching.rds")


#### 2020-szn ----
# creating an empty list to store scrapped data

# batting stats
season_20_batting <- list()

for (i in 1:6) {
  url <- paste0("https://www.mlb.com/stats/2020?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_20_batting[[i]] <- table
}
write_rds(season_20_batting, file = "data/raw_data/20_hitting.rds")

# pitching stats
season_20_pitching <- list()

for (i in 1:2) {
  url <- paste0("https://www.mlb.com/stats/pitching/2020?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_20_pitching[[i]] <- table
}

write_rds(season_20_pitching, file = "data/raw_data/20_pitching.rds")


#### 2021-szn ----
# creating an empty list to store scrapped data

# batting stats
season_21_batting <- list()

for (i in 1:6) {
  url <- paste0("https://www.mlb.com/stats/2021?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_21_batting[[i]] <- table
}
write_rds(season_21_batting, file = "data/raw_data/21_hitting.rds")

# pitching stats
season_21_pitching <- list()

for (i in 1:2) {
  url <- paste0("https://www.mlb.com/stats/pitching/2021?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_21_pitching[[i]] <- table
}

write_rds(season_21_pitching, file = "data/raw_data/21_pitching.rds")

#### 2022-szn ----
# creating an empty list to store scrapped data

# batting stats
season_22_batting <- list()

for (i in 1:6) {
  url <- paste0("https://www.mlb.com/stats/2022?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_22_batting[[i]] <- table
}
write_rds(season_22_batting, file = "data/raw_data/22_hitting.rds")

# pitching stats
season_22_pitching <- list()

for (i in 1:2) {
  url <- paste0("https://www.mlb.com/stats/pitching/2022?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_22_pitching[[i]] <- table
}

write_rds(season_22_pitching, file = "data/raw_data/22_pitching.rds")

#### 2023-szn ----

# creating an empty list to store scrapped data

# batting stats
season_23_batting <- list()

for (i in 1:6) {
  url <- paste0("https://www.mlb.com/stats/2023?page=",i)
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_23_batting[[i]] <- table
}
write_rds(season_23_batting, file = "data/raw_data/23_hitting.rds")

# pitching stats
season_23_pitching <- list()

for (i in 1:2) {
  url <- paste0("https://www.mlb.com/stats/pitching/2023?page=",i,"&sortState=asc")
  
  response <- GET(url)
  webpage <- read_html(content(response, as = "text"))
  tables <- webpage %>%
    html_nodes("table")
  table <- tables[[1]] %>%
    html_table()
  season_23_pitching[[i]] <- table
}

write_rds(season_23_pitching, file = "data/raw_data/23_pitching.rds")
