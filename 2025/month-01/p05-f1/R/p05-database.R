# F1 race data from 1950 to 2024 ----
rm(list=ls())
# browseURL("https://www.kaggle.com/datasets/rohanrao/formula-1-world-championship-1950-2020")

# Loading the data sources ----
path <- "2025/month-01/data/kaggle/files/"

# CSV directory
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# names to the files
file_names <- tools::file_path_sans_ext(basename(files))
file_names <- make.names(file_names)  

# Criar variáveis dinamicamente no Environment
for (i in seq_along(files)) {
  assign(file_names[i], read.csv(files[i]), envir = .GlobalEnv)
}

# show the files
print(ls(pattern = paste(file_names, collapse = "|")))

## transforming dataframes
library(tidyverse)

df_races <- races %>%
  select(raceId, year, round)

df_results <- results %>%
  select(raceId, driverId, constructorId, grid, positionOrder,
         points, milliseconds, fastestLapTime, fastestLapSpeed)

df_drivers <- drivers %>%
  mutate(name = paste0(forename, " ", surname)) %>%
  select(driverId, name, nationality, url)

df_driverstandings <- driver_standings %>%
  select(raceId, driverId, position)

df_constructors <- constructors %>%
  select(constructorId, name, nationality, url)

## total drivers and winners
g1_data <- df_drivers %>%
  left_join(df_driverstandings, by = "driverId") %>%
  left_join(df_races, by = "raceId") %>%
  select(year, driverId, round, position) %>%
  group_by(year) %>%
  mutate(lastgp = max(round)) %>%
  ungroup() %>%
  mutate(champion = ifelse(lastgp == round & position == 1, 1, 0)) %>%
  replace_na(list(champion = 0)) %>%
  group_by(driverId) %>%
  summarise(champion = max(champion)) %>%  
  ungroup() %>%
  mutate(type = ifelse(champion == 0, "pilot", "champion"))

## total teams and winners
g2_data <- df_constructors %>%
  left_join(constructor_standings, by = "constructorId") %>%
  left_join(df_races, by = "raceId") %>%
  select(year, constructorId, round, position) %>%
  group_by(year) %>%
  mutate(lastgp = max(round)) %>%
  ungroup() %>%
  mutate(champion = ifelse(lastgp == round & position == 1, 1, 0)) %>%
  replace_na(list(champion = 0)) %>%
  group_by(constructorId) %>%
  summarise(champion = max(champion)) %>%  
  ungroup() %>%
  mutate(type = ifelse(champion == 0, "team", "champion"))


## top 5 champions
g3_data <- df_drivers %>%
  left_join(df_driverstandings, by = "driverId") %>%
  left_join(df_races, by = "raceId") %>%
  select(year, driverId, name, url, round, position) %>%
  group_by(year) %>%
  mutate(lastgp = max(round)) %>%
  ungroup() %>%
  mutate(champion = ifelse(lastgp == round & position == 1, 1, 0)) %>%
  filter(champion == 1) %>%  
  group_by(driverId, name, url) %>%
  summarise(first_title_year = min(year), total_titles = n(), .groups = "drop") %>%
  arrange(first_title_year) %>%
  arrange(desc(total_titles)) %>%
  slice_head(n = 6) %>%
  mutate(rank = row_number())

# View the top 6 champions
print(g3_data)

