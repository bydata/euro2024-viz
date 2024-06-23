library(tidyverse)
library(here)

#' Manually download players dataset from Kaggle
#' https://www.kaggle.com/datasets/damirdizdarevic/uefa-euro-2024-players/

players <- read_csv(here("data", "euro2024_players.csv"))
skimr::skim(players)
colnames(players)

# Squad updates (2024-06-15)

## Germany: Emre Can replaces Aleksandar Pavlovic
replaced_players <- c("Aleksandar Pavlovic")
removed_players <- c("Jakub Kaluzinski")
added_players <- list(
  data.frame(
    Name = "Emre Can",
    Position = "Defensive Midfield",
    Age = 30,
    Club = "Borussia Dortmund",
    Height = NA,
    Foot = NA,
    Caps = NA,
    Goals = NA,
    MarketValue = 10e6,
    Country = "Germany"
  )
)

players_updated <- players |> 
  filter(!Name %in% c(replaced_players, removed_players)) |> 
  bind_rows(added_players) |> 
  mutate(
    Country = ifelse(Country == "Turkiye", "Türkiye", Country),
    family_name = str_squish(str_extract(Name, "\\s.+$")),
    family_name = ifelse(is.na(family_name), Name, family_name)
  )
  
write_rds(players_updated, here("data", "players-updated.rds"))

# Unique list of clubs
players_updated |> 
  distinct(Club) |> 
  write_csv(here("data", "clubs.csv"))
