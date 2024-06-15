library(tidyverse)
library(marquee)
library(here)

#' Manually download players dataset from Kaggle
#' https://www.kaggle.com/datasets/damirdizdarevic/uefa-euro-2024-players/

players <- read_csv(here("data", "euro2024_players.csv"))
skimr::skim(players)
colnames(players)

# Squad updates (2024-06-15)

## Germany: Emre Can replaces Aleksandar Pavlovic
replaced_players <- c("Aleksandar Pavlovic")
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
    MarketValue = NA,
    Country = "Germany"
  )
)

players_updated <- players |> 
  filter(!Name %in% replaced_players) |> 
  bind_rows(added_players)


# Unique list of clubs
players_updated |> 
  distinct(Club) |> 
  write_csv(here("data", "clubs.csv"))
