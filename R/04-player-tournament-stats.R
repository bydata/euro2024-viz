library(tidyverse)
library(jsonlite)
library(stringdist)
library(fuzzyjoin)

#' Source: UEFA
#' Direct URL: https://www.uefa.com/euro2024/statistics/players/goals/?order=desc&sortBy=goals_scored_head

# url <- "https://compstats.uefa.com/v1/player-ranking?competitionId=3&limit=200&offset=0&optionalFields=PLAYER%2CTEAM&order=DESC&phase=TOURNAMENT&seasonYear=2024&stats=goals%2Cgoals_scored_with_right%2Cgoals_scored_with_left%2Cgoals_scored_head%2Cgoals_scored_other%2Cgoals_scored_inside_penalty_area%2Cgoals_scored_outside_penalty_area%2Cpenalty_scored%2Cmatches_appearance"
base_url <- "https://compstats.uefa.com/v1/player-ranking?competitionId=3&optionalFields=PLAYER%2CTEAM&order=DESC&phase=TOURNAMENT&seasonYear=2024&stats="

stats <- c("goals", "goals_scored_with_right", "goals_scored_with_left", 
"goals_scored_head", "goals_scored_other", "goals_scored_inside_penalty_area", 
"goals_scored_outside_penalty_area", "penalty_scored", "matches_appearance", 
"distance_covered", "minutes_played_official", 
"goals", "assists", "top_speed", "attempts", "attempts_on_target", 
"attempts_off_target", "attempts_blocked", "passes_accuracy", "passes_attempted", 
"passes_completed", "cross_accuracy", "cross_attempted", "cross_completed",
"free_kick", "assists", "corners", "offsides", "dribbling", "recovered_ball", 
"tackles", "clearance_attempted", "saves", "goals_conceded", "saves_on_penalty", 
"clean_sheet", "punches", "fouls_committed", "fouls_suffered", "yellow_cards", 
"red_cards")

stats <- unique(stats)


build_query_urls <- function(stat_name, chunk_size = 500, n_players = 630) {
  stopifnot(stat_name %in% stats)
  
  num_chunks <- ceiling(n_players / chunk_size)
  offsets <- seq(from = chunk_size, by = chunk_size, length.out = num_chunks) - chunk_size
  
  query_urls <- paste0(
    base_url, 
    stat_name,
    "&limit=", chunk_size,
    paste0("&offset=", offsets)
  )
  query_urls
}

build_query_urls("matches_appearance")



run_query <- function(query_urls) {
  res <- map(query_urls, jsonlite::read_json)
  res
}

foo <- run_query(build_query_urls("matches_appearance"))


# Extract relevant information from the json and create a dataframe
stat_json_to_df <- function(json_obj, player_bio = FALSE) {
 
  # Discard rows where the statistics object is empty
  has_statistics <- json_obj |>
    map("statistics") |>
    map_int(length) > 0
  
  print(sum(has_statistics))
  df <- NULL
  if (sum(has_statistics > 0)) {
    if (player_bio) {
      df_players <- json_obj[has_statistics] |> 
        map("player") |> 
        map(function(json_obj) discard(json_obj, names(json_obj) == "translations")) |> 
        bind_rows()
      
      player_countries <- json_obj[has_statistics] |> 
        map("player") |> 
        map_chr(pluck, "translations", "countryName", "EN")
      
      df_players$countryName <- player_countries
      
    } else {
      player_ids <- json_obj[has_statistics] |> 
        map_chr(pluck, "playerId")
      df_players <- data.frame(id = player_ids)
    }
    
    stat_values <- json_obj[has_statistics] |> 
      map("statistics") |>
      modify_depth(2, function(x) discard(x, names(x) == "translations")) |>
      map_depth(2, function(x) discard(x, names(x) == "unit")) |> 
      map_chr(pluck, 1, "value")
    
    stat_name <- json_obj[has_statistics] |> 
      map("statistics") |>
      modify_depth(2, function(x) discard(x, names(x) == "translations")) |>
      map_depth(2, function(x) discard(x, names(x) == "unit")) |> 
      map_chr(pluck, 1, "name")
    stat_name <- stat_name[[1]]
    
    df_stats <- data.frame(name = stat_name, value = stat_values)

    df <- bind_cols(df_players, df_stats)
  }
  df
}


# dfs <- map_dfr(foo, stat_json_to_df)
# stat_json_to_df(foo[[1]])


df_all_stats <- map_dfr(stats, function(x) {
  res <- run_query(build_query_urls(x))
  dfs <- map_dfr(res, stat_json_to_df)
  Sys.sleep(2)
  dfs
})


# get player bio data for all players - including those who didn't play
player_bio_from_json <- function(json_obj) {

  df_players <- json_obj |> 
    map("player") |> 
    map(function(json_obj) discard(json_obj, names(json_obj) == "translations")) |> 
    bind_rows()
  
  player_countries <- json_obj |> 
    map("player") |> 
    map_chr(pluck, "translations", "countryName", "EN")
  
  df_players$countryName <- player_countries
  df_players
}


df_players <- {
  foo <- run_query(build_query_urls("matches_appearance"))
  map_dfr(foo, player_bio_from_json)
}


colnames(df_players)


## Resolve club ids
#' Possible via call to this URL: 
#' Multiple club ids can be passed separated by "%2C"
#' This is FC Barcelona
#' https://comp.uefa.com/v2/teams?teamIds=50080

length(unique(df_players$clubId))

clubid_base_url <- "https://comp.uefa.com/v2/teams?teamIds="
paste0(clubid_base_url, paste(unique(df_players$clubId), collapse = "%2C"))
# TODO: Split into multiple requests, 219 ids exceeds a limit


retrieve_club_names <- function(club_ids, chunk_size = 100) {
  clubid_base_url <- "https://comp.uefa.com/v2/teams?teamIds="
  num_clubs <- length(club_ids)
  num_chunks <- ceiling(num_clubs / chunk_size)
  start_idx <- seq(from = 0, by = chunk_size, length.out = num_chunks) + 1
  end_idx <- seq(from = chunk_size, by = chunk_size, length.out = num_chunks)
  end_idx[length(end_idx)] <- num_clubs 
  
  query_urls <- map2(
    start_idx, end_idx,
    function(x, y) paste0(clubid_base_url, paste(club_ids[x:y], collapse = "%2C"))
  )
  res <- map(query_urls, read_json)
  res
}

club_ids <- unique(df_players$clubId)[!is.na(unique(df_players$clubId))]
clubs_json <- retrieve_club_names(club_ids)


clubs_info <- data.frame(
  id = clubs_json |> 
    map_depth(2, "id") |> 
    unlist() |> 
    unname(),
  club_name = clubs_json |> 
    map_depth(2, pluck, "translations", "displayOfficialName", "EN") |> 
    unlist() |> 
    unname(),
  club_name_short = clubs_json |> 
    map_depth(2, "internationalName") |> 
    unlist() |> 
    unname(),
  club_countryCode = clubs_json |> 
    map_depth(2, "countryCode")  |> 
    unlist() |> 
    unname()
)


# Join stats and bio and reshape dataset

df_player_stats <- df_players |> 
  left_join(clubs_info, by = join_by(clubId == id)) |> 
  left_join(df_all_stats, by = "id") |> 
  mutate(value = as.numeric(value)) |> 
  pivot_wider(id_cols = age:club_countryCode) |> 
  select(id, internationalName, countryName, countryCode, everything())


write_rds(df_player_stats, file.path("data", "euro2024-player-tournament-stats.rds"))
write_csv(df_player_stats, file.path("data", "euro2024-player-tournament-stats.csv"))


