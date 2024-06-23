library(tidyverse)
library(rvest)
library(here)

# Load club names data
clubs <- read_csv(here("data", "clubs.csv"))

tm_build_query_url <- function(club_name) {
  tm_base_url <- "https://www.transfermarkt.com/schnellsuche/ergebnis/schnellsuche?query="
  club_name_for_query <- htmltools::urlEncodePath(club_name)
  club_name_for_query <- gsub("%20", "+", club_name_for_query)
  query_url <- paste0(tm_base_url, club_name_for_query)
  query_url
}

tm_query_club <- function(club_name, sleep = 1) {
  club_name <- ifelse(club_name == "Brighton & Hove Albion", "Brighton", club_name)
  query_url <- tm_build_query_url(club_name)
  page <- read_html(query_url)
  
  # Extract all tables and find the result table for clubs
  # (important in case the search query returns other result types, e.g. players)
  table_selector <- "div table.items"
  table_nodes <- html_nodes(page, table_selector)
  node_texts <- map_chr(table_nodes, html_text)
  which_table_node_id <- which(str_detect(node_texts, "ClubCountrySquad"))
  
  raw_table <- table_nodes[[which_table_node_id]] |>
    html_table()
  fmt_table <- raw_table[1, c(3,4, 7)]
  colnames(fmt_table) <- c("club_name", "club_league", "market_value")

  # In case the club name returned in the search results is different from the
  # one provided
  fmt_table$original_club_name <- club_name

  # Get the country of the league
  country <- html_nodes(table_nodes[[which_table_node_id]], "tbody td img")[2] |>
    html_attr("title")
  fmt_table$league_country <- country

  # Reorder columns
  fmt_table <- fmt_table[c(4, 1, 2, 5, 3)]

  # Add some wait time after the query
  Sys.sleep(sleep)

  fmt_table
}

tm_query_club_possibly <- possibly(tm_query_club, otherwise = NULL)

clubs_info <- map(clubs$Club, tm_query_club_possibly)
clubs_info <- set_names(clubs_info, clubs$Club)
write_rds(clubs_info, here("data", "clubs-info.rds"))


market_value_to_numeric <- function(x) {
  numeric_part <- as.numeric(str_extract(x, "\\d+(\\.\\d+)"))
  multiplier_part <- str_extract(str_remove(x, "€"), "[kbnm]+$")
  multiplier_value <- c("k" = 1e3, "m" = 1e6, "bn" = 1e9)[multiplier_part]
  numeric_part * multiplier_value
}


clubs_info_prep <- clubs_info |> 
  compact() |> 
  map(function(x) mutate(x, market_value = as.character(market_value))) |>
  bind_rows(.id = "club_name2") |> 
  mutate(market_value = market_value_to_numeric(market_value))


# Add AS Monaco to Ligue 1 / France
clubs_info_prep <- clubs_info_prep |> 
  mutate(league_country = ifelse(club_name2 == "AS Monaco", "France", league_country))

write_rds(clubs_info_prep, here("data", "clubs-info-prep.rds"))  
