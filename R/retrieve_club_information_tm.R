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
  query_url <- tm_build_query_url(club_name)
  page <- read_html(query_url)
  table_selector <- "div#yw0 > table"
  raw_table <- html_node(page, table_selector) |> 
    html_table()
  fmt_table <- raw_table[1, c(3,4, 7)]
  colnames(fmt_table) <- c("club_name", "club_league", "market_value")
  
  # In case the club name returned in the search results is different from the 
  # one provided
  fmt_table$original_club_name <- club_name
  
  # Get the country of the league
  country <- html_nodes(page, "tbody td img")[2] |> 
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

clubs_info |> 
  compact() |> 
  map(function(x) mutate(x, market_value = as.character(market_value))) |>
  bind_rows() |> 
  filter(club_name != original_club_name) |> 
  View()

tm_build_query_url("Újpest FC")
tm_query_club("Újpest FC")
tm_query_club("Borussia Dortmund")
