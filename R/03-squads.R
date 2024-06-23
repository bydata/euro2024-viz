library(tidyverse)
library(here)
library(countrycode)

# Read prepared datasets
# Run 01-update-squads.R and 02-retrieve-club-information-tm.R to produce the files
players_updated <- read_rds(here("data", "players-updated.rds"))
clubs_info_prep <- read_rds(here("data", "clubs-info-prep.rds"))

# Check if the match works as expected 
players_updated |> 
  anti_join(clubs_info_prep, by = join_by("Club" == "club_name2"))

players_updated <- players_updated |> 
  inner_join(clubs_info_prep, by = join_by("Club" == "club_name2")) |> 
  select(Name:Country, family_name, club_league:league_country)

# Check league name duplicates between countries (e.g. Bundesliga in DE and AT)
players_updated |> 
  distinct(club_league, league_country) |> 
  count(club_league, name = "n_countries", sort = TRUE)


players_updated |> 
  distinct(club_league, league_country) |> View()

players_updated <- players_updated |> 
  mutate(club_league = case_when(
    club_league == "Ligue 1" & league_country == "Monaco" ~ "Ligue 1 (Monaco)",
    club_league == "Bundesliga" & league_country == "Austria" ~ "Bundesliga (AT)",
    club_league == "Bundesliga" & league_country == "Germany" ~ "1. Bundesliga",
    club_league == "Premier Liga" & league_country == "Ukraine" ~ "Premier Liga (UA)",
    club_league == "Premier Liga" & league_country == "Russia" ~ "Premier Liga (RU)",
    TRUE ~ club_league
  ))

# Prepare data for Sankey chart in Flourish

## Player country to league country
country_2_league_country <- players_updated |> 
  count(from = Country, to = paste(league_country, ""), name = "n_players", sort = TRUE)

## League country to club league
league_country_2_club_league <- players_updated |> 
  count(from = paste(league_country, ""), to = club_league, name = "n_players", sort = TRUE)

## Combine the connection data frames
connections <- bind_rows(country_2_league_country, league_country_2_club_league) |> 
  arrange(from, to)


# Replace country names with flag emojis ---------------------------------------

# Function to convert ISO 3166-1 alpha-2 code to flag emoji
iso_to_flag <- function(iso_code) {
  # Convert each character to its corresponding regional indicator symbol letter
  intToUtf8(utf8ToInt("🇦") - utf8ToInt("A") + utf8ToInt(toupper(substr(iso_code, 1, 1)))) %>%
    paste0(intToUtf8(utf8ToInt("🇦") - utf8ToInt("A") + utf8ToInt(toupper(substr(iso_code, 2, 2)))))
}

# Function to map country names to flag emojis
country_to_flag <- function(country_name) {
  # Get the ISO 3166-1 alpha-2 code
  iso_code <- countrycode(country_name, "country.name", "iso2c")
  # Convert to flag emoji
  flag <- iso_to_flag(iso_code)
  return(flag)
}

# Vector of country names
country_names <- unique(players_updated$Country)

# Convert country names to flag emojis
flag_emojis <- sapply(country_names, country_to_flag)
# flag_emojis["England"] <- "\\U1F3F4\\UE0067\\UE0062\\UE0065\\UE006E\\UE0067\\UE007F"
flag_emojis[c("England", "Scotland")] <- c("ENG", "SCO")

# ------------------------------------------------------------------------------

connections <- connections |> 
  mutate(
    from_flag = flag_emojis[from],
    # from = ifelse(is.na(from_flag), from, from_flag)
    from = paste(from, ifelse(is.na(from_flag), "", from_flag))
    )

clipr::write_clip(connections)


# Alternative preparation: 
#' Only split league countries into leagues if there is more than one league,
#' e.g. Germany has 1. Bundesliga and 2. Bundesliga -> league split,

## League country to club league
league_country_2_club_league <- players_updated |> 
  count(from = paste(league_country, ""), to = club_league, name = "n_players", sort = TRUE)

countries_with_multiple_leagues <- league_country_2_club_league |> 
  count(from) |> 
  filter(n > 1) |> 
  pull(from)

league_country_2_club_league_filtered <- league_country_2_club_league |> 
  filter(from %in% countries_with_multiple_leagues)

## Combine the connection data frames
connections <- bind_rows(country_2_league_country, league_country_2_club_league_filtered) |> 
  arrange(from, to) 

connections <- connections |> 
  mutate(
    from_flag = flag_emojis[from],
    from = paste(from, ifelse(is.na(from_flag), "", from_flag))
  )
clipr::write_clip(connections)


## Country lists
unique(players_updated$Country)
unique(players_updated$league_country)
setdiff(unique(players_updated$league_country), unique(players_updated$Country))


## Number of players per league country
players_updated |> 
  count(league_country, sort = TRUE) |> 
  clipr::write_clip()

## Number of players playing in their country
players_updated |> 
  mutate(plays_in_home_country = Country == league_country) |> 
  group_by(Country) |> 
  summarize(n_players_in_home_country = sum(plays_in_home_country)) |> 
  arrange(-n_players_in_home_country) |> 
  clipr::write_clip()

# Market value by team and player
players_updated |>
  select(Country, family_name, Name, MarketValue) |>
  clipr::write_clip()

# Only England, France + Hungary, Georgia, Slovakia, Slovenia, Albania, Romania
players_updated |>
  filter(Country %in% c("England", "France", "Hungary", "Georgia", "Slovakia", 
                        "Slovenia", "Albania", "Romania")) |> 
  select(Country, family_name, Name, MarketValue) |>
  clipr::write_clip()



players_updated |> 
  count(Club, sort = TRUE) |> 
  head(10)

players_updated |> 
  count(Club, club_league, league_country, sort = TRUE) |> 
  clipr::write_clip()

players_updated |> 
  count(league_country, club_league, sort = TRUE) |> 
  clipr::write_clip()


players_updated |> 
  count(league_country, club_league, Club, sort = TRUE) |> 
  clipr::write_clip()


players_updated |> 
  filter(!league_country %in% unique(players_updated$Country)) |> 
  count(league_country, club_league, Club, sort = TRUE) |> 
  clipr::write_clip()


players_updated |> 
  select(league_country, club_league, Club, Name, Country) |> 
  clipr::write_clip()


# Clubs sending the most players

players_updated |> 
  count(Club, sort = TRUE) |> 
  slice_max(order_by = n, n = 10) |> 
  clipr::write_clip()


# Number of countries players from national teams play in

players_updated |> 
  count(Country, league_country, name = "n_players") |> 
  arrange(Country, -n_players) |> 
  clipr::write_clip()

players_updated |> 
  group_by(Country) |> 
  summarize(n_league_countries = n_distinct(league_country)) |> 
  arrange(Country, -n_league_countries) |> 
  clipr::write_clip()


# Top 20 players by market value

players_updated |> 
  select(Name, Country, MarketValue) |> 
  slice_max(order_by = MarketValue, n = 20) |> 
  mutate(
    name_with_flag = paste(flag_emojis[Country], Name),
    MarketValue = scales::number(MarketValue, scale = 1e-6, suffix = "M")) |> 
  select(-name_with_flag) |> 
  clipr::write_clip()


# Player explorer: full records

## Position mapping
unique(players_updated$Position) |> clipr::write_clip()



position_mapping <- tribble(
  ~Position, ~position_detail, ~position_group,
"Goalkeeper", "Goalkeeper", "Goalkeeper",
"Centre-Back", "Centre-Back", "Defense",
"Left-Back", "Left-Back", "Defense",
"Right-Back", "Right-Back", "Defense",
"Defensive Midfield", "Defensive Midfield", "Midfield",
"Central Midfield", "Central Midfield", "Midfield",
"Attacking Midfield", "Attacking Midfield", "Midfield",
"Left Winger", "Left Winger", "Attack",
"Right Winger", "Right Winger", "Attack",
"Second Striker", "Striker", "Attack",
"Centre-Forward", "Striker", "Attack",
"Right Midfield", "Right Midfield", "Midfield",
"Left Midfield", "Left Midfield", "Midfield"
)


players_updated |> 
  inner_join(position_mapping, by = join_by(Position)) |> 
  select(Name, Country, position_group, position_detail, Age:MarketValue, 
         club_league, league_country) |> 
  clipr::write_clip()

